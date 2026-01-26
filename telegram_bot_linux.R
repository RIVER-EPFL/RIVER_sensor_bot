#!/usr/bin/env Rscript
# ViewLinc Telegram Bot - Production Version

library(httr)
library(jsonlite)
library(lubridate)
library(grDevices)
library(base64enc)

log_message <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(...), "\n")
  flush.console()
}

# Determine script directory for .Renviron loading
script_dir <- tryCatch({
  # Try to get script directory from command line args
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    dirname(script_path)
  } else {
    # Fallback to current directory
    getwd()
  }
}, error = function(e) getwd())

# Load environment from .Renviron file in script directory
renv_path <- file.path(script_dir, ".Renviron")
if (file.exists(renv_path)) {
  readRenviron(renv_path)
  log_message(".Renviron loaded from:", renv_path)
} else {
  log_message("Warning: .Renviron not found at:", renv_path)
}

# Load configuration from environment variables
token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
chat_ids_raw <- Sys.getenv("TELEGRAM_ALLOWED_CHAT_IDS")
VIEWLINC_TOKEN <- Sys.getenv("VIEWLINC_TOKEN")
VIEWLINC_SERVER <- Sys.getenv("VIEWLINC_SERVER")
BASE_DIR <- Sys.getenv("BOT_BASE_DIR")

# Validate required environment variables
if (!nzchar(BASE_DIR)) {
  stop("BOT_BASE_DIR not set in .Renviron")
}
if (!nzchar(VIEWLINC_SERVER)) {
  stop("VIEWLINC_SERVER not set in .Renviron")
}

# Set up directory structure
DATA_DIR <- file.path(BASE_DIR, "data")
STATUS_FILE <- file.path(DATA_DIR, "latest_status.rds")
ALARM_STATE_FILE <- file.path(DATA_DIR, "last_alarm_state.rds")
MUTE_FILE <- file.path(DATA_DIR, "muted_alarms.rds")

if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE)
  log_message("Created:", DATA_DIR)
}

if (!nzchar(token)) stop("TELEGRAM_BOT_TOKEN not set")
if (!nzchar(chat_ids_raw)) stop("TELEGRAM_ALLOWED_CHAT_IDS not set")
if (!nzchar(VIEWLINC_TOKEN)) stop("VIEWLINC_TOKEN not set")

allowed_chat_ids <- as.numeric(strsplit(chat_ids_raw, ",")[[1]])
base_url <- paste0("https://api.telegram.org/bot", token)

# ViewLinc API functions
PARAM_MAPPING <- list(
  list(pattern = "Depthmm", name = "Depth", order = 1),
  list(pattern = "CDOMppb", name = "CDOM", order = 2),
  list(pattern = "TurbNTU", name = "Turbidity", order = 3),
  list(pattern = "DOuM", name = "Dissolved O2", order = 4),
  list(pattern = "ConduS[Cc]m", name = "Conductivity", order = 5),
  list(pattern = "DOdegC|DOTdegC", name = "Water Temperature", order = 6),
  list(pattern = "CondTdegC", name = "Temperature_Cond", order = 7),
  list(pattern = "BattV", name = "Battery Voltage", order = 8)
)

map_param_name <- function(param_name) {
  for (mapping in PARAM_MAPPING) {
    if (grepl(mapping$pattern, param_name, ignore.case = TRUE)) {
      return(list(name = mapping$name, order = mapping$order))
    }
  }
  return(list(name = param_name, order = 999))
}

get_param_status <- function(param_name, value) {
  if (grepl("BattV", param_name, ignore.case = TRUE)) {
    if (value < 11.5) {
      return(list(severity = 2, status = "Alarm"))
    } else if (value < 12.0) {
      return(list(severity = 1, status = "Warning"))
    }
  }
  return(list(severity = 0, status = "OK"))
}

clean_token <- function(token) {
  decoded <- rawToChar(base64enc::base64decode(token))
  decoded_clean <- gsub("[\r\n]+$", "", decoded)
  base64enc::base64encode(charToRaw(decoded_clean))
}

fetch_viewlinc <- function(endpoint, method = "GET") {
  url <- paste0(VIEWLINC_SERVER, "/rest/v1/", endpoint)
  token_clean <- clean_token(VIEWLINC_TOKEN)
  
  tryCatch({
    resp <- if (method == "POST") {
      POST(url,
           add_headers(Authorization = paste0("Bearer ", token_clean),
                      Accept = "application/vnd.api+json",
                      `Content-Type` = "application/json"),
           httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
           timeout(30))
    } else {
      GET(url,
          add_headers(Authorization = paste0("Bearer ", token_clean),
                     Accept = "application/vnd.api+json"),
          httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE),
          timeout(30))
    }
    
    if (status_code(resp) >= 200 && status_code(resp) < 300) {
      parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
      return(parsed)
    } else {
      log_message("ViewLinc API error:", status_code(resp))
      return(NULL)
    }
  }, error = function(e) {
    log_message("Error fetching from ViewLinc:", e$message)
    return(NULL)
  })
}

refresh_data <- function() {
  tryCatch({
    log_message("Fetching data from ViewLinc API...")
    
    # Fetch locations
    locations_result <- fetch_viewlinc("locations")
    if (is.null(locations_result)) {
      log_message("Failed to fetch locations")
      return(FALSE)
    }
    
    locations <- locations_result$data
    if (is.null(locations) || !is.data.frame(locations) || nrow(locations) == 0) {
      log_message("No locations found")
      return(FALSE)
    }
    
    log_message("Found", nrow(locations), "locations")
    
    # Get leaf locations (sensors)
    if ("attributes.leaf" %in% names(locations)) {
      leaf_ids <- locations$id[locations$attributes.leaf == TRUE]
    } else {
      leaf_ids <- locations$id
    }
    
    if (length(leaf_ids) == 0) {
      log_message("No leaf locations found")
      return(FALSE)
    }
    
    log_message("Found", length(leaf_ids), "sensors")
    
    # Fetch sensor data
    ids_str <- paste(leaf_ids, collapse = ",")
    endpoint <- sprintf("locations_data?location_ids=[%s]", ids_str)
    
    data_result <- fetch_viewlinc(endpoint, method = "POST")
    if (is.null(data_result)) {
      log_message("Failed to fetch sensor data")
      return(FALSE)
    }
    
    sensor_data <- data_result$data
    if (is.null(sensor_data) || !is.data.frame(sensor_data) || nrow(sensor_data) == 0) {
      log_message("No sensor data found")
      return(FALSE)
    }
    
    log_message("Fetched data for", nrow(sensor_data), "sensors")
    
    # Process data
    df <- data.frame(
      station = character(),
      parameter = character(),
      value = numeric(),
      units = character(),
      severity = integer(),
      timestamp = as.POSIXct(character()),
      location_id = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(sensor_data)) {
      row <- sensor_data[i, ]
      
      station <- if ("attributes.zone" %in% names(sensor_data)) {
        zone_parts <- strsplit(as.character(row$attributes.zone), "/")[[1]]
        tail(zone_parts, 1)
      } else {
        "Unknown"
      }
      
      param_raw <- if ("attributes.location_name" %in% names(sensor_data)) {
        as.character(row$attributes.location_name)
      } else {
        "Unknown"
      }
      param_mapped <- map_param_name(param_raw)$name
      
      value <- if ("attributes.value" %in% names(sensor_data)) {
        as.numeric(row$attributes.value)
      } else {
        NA_real_
      }
      
      units <- if ("attributes.display_units" %in% names(sensor_data)) {
        as.character(row$attributes.display_units)
      } else {
        ""
      }
      
      status <- get_param_status(param_raw, value)
      severity <- status$severity
      
      timestamp <- if ("attributes.timestamp" %in% names(sensor_data)) {
        as.POSIXct(as.numeric(row$attributes.timestamp), origin = "1970-01-01", tz = "Europe/Zurich")
      } else {
        Sys.time()
      }
      
      location_id <- if ("id" %in% names(sensor_data)) {
        as.character(row$id)
      } else {
        NA_character_
      }
      
      df <- rbind(df, data.frame(
        station = station,
        parameter = param_mapped,
        value = value,
        units = units,
        severity = severity,
        timestamp = timestamp,
        location_id = location_id,
        stringsAsFactors = FALSE
      ))
    }
    
    saveRDS(df, STATUS_FILE)
    log_message("✓ Saved", nrow(df), "readings to", STATUS_FILE)
    TRUE
  }, error = function(e) {
    log_message("✗ Error:", e$message)
    FALSE
  })
}

# Telegram functions
send_message <- function(chat_id, text) {
  tryCatch({
    resp <- POST(paste0(base_url, "/sendMessage"),
                 body = list(chat_id = chat_id, text = text),
                 encode = "form")
    if (status_code(resp) == 200) {
      log_message("Sent message to", chat_id)
      return(TRUE)
    } else {
      log_message("Failed to send message, status:", status_code(resp))
      return(FALSE)
    }
  }, error = function(e) {
    log_message("Error sending message:", e$message)
    return(FALSE)
  })
}

send_photo <- function(chat_id, photo_path, caption = NULL) {
  tryCatch({
    body_list <- list(chat_id = chat_id, photo = upload_file(photo_path))
    if (!is.null(caption)) body_list$caption <- caption
    
    resp <- POST(
      paste0(base_url, "/sendPhoto"),
      body = body_list,
      encode = "multipart"
    )
    if (status_code(resp) == 200) {
      log_message("Sent photo to", chat_id)
      return(TRUE)
    } else {
      log_message("Failed to send photo, status:", status_code(resp))
      return(FALSE)
    }
  }, error = function(e) {
    log_message("Error sending photo:", e$message)
    return(FALSE)
  })
}

get_updates <- function(offset = NULL) {
  tryCatch({
    url <- paste0(base_url, "/getUpdates")
    if (!is.null(offset)) url <- paste0(url, "?offset=", offset)
    resp <- GET(url, timeout(20))
    if (status_code(resp) != 200) return(data.frame())
    result <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
    if (is.null(result$result) || length(result$result) == 0) return(data.frame())
    result$result
  }, error = function(e) {
    data.frame()
  })
}

read_status <- function() {
  if (!file.exists(STATUS_FILE)) return(NULL)
  tryCatch(readRDS(STATUS_FILE), error = function(e) NULL)
}

# Fetch historical data from ViewLinc API for a specific location and date range
fetch_historical_for_plot <- function(location_id, days) {
  end_time <- Sys.time()
  start_time <- end_time - (days * 24 * 3600)
  
  start_ts <- as.numeric(start_time)
  end_ts <- as.numeric(end_time)
  
  endpoint <- sprintf("locations_history?location_ids=[%s]&date_from=%.0f&date_to=%.0f", 
                     location_id, start_ts, end_ts)
  
  log_message("Fetching", days, "days of history for location ID:", location_id)
  log_message("Date range:", format(start_time, "%Y-%m-%d %H:%M"), "to", format(end_time, "%Y-%m-%d %H:%M"))
  log_message("Endpoint:", endpoint)
  
  result <- fetch_viewlinc(endpoint, method = "POST")
  
  if (is.null(result)) {
    log_message("✗ API returned NULL")
    return(NULL)
  }
  
  # Debug: show structure of result
  log_message("API response structure:", paste(names(result), collapse=", "))
  
  # Check meta for pagination info
  if (!is.null(result$meta)) {
    log_message("Meta info:", paste(names(result$meta), collapse=", "))
    if (!is.null(result$meta$total_count)) {
      log_message("Total count in meta:", result$meta$total_count)
    }
  }
  
  # Check links for pagination
  if (!is.null(result$links)) {
    log_message("Links:", paste(names(result$links), collapse=", "))
  }
  
  if (is.null(result$data)) {
    log_message("✗ No data field in API response")
    return(NULL)
  }
  
  # Check if data is a data frame
  if (is.data.frame(result$data)) {
    log_message("Data frame with", nrow(result$data), "rows and", ncol(result$data), "columns")
    
    # Check if data_points is nested in attributes.data_points
    if ("attributes.data_points" %in% names(result$data)) {
      log_message("Found nested data_points structure")
      data_points <- result$data$attributes.data_points[[1]]
      
      if (!is.null(data_points) && length(data_points) > 0) {
        # Convert to data frame if it's a matrix
        if (is.matrix(data_points)) {
          df <- as.data.frame(data_points)
          colnames(df) <- c("timestamp", "value", "valid")
        } else if (is.data.frame(data_points)) {
          df <- data_points
          if (ncol(df) == 3 && !all(c("timestamp", "value", "valid") %in% names(df))) {
            colnames(df) <- c("timestamp", "value", "valid")
          }
        } else {
          log_message("✗ Unexpected data_points format")
          return(NULL)
        }
        
        log_message("✓ Extracted", nrow(df), "data points from nested structure")
        return(df)
      } else {
        log_message("✗ Empty data_points")
        return(NULL)
      }
    }
    
    log_message("Column names:", paste(head(names(result$data), 10), collapse=", "))
    if (nrow(result$data) == 0) {
      log_message("✗ Empty data frame")
      return(NULL)
    }
    log_message("✓ Fetched", nrow(result$data), "history points")
    return(result$data)
  } else if (is.list(result$data)) {
    log_message("Data is a list with", length(result$data), "elements")
    # Try to convert list to data frame
    if (length(result$data) > 0) {
      tryCatch({
        df <- do.call(rbind, lapply(result$data, as.data.frame))
        log_message("✓ Converted list to data frame:", nrow(df), "rows")
        return(df)
      }, error = function(e) {
        log_message("✗ Could not convert list to data frame:", e$message)
        return(NULL)
      })
    }
  }
  
  log_message("✗ Unexpected data format")
  return(NULL)
}

format_status <- function(data) {
  if (is.null(data) || nrow(data) == 0) return("No data")
  lines <- character()
  for (st in unique(data$station)) {
    rows <- data[data$station == st, ]
    lines <- c(lines, paste0("📍 ", st))
    for (i in 1:nrow(rows)) {
      emoji <- if (rows$severity[i] == 2) "🔴" else if (rows$severity[i] == 1) "🟡" else "🟢"
      lines <- c(lines, sprintf("  %s %s: %.2f %s", emoji, rows$parameter[i], rows$value[i], rows$units[i]))
    }
    # Add timestamp for this station
    ts_latest <- rows$timestamp[which.max(rows$timestamp)]
    ts_formatted <- format(ts_latest, "%Y-%m-%d %H:%M", tz = "Europe/Zurich")
    lines <- c(lines, paste0("  🕐 ", ts_formatted), "")
  }
  paste(lines, collapse = "\n")
}

# Mute management functions with time-based expiration
load_mutes <- function() {
  if (!file.exists(MUTE_FILE)) return(list())
  tryCatch(readRDS(MUTE_FILE), error = function(e) list())
}

save_mutes <- function(mutes) {
  tryCatch(saveRDS(mutes, MUTE_FILE), error = function(e) {
    log_message("Failed to save mutes:", e$message)
  })
}

add_mute <- function(station, parameter, days = NULL) {
  mute_key <- paste(tolower(station), tolower(parameter), sep = "|")
  mutes <- load_mutes()
  
  # Calculate expiration time if days specified
  expires_at <- if (!is.null(days)) {
    Sys.time() + (days * 24 * 3600)
  } else {
    NULL  # Permanent mute
  }
  
  # Store as list with expiration time
  mutes[[mute_key]] <- list(expires_at = expires_at)
  save_mutes(mutes)
  
  return(list(key = mute_key, days = days))
}

clear_mutes <- function() {
  save_mutes(list())
}

is_muted <- function(station, parameter) {
  mute_key <- paste(tolower(station), tolower(parameter), sep = "|")
  mutes <- load_mutes()
  
  if (!(mute_key %in% names(mutes))) return(FALSE)
  
  # Check if mute has expired
  mute_info <- mutes[[mute_key]]
  if (!is.null(mute_info$expires_at)) {
    if (Sys.time() > mute_info$expires_at) {
      # Mute expired, remove it
      mutes[[mute_key]] <- NULL
      save_mutes(mutes)
      return(FALSE)
    }
  }
  
  return(TRUE)
}

clean_expired_mutes <- function() {
  mutes <- load_mutes()
  if (length(mutes) == 0) return()
  
  # Remove expired mutes
  for (key in names(mutes)) {
    mute_info <- mutes[[key]]
    if (!is.null(mute_info$expires_at)) {
      if (Sys.time() > mute_info$expires_at) {
        mutes[[key]] <- NULL
        log_message("Removed expired mute:", key)
      }
    }
  }
  
  save_mutes(mutes)
}

# Generate time-series plot by fetching data from ViewLinc API on-demand
generate_timeseries_plot <- function(station_name, parameter_name, days) {
  # Map acronym to full parameter name
  param_full <- map_param_acronym(parameter_name)
  
  # Get current data to find the location ID for this station/parameter
  current_data <- read_status()
  if (is.null(current_data)) {
    log_message("No current data available")
    return(NULL)
  }
  
  log_message("Looking for:", station_name, "-", param_full)
  
  # Find matching station and parameter
  matched <- current_data[
    tolower(current_data$station) == tolower(station_name) & 
    tolower(current_data$parameter) == tolower(param_full), 
  ]
  
  if (nrow(matched) == 0) {
    log_message("✗ No matching sensor found")
    log_message("Available stations:", paste(unique(current_data$station), collapse=", "))
    return(NULL)
  }
  
  # Get location ID from matched data
  location_id <- matched$location_id[1]
  
  if (is.na(location_id) || !nzchar(location_id)) {
    log_message("✗ Location ID not available in status data")
    return(NULL)
  }
  
  log_message("✓ Found location ID:", location_id, "for", station_name, "-", param_full)
  
  # Fetch historical data from API for this specific location and date range
  history_data <- fetch_historical_for_plot(location_id, days)
  
  if (is.null(history_data) || nrow(history_data) < 2) {
    log_message("Insufficient historical data for plot")
    return(NULL)
  }
  
  # Process the historical data
  # Data comes as data frame with columns: timestamp, value, valid
  times <- as.POSIXct(as.numeric(history_data$timestamp), 
                     origin = "1970-01-01", tz = "Europe/Zurich")
  values <- as.numeric(history_data$value)
  
  # Filter out invalid data points if valid column exists
  if ("valid" %in% names(history_data)) {
    valid_mask <- as.logical(history_data$valid)
    times <- times[valid_mask]
    values <- values[valid_mask]
  }
  
  units <- matched$units[1]
  
  plot_file <- tempfile(fileext = ".png")
  
  tryCatch({
    
    # Calculate statistics
    current_val <- tail(values, 1)
    mean_val <- mean(values, na.rm = TRUE)
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)
    
    # Create plot
    png(plot_file, width = 1200, height = 700, res = 100)
    par(mar = c(5, 4, 4, 2) + 0.1)
    
    plot(times, values, type = "l", col = "#2196F3", lwd = 2,
         xlab = "Time", 
         ylab = paste0(param_full, " (", units, ")"),
         main = paste0(station_name, " - ", param_full, " (Last ", days, " day", 
                      if(days > 1) "s" else "", ")"),
         ylim = c(min_val * 0.95, max_val * 1.05))
    
    # Add points
    points(times, values, pch = 20, col = "#1976D2", cex = 0.5)
    
    # Add grid
    grid(col = "gray", lty = "dotted")
    
    # Add mean line
    abline(h = mean_val, col = "#FF9800", lty = 2, lwd = 1.5)
    
    # Add current value line
    abline(h = current_val, col = "#F44336", lty = 2, lwd = 1.5)
    
    # Add legend
    legend("topright", 
           legend = c(
             paste0("Current: ", sprintf("%.2f", current_val)),
             paste0("Mean: ", sprintf("%.2f", mean_val)),
             paste0("Min: ", sprintf("%.2f", min_val)),
             paste0("Max: ", sprintf("%.2f", max_val)),
             paste0("Points: ", length(values))
           ),
           col = c("#F44336", "#FF9800", "black", "black", "#2196F3"),
           lty = c(2, 2, 0, 0, 1),
           lwd = c(1.5, 1.5, 0, 0, 2),
           pch = c(NA, NA, NA, NA, 20),
           bg = "white",
           cex = 0.9)
    
    dev.off()
    log_message("✓ Generated plot with", length(values), "data points")
    return(plot_file)
  }, error = function(e) {
    log_message("Error generating plot:", e$message)
    if (file.exists(plot_file)) unlink(plot_file)
    return(NULL)
  })
}

# Parameter name mapping for user-friendly commands
map_param_acronym <- function(acronym) {
  acronym_lower <- tolower(trimws(acronym))
  mapping <- list(
    "depth" = "Depth",
    "cdom" = "CDOM",
    "turb" = "Turbidity",
    "turbidity" = "Turbidity",
    "o2" = "Dissolved O2",
    "oxygen" = "Dissolved O2",
    "cond" = "Conductivity",
    "conductivity" = "Conductivity",
    "temp" = "Water Temperature",
    "dot" = "Water Temperature",
    "condtemp" = "Temperature_Cond",
    "condt" = "Temperature_Cond",
    "volt" = "Battery Voltage",
    "battery" = "Battery Voltage"
  )
  if (acronym_lower %in% names(mapping)) {
    return(mapping[[acronym_lower]])
  }
  return(acronym)
}

dispatch_command <- function(chat_id, text) {
  cmd <- tolower(trimws(text))
  
  if (cmd == "/status") {
    data <- read_status()
    send_message(chat_id, format_status(data))
    
  } else if (cmd == "/alarms") {
    data <- read_status()
    if (is.null(data)) {
      send_message(chat_id, "No data")
    } else {
      alarms <- data[data$severity == 2, ]
      if (nrow(alarms) == 0) {
        send_message(chat_id, "✅ No alarms")
      } else {
        send_message(chat_id, format_status(alarms))
      }
    }
    
  } else if (startsWith(cmd, "/latest")) {
    parts <- strsplit(text, "\\s+", perl = TRUE)[[1]]
    if (length(parts) < 2) {
      send_message(chat_id, "Usage: /latest <station>\nExample: /latest martigny")
      return()
    }
    station_query <- paste(parts[-1], collapse = " ")
    data <- read_status()
    if (is.null(data)) {
      send_message(chat_id, "❌ Data unavailable")
      return()
    }
    matched <- data[tolower(data$station) == tolower(station_query), ]
    if (nrow(matched) == 0) {
      send_message(chat_id, paste0("❌ No data found for station: ", station_query))
      return()
    }
    send_message(chat_id, format_status(matched))
    
  } else if (startsWith(cmd, "/mute")) {
    parts <- strsplit(text, "\\s+", perl = TRUE)[[1]]
    if (length(parts) < 3) {
      send_message(chat_id, "Usage: /mute <station> <parameter> [Xd]\nExamples:\n  /mute verbier volt\n  /mute verbier volt 7d")
      return()
    }
    station <- parts[2]
    
    # Check if last part is a duration (e.g., "7d")
    last_part <- parts[length(parts)]
    days <- NULL
    param_parts <- parts[3:length(parts)]
    
    if (grepl("^\\d+d$", last_part, ignore.case = TRUE)) {
      days <- as.numeric(gsub("d", "", last_part, ignore.case = TRUE))
      param_parts <- parts[3:(length(parts)-1)]
    }
    
    parameter <- paste(param_parts, collapse = " ")
    param_full <- map_param_acronym(parameter)
    mute_result <- add_mute(station, param_full, days)
    
    if (!is.null(mute_result$days)) {
      send_message(chat_id, paste0("🔇 Muted alarms for ", mute_result$days, " days: ", station, " - ", param_full))
    } else {
      send_message(chat_id, paste0("🔇 Muted alarms (permanent): ", station, " - ", param_full))
    }
    
  } else if (cmd %in% c("/clearmute", "/clearmutes")) {
    clear_mutes()
    send_message(chat_id, "🔊 All alarm mutes cleared")
    
  } else if (startsWith(cmd, "/1d") || startsWith(cmd, "/3d") || startsWith(cmd, "/7d") || startsWith(cmd, "/30d")) {
    parts <- strsplit(text, "\\s+", perl = TRUE)[[1]]
    if (length(parts) < 3) {
      send_message(chat_id, "Usage: /1d <station> <parameter>\nExample: /1d martigny depth")
      return()
    }
    
    days <- if (startsWith(cmd, "/1d")) 1 else if (startsWith(cmd, "/3d")) 3 else if (startsWith(cmd, "/7d")) 7 else 30
    station_query <- parts[2]
    parameter_query <- paste(parts[3:length(parts)], collapse = " ")
    
    send_message(chat_id, paste0("📊 Generating ", days, "-day plot for ", station_query, " - ", parameter_query, "..."))
    
    plot_file <- generate_timeseries_plot(station_query, parameter_query, days)
    if (!is.null(plot_file) && file.exists(plot_file)) {
      caption <- paste0(station_query, " - ", parameter_query, " (", days, "d)")
      success <- send_photo(chat_id, plot_file, caption)
      unlink(plot_file)
      if (!success) {
        send_message(chat_id, "❌ Failed to send plot")
      }
    } else {
      send_message(chat_id, paste0("❌ Could not generate plot for ", station_query, " - ", parameter_query))
    }
    
  } else if (cmd == "/muted") {
    mutes <- load_mutes()
    if (length(mutes) == 0) {
      send_message(chat_id, "🔇 No active mutes")
      return()
    }
    
    lines <- c("🔇 Active Mutes:", "")
    for (key in names(mutes)) {
      parts <- strsplit(key, "\\|")[[1]]
      station <- parts[1]
      parameter <- parts[2]
      
      mute_info <- mutes[[key]]
      duration_text <- if (!is.null(mute_info$expires_at)) {
        time_left <- difftime(mute_info$expires_at, Sys.time(), units = "days")
        days_left <- ceiling(as.numeric(time_left))
        if (days_left > 0) {
          paste0(days_left, " day", if(days_left > 1) "s" else "", " remaining")
        } else {
          hours_left <- ceiling(as.numeric(difftime(mute_info$expires_at, Sys.time(), units = "hours")))
          if (hours_left > 0) {
            paste0(hours_left, " hour", if(hours_left > 1) "s" else "", " remaining")
          } else {
            "Expires soon"
          }
        }
      } else {
        "Permanent"
      }
      
      lines <- c(lines, paste0("📍 ", station, " - ", parameter))
      lines <- c(lines, paste0("   ⏱️ ", duration_text), "")
    }
    
    send_message(chat_id, paste(lines, collapse = "\n"))
    
  } else if (cmd == "/ping") {
    send_message(chat_id, "🏓 pong")
    
    
  } else if (cmd == "/coffee") {
    send_message(chat_id, "☕ The bot runs on coffee and data!\n\n*beep boop* ... need more caffeine...")
    
  } else if (cmd == "/weather") {
    weather_msgs <- c(
      "🌤️ Perfect weather for monitoring sensors!",
      "⛈️ Storm approaching... better check those sensors!",
      "☀️ Sunny day = happy sensors",
      "🌧️ Rain detected... or is it just the turbidity sensor?",
      "❄️ Winter is coming... check those battery voltages!"
    )
    send_message(chat_id, sample(weather_msgs, 1))
    
  } else if (cmd == "/wisdom") {
    wisdom <- c(
      "💡 A sensor in alarm is worth two in OK status",
      "🧠 The best time to check sensors was yesterday. The second best time is now.",
      "⚡ With great voltage comes great responsibility",
      "🌊 You can't cross the river without monitoring its depth",
      "📊 In data we trust, all others must bring plots",
      "🔋 Battery low? Morale high!",
      "🎯 Measure twice, alert once"
    )
    send_message(chat_id, sample(wisdom, 1))
    
  } else if (cmd == "/dance") {
    send_message(chat_id, "💃🕺\n  ┏(･o･)┛\n  ┗(･o･)┓\n  ┗(･o･)┛\n  ┏(･o･)┓\n\nBot is dancing! Sensors are grooving!")
    
  } else if (cmd == "/motivate") {
    motivation <- c(
      "💪 You got this! Those sensors won't monitor themselves!",
      "🌟 Every alarm is an opportunity to save the day!",
      "🚀 To infinity and beyond... but first, check the battery voltage!",
      "⭐ You're doing amazing! Keep those rivers monitored!",
      "🎉 Another day, another dataset. You're crushing it!",
      "🏆 Sensor monitoring champion right here!"
    )
    send_message(chat_id, sample(motivation, 1))
    
  } else if (cmd == "/joke") {
    jokes <- c(
      "Why did the sensor go to therapy?\nIt had too many issues! 🤖",
      "What's a sensor's favorite music?\nHeavy metal... voltage! ⚡",
      "Why don't sensors ever lie?\nBecause the data doesn't lie! 📊",
      "How do sensors stay in shape?\nThey do circuit training! 💪",
      "What did the battery say to the sensor?\nI'm feeling a bit drained today... 🔋",
      "Why was the turbidity sensor always confused?\nThings were never clear! 🌊"
    )
    send_message(chat_id, sample(jokes, 1))
    
  } else if (cmd == "/secret") {
    send_message(chat_id, "🤫 Psst... the real secret is that I'm powered by Swiss precision and coffee!\n\n🇨🇭☕🤖")
    
  } else if (cmd == "/help") {
    help_text <- paste(
      "🤖 River Bot Commands:",
      "",
      "📊 Data Commands:",
      "  /status - Show all sensor readings",
      "  /alarms - Show only active alarms",
      "  /latest <station> - Show specific station",
      "  /1d <station> <param> - Plot last 1 day",
      "  /3d <station> <param> - Plot last 3 days",
      "  /7d <station> <param> - Plot last 7 days",
      "  /30d <station> <param> - Plot last 30 days",
      "",
      "🔔 Alarm Management:",
      "  /mute <station> <param> [Xd] - Mute alarms",
      "  /muted - List active mutes",
      "  /clearmute - Clear all muted alarms",
      "",
      "🔧 System:",
      "  /ping - Test bot connectivity",
      "  /help - Show this message",
      "",
      "📝 Examples:",
      "  /latest martigny",
      "  /1d verbier volt",
      "  /7d martigny depth",
      "  /30d saxon turb",
      "  /mute verbier volt 7d",
      sep = "\n"
    )
    send_message(chat_id, help_text)
    
  } else {
    send_message(chat_id, "❓ Unknown command. Type /help for options.")
  }
}

process_updates <- function(updates) {
  if (nrow(updates) == 0) return()
  for (i in 1:nrow(updates)) {
    if (!"message.chat.id" %in% names(updates)) next
    if (!"message.text" %in% names(updates)) next
    
    chat_id <- as.numeric(updates$message.chat.id[i])
    text <- as.character(updates$message.text[i])
    
    if (!(chat_id %in% allowed_chat_ids)) next
    
    log_message("Command from", chat_id, ":", text)
    dispatch_command(chat_id, text)
  }
}

# Alarm state management
load_alarm_state <- function() {
  if (!file.exists(ALARM_STATE_FILE)) return(character())
  tryCatch(as.character(readRDS(ALARM_STATE_FILE)), error = function(e) character())
}

save_alarm_state <- function(state) {
  tryCatch(saveRDS(state, ALARM_STATE_FILE), error = function(e) {
    log_message("Failed to save alarm state:", e$message)
  })
}

# Broadcast new alarms
broadcast_alarms <- function() {
  tryCatch({
    data <- read_status()
    if (is.null(data) || nrow(data) == 0) return()
    
    # Get current alarms (severity = 2)
    current_alarms <- data[data$severity == 2, ]
    current_keys <- if (nrow(current_alarms) > 0) {
      paste(current_alarms$station, current_alarms$parameter, sep = "|")
    } else {
      character()
    }
    
    # Get previous alarm state
    prev_keys <- load_alarm_state()
    
    # Find NEW alarms
    new_keys <- setdiff(current_keys, prev_keys)
    if (length(new_keys) > 0) {
      for (key in new_keys) {
        parts <- strsplit(key, "\\|")[[1]]
        station <- parts[1]
        parameter <- parts[2]
        
        # Check if muted
        if (is_muted(station, parameter)) {
          log_message("Skipped muted alarm:", key)
          next
        }
        
        alarm_row <- current_alarms[current_alarms$station == station & 
                                     current_alarms$parameter == parameter, ]
        
        if (nrow(alarm_row) > 0) {
          msg <- sprintf("🔴 ALARM\n📍 Station: %s\n  %s: %.2f %s\n🕐 %s",
                        station, parameter, alarm_row$value[1], alarm_row$units[1],
                        format(alarm_row$timestamp[1], "%Y-%m-%d %H:%M", tz = "Europe/Zurich"))
          
          for (chat_id in allowed_chat_ids) {
            send_message(chat_id, msg)
          }
          log_message("Sent NEW alarm notification:", key)
        }
      }
    }
    
    # Find RESOLVED alarms
    resolved_keys <- setdiff(prev_keys, current_keys)
    if (length(resolved_keys) > 0) {
      for (key in resolved_keys) {
        parts <- strsplit(key, "\\|")[[1]]
        station <- parts[1]
        parameter <- parts[2]
        
        # Always send resolution notifications (don't check mute)
        msg <- sprintf("✅ RESOLVED\n📍 Station: %s\n  %s is now OK", station, parameter)
        
        for (chat_id in allowed_chat_ids) {
          send_message(chat_id, msg)
        }
        log_message("Sent RESOLVED alarm notification:", key)
      }
    }
    
    # Save current alarm state
    save_alarm_state(current_keys)
    
  }, error = function(e) {
    log_message("Error in broadcast_alarms:", e$message)
  })
}

# Main loop
main <- function() {
  last_update_id <- 0
  last_refresh <- Sys.time() - 60
  last_alarm_check <- Sys.time() - 600  # Force alarm check on start
  
  log_message("========================================")
  log_message("ViewLinc Telegram Bot Started")
  log_message("Working directory:", BASE_DIR)
  log_message("Data directory:", DATA_DIR)
  log_message("Allowed chats:", paste(allowed_chat_ids, collapse = ", "))
  log_message("Data refresh: 60 seconds | Alarm check: 600 seconds")
  log_message("========================================")
  
  # No historical data initialization needed - data fetched on-demand for plots
  
  repeat {
    tryCatch({
      current_time <- Sys.time()
      
      # Refresh data every 60 seconds
      if (as.numeric(difftime(current_time, last_refresh, units = "secs")) >= 60) {
        refresh_data()
        last_refresh <- current_time
      }
      
      # Check for alarms every 10 minutes
      if (as.numeric(difftime(current_time, last_alarm_check, units = "secs")) >= 600) {
        log_message("Checking for alarms...")
        clean_expired_mutes()  # Clean up expired mutes
        broadcast_alarms()
        last_alarm_check <- current_time
      }
      
      # Poll Telegram
      updates <- get_updates(if (last_update_id == 0) NULL else last_update_id + 1)
      
      if (nrow(updates) > 0 && "update_id" %in% names(updates)) {
        last_update_id <- max(updates$update_id, na.rm = TRUE)
        process_updates(updates)
        GET(paste0(base_url, "/getUpdates?offset=", last_update_id + 1))
      }
      
    }, error = function(e) {
      log_message("Loop error:", e$message)
    })
    
    Sys.sleep(2)
  }
}

# Start
log_message("Starting bot...")
main()
