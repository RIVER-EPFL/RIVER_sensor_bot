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
  list(pattern = "CO2ppm", name = "CO2", order = 5),
  list(pattern = "ConduS[Cc]m", name = "Conductivity", order = 6),
  list(pattern = "DOdegC|DOTdegC", name = "Water Temperature", order = 7),
  list(pattern = "CondTdegC", name = "Temperature_Cond", order = 8),
  list(pattern = "BattV", name = "Battery Voltage", order = 9)
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
  val <- value
  
  # Battery Voltage
  if (grepl("BattV", param_name, ignore.case = TRUE)) {
    if (val < 10.8) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val < 11.4) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # Depth
  if (grepl("Depthmm", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 2000) {
      return(list(severity = 2, status = "Alarm"))
    } else if ((val >= 0 && val <= 100) || (val >= 1000 && val <= 2000)) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # CDOM
  if (grepl("CDOMppb", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 150) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val >= 100 && val <= 150) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # Turbidity
  if (grepl("TurbNTU", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 500) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val >= 100 && val <= 500) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # Dissolved O2
  if (grepl("DOuM", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 625) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val < 120 || val > 360) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # Conductivity
  if (grepl("ConduS[Cc]m", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 1000) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val < 100 || val > 900) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # Water Temperature (DOdegC, DOTdegC, and CondTdegC)
  if (grepl("DOdegC|DOTdegC|CondTdegC", param_name, ignore.case = TRUE)) {
    if (val < 0 || val > 25) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val < 0.5 || val > 20) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  # CO2
  if (grepl("CO2ppm", param_name, ignore.case = TRUE)) {
    if (val < 150 || val > 5000) {
      return(list(severity = 2, status = "Alarm"))
    } else if (val >= 4000 && val <= 5000) {
      return(list(severity = 1, status = "Warning"))
    } else {
      return(list(severity = 0, status = "OK"))
    }
  }
  
  return(list(severity = 0, status = "OK"))
}

clean_token <- function(token) {
  decoded <- rawToChar(base64enc::base64decode(token))
  decoded_clean <- gsub("[\r\n]+$", "", decoded)
  base64enc::base64encode(charToRaw(decoded_clean))
}

# Device monitoring - Load from environment variables
VPN_SERVER <- Sys.getenv("VPN_SERVER", "")
ROUTER_MARTIGNY <- Sys.getenv("ROUTER_MARTIGNY", "")
ROUTER_SAXON <- Sys.getenv("ROUTER_SAXON", "")
ROUTER_BAGNES <- Sys.getenv("ROUTER_BAGNES", "")
ROUTER_DAILLES <- Sys.getenv("ROUTER_DAILLES", "")
VNET_MARTIGNY1 <- Sys.getenv("VNET_MARTIGNY1", "")
VNET_MARTIGNY2 <- Sys.getenv("VNET_MARTIGNY2", "")
VNET_SAXON1 <- Sys.getenv("VNET_SAXON1", "")
VNET_SAXON2 <- Sys.getenv("VNET_SAXON2", "")
VNET_VERBIER1 <- Sys.getenv("VNET_VERBIER1", "")
VNET_DAILLES1 <- Sys.getenv("VNET_DAILLES1", "")

DEVICES <- list(
  vpn_server = list(name = "VPN Server", host = VPN_SERVER, type = "ping"),
  router_martigny = list(name = "Martigny Router", host = ROUTER_MARTIGNY, type = "ping"),
  router_saxon = list(name = "Saxon Router", host = ROUTER_SAXON, type = "ping"),
  router_bagnes = list(name = "Bagnes Router", host = ROUTER_BAGNES, type = "ping"),
  router_dailles = list(name = "Les Dailles Router", host = ROUTER_DAILLES, type = "ping"),
  vnet_martigny1 = list(name = "Martigny vNET 1", host = VNET_MARTIGNY1, type = "ping"),
  vnet_martigny2 = list(name = "Martigny vNET 2", host = VNET_MARTIGNY2, type = "ping"),
  vnet_saxon1 = list(name = "Saxon vNET 1", host = VNET_SAXON1, type = "ping"),
  vnet_saxon2 = list(name = "Saxon vNET 2", host = VNET_SAXON2, type = "ping"),
  vnet_verbier1 = list(name = "Verbier vNET 1", host = VNET_VERBIER1, type = "ping"),
  vnet_dailles1 = list(name = "Les Dailles vNET 1", host = VNET_DAILLES1, type = "ping"),
  viewlinc_vm = list(name = "ViewLinc VM", host = VIEWLINC_SERVER, type = "api")
)

ping_host <- function(host, timeout = 2) {
  tryCatch({
    # Use system ping command (works on Linux)
    result <- system(paste0("ping -c 1 -W ", timeout, " ", host, " > /dev/null 2>&1"), 
                    intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(result == 0)
  }, error = function(e) {
    return(FALSE)
  })
}

check_viewlinc_api <- function() {
  tryCatch({
    # Try to fetch locations as a connectivity test
    result <- fetch_viewlinc("locations")
    return(!is.null(result))
  }, error = function(e) {
    return(FALSE)
  })
}

check_infrastructure <- function() {
  results <- list()
  
  for (key in names(DEVICES)) {
    device <- DEVICES[[key]]
    
    status <- if (device$type == "ping") {
      ping_host(device$host)
    } else if (device$type == "api") {
      check_viewlinc_api()
    } else {
      FALSE
    }
    
    results[[key]] <- list(
      name = device$name,
      host = device$host,
      type = device$type,
      status = status,
      timestamp = Sys.time()
    )
  }
  
  return(results)
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
    
    # Add order column if not present
    if (!"order" %in% names(rows)) {
      rows$order <- sapply(rows$parameter, function(p) {
        for (mapping in PARAM_MAPPING) {
          if (p == mapping$name) return(mapping$order)
        }
        return(999)
      })
    }
    
    # Sort by order
    rows <- rows[order(rows$order), ]
    
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

# Battery forecasting function
forecast_battery <- function(station_name) {
  # Get current data to find the battery voltage location ID
  current_data <- read_status()
  if (is.null(current_data)) {
    log_message("No current data available")
    return(NULL)
  }
  
  # Find matching station and Battery Voltage parameter
  matched <- current_data[
    tolower(current_data$station) == tolower(station_name) & 
    grepl("battery", current_data$parameter, ignore.case = TRUE), 
  ]
  
  if (nrow(matched) == 0) {
    log_message("No battery voltage sensor found for station:", station_name)
    return(NULL)
  }
  
  location_id <- matched$location_id[1]
  
  if (is.na(location_id) || !nzchar(location_id)) {
    log_message("Location ID not available")
    return(NULL)
  }
  
  log_message("Fetching 7 days of battery data for:", station_name)
  
  # Fetch 7 days of historical data
  history_data <- fetch_historical_for_plot(location_id, 7)
  
  if (is.null(history_data) || nrow(history_data) < 3) {
    log_message("Insufficient historical data")
    return(NULL)
  }
  
  # Convert timestamps and values
  times <- tryCatch({
    # Try to convert timestamps - they should be numeric Unix timestamps
    as.POSIXct(as.numeric(history_data$timestamp), 
               origin = "1970-01-01", tz = "Europe/Zurich")
  }, error = function(e) {
    log_message("Error parsing timestamps:", e$message)
    log_message("Timestamp sample:", head(history_data$timestamp, 3))
    NULL
  })
  
  if (is.null(times)) {
    log_message("Failed to parse timestamps, aborting forecast")
    return(NULL)
  }
  
  values <- as.numeric(history_data$value)
  
  # Filter valid data
  if ("valid" %in% names(history_data)) {
    valid_mask <- as.logical(history_data$valid)
    times <- times[valid_mask]
    values <- values[valid_mask]
  }
  
  # Extract 4AM readings (between 3:30 and 4:30)
  hours <- as.numeric(format(times, "%H"))
  minutes <- as.numeric(format(times, "%M"))
  
  # Find readings close to 1AM (1:30 to 2:30)
  am4_mask <- (hours == 2) | (hours == 3 & minutes >= 30) | (hours == 4 & minutes <= 30)
  
  if (sum(am4_mask) < 2) {
    log_message("Not enough 2AM readings found")
    return(NULL)
  }
  
  am4_times <- times[am4_mask]
  am4_values <- values[am4_mask]
  
  # Get one reading per day (closest to 4AM)
  dates <- as.Date(am4_times, tz = "Europe/Zurich")
  unique_dates <- unique(dates)
  
  daily_times <- c()
  daily_values <- c()
  
  for (i in seq_along(unique_dates)) {
    d <- unique_dates[i]
    day_mask <- dates == d
    day_times <- am4_times[day_mask]
    day_values <- am4_values[day_mask]
    
    # Find reading closest to 4AM
    target_time <- as.POSIXct(paste(as.character(d), "04:00:00"), tz = "Europe/Zurich")
    time_diffs <- abs(difftime(day_times, target_time, units = "mins"))
    closest_idx <- which.min(time_diffs)
    
    daily_times <- c(daily_times, as.numeric(day_times[closest_idx]))
    daily_values <- c(daily_values, day_values[closest_idx])
  }
  
  daily_times <- as.POSIXct(daily_times, origin = "1970-01-01", tz = "Europe/Zurich")
  
  if (length(daily_values) < 2) {
    log_message("Not enough daily readings")
    return(NULL)
  }
  
  # Fit linear model (voltage vs days since first reading)
  days_numeric <- as.numeric(difftime(daily_times, daily_times[1], units = "days"))
  
  tryCatch({
    model <- lm(daily_values ~ days_numeric)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    log_message("Battery trend - Slope:", round(slope, 4), "V/day, Intercept:", round(intercept, 2), "V")
    
    # Predict when battery will reach 10.5V
    threshold <- 10.5
    current_voltage <- tail(daily_values, 1)
    
    if (slope >= 0) {
      # Battery is charging or stable
      return(list(
        status = "stable",
        current = current_voltage,
        slope = slope,
        message = sprintf("🔋 Battery Forecast\n📍 %s\n\n✅ Battery is stable/charging (%.2f V/day)\n🔋 Current: %.2f V", 
                         station_name, slope, current_voltage)
      ))
    }
    
    # Calculate days until threshold
    days_to_threshold <- (threshold - intercept) / slope - max(days_numeric)
    
    if (days_to_threshold <= 0) {
      # Already below threshold or will never reach it
      return(list(
        status = "critical",
        current = current_voltage,
        slope = slope,
        message = sprintf("Battery already critical! Current: %.2f V (%.3f V/day)", current_voltage, slope)
      ))
    }
    
    forecast_date <- Sys.Date() + days_to_threshold
    
    return(list(
      status = "forecast",
      current = current_voltage,
      slope = slope,
      days_remaining = round(days_to_threshold, 1),
      forecast_date = forecast_date,
      message = sprintf("📉 Battery Forecast\n📍 %s\n\n🔋 Current: %.2f V\n📊 Trend: %.3f V/day\n⚠️ Will reach 10.5V in ~%.1f days\n📅 Estimated date: %s",
                       station_name, current_voltage, slope, days_to_threshold, 
                       format(forecast_date, "%Y-%m-%d"))
    ))
    
  }, error = function(e) {
    log_message("Error in linear model:", e$message)
    return(NULL)
  })
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
    
  } else if (cmd == "/battery" || startsWith(cmd, "/battery ")) {
    parts <- strsplit(text, "\\s+", perl = TRUE)[[1]]
    
    # If no station specified, forecast all stations
    if (length(parts) < 2) {
      data <- read_status()
      if (is.null(data)) {
        send_message(chat_id, "❌ Data unavailable")
        return()
      }
      
      # Get all stations with battery voltage
      battery_stations <- unique(data[grepl("battery", data$parameter, ignore.case = TRUE), "station"])
      
      if (length(battery_stations) == 0) {
        send_message(chat_id, "No stations with battery voltage found")
        return()
      }
      
      send_message(chat_id, paste0("🔋 Analyzing battery trends for ", length(battery_stations), " stations..."))
      
      results <- list()
      for (station in battery_stations) {
        result <- tryCatch({
          forecast_battery(station)
        }, error = function(e) {
          log_message("Error in forecast_battery for", station, ":", e$message)
          NULL
        })
        if (!is.null(result)) {
          results[[station]] <- result
        }
      }
      
      if (length(results) == 0) {
        send_message(chat_id, "❌ Could not generate forecasts")
        return()
      }
      
      # Wait 3 seconds before sending results
      Sys.sleep(3)
      
      # Combine all results into a single simplified message
      combined_message <- "🔋 Battery Status\n━━━━━━━━━━━━━━━━━━━━\n\n"
      
      for (station in names(results)) {
        res <- results[[station]]
        if (res$status == "stable") {
          combined_message <- paste0(combined_message, 
                                    "📍 ", station, "\n",
                                    "✅ Stable (", sprintf("%.2f", res$slope), " V/day)\n",
                                    "🔋 ", sprintf("%.2f", res$current), " V\n\n")
        } else if (res$status == "critical") {
          combined_message <- paste0(combined_message,
                                    "📍 ", station, "\n",
                                    "⚠️ CRITICAL\n",
                                    "🔋 ", sprintf("%.2f", res$current), " V\n\n")
        } else {
          combined_message <- paste0(combined_message,
                                    "📍 ", station, "\n",
                                    "📉 ", sprintf("%.2f", res$current), " V (", sprintf("%.3f", res$slope), " V/day)\n",
                                    "⚠️ 10.5V in ~", round(res$days_remaining), " days\n",
                                    "📅 ", format(res$forecast_date, "%Y-%m-%d"), "\n\n")
        }
      }
      
      send_message(chat_id, combined_message)
      
    } else {
      # Single station forecast
      station_query <- paste(parts[-1], collapse = " ")
      
      send_message(chat_id, paste0("🔋 Analyzing battery trend for ", station_query, "..."))
      
      result <- tryCatch({
        forecast_battery(station_query)
      }, error = function(e) {
        log_message("Error in forecast_battery:", e$message)
        NULL
      })
      
      if (!is.null(result)) {
        send_message(chat_id, result$message)
      } else {
        send_message(chat_id, paste0("❌ Could not forecast battery for ", station_query, "\nMake sure the station has battery voltage data."))
      }
    }
    
  } else if (cmd == "/stations") {
    data <- read_status()
    if (is.null(data)) {
      send_message(chat_id, "❌ Data unavailable")
      return()
    }
    stations <- unique(data$station)
    if (length(stations) == 0) {
      send_message(chat_id, "No stations found")
    } else {
      station_list <- paste0("📍 Available Stations:\n\n", paste(stations, collapse = "\n"))
      send_message(chat_id, station_list)
    }
    
  } else if (cmd == "/ping") {
    send_message(chat_id, "🏓 pong")
    
  } else if (cmd == "/server") {
    send_message(chat_id, "🔍 Checking device status...")
    
    device_status <- check_infrastructure()
    check_time <- format(Sys.time(), "%H:%M", tz = "Europe/Zurich")
    
    # Group by category
    vpn_status <- device_status$vpn_server
    routers <- device_status[grepl("^router_", names(device_status))]
    vnets <- device_status[grepl("^vnet_", names(device_status))]
    viewlinc <- device_status$viewlinc_vm
    
    # Build status message
    lines <- c("🖥️ Device Status", "━━━━━━━━━━━━━━━━━━━━", "")
    
    # VPN Server
    vpn_icon <- if (vpn_status$status) "✅" else "❌"
    vpn_name <- sub("\\.synology\\.me$", "", vpn_status$host)  # Remove .synology.me
    lines <- c(lines, "🌐 VPN Server:")
    lines <- c(lines, paste0(vpn_icon, " ", vpn_name), "")
    
    # Field Routers
    lines <- c(lines, "📡 Field Routers:")
    for (router in routers) {
      icon <- if (router$status) "✅" else "❌"
      lines <- c(lines, paste0(icon, " ", router$name))
    }
    lines <- c(lines, "")
    
    # vNETs
    lines <- c(lines, "🔌 vNETs:")
    for (vnet in vnets) {
      icon <- if (vnet$status) "✅" else "❌"
      lines <- c(lines, paste0(icon, " ", vnet$name))
    }
    lines <- c(lines, "")
    
    # ViewLinc VM
    viewlinc_icon <- if (viewlinc$status) "✅" else "❌"
    lines <- c(lines, "💾 ViewLinc VM:")
    lines <- c(lines, paste0(viewlinc_icon, " API connectivity"))
    
    # Summary
    total <- length(device_status)
    online <- sum(sapply(device_status, function(x) x$status))
    lines <- c(lines, "", "━━━━━━━━━━━━━━━━━━━━")
    lines <- c(lines, paste0("📊 Summary: ", online, "/", total, " devices online ", check_time))
    
    send_message(chat_id, paste(lines, collapse = "\n"))
    
  } else if (cmd == "/thresholds") {
    threshold_text <- paste(
      "📊 Parameter Alarm Thresholds",
      "━━━━━━━━━━━━━━━━━━━━━━━━━━",
      "",
      "🔋 Battery Voltage:",
      "  🔴 Alarm: < 10.8 V",
      "  ⚠️ Warning: 10.8-11.4 V",
      "  ✅ OK: ≥ 11.4 V",
      "",
      "📏 Depth:",
      "  🔴 Alarm: < 0 or > 2000 mm",
      "  ⚠️ Warning: 0-100 or 1000-2000 mm",
      "  ✅ OK: 100-1000 mm",
      "",
      "🟡 CDOM:",
      "  🔴 Alarm: < 0 or > 150 ppb",
      "  ⚠️ Warning: 100-150 ppb",
      "  ✅ OK: 0-100 ppb",
      "",
      "🌫️ Turbidity:",
      "  🔴 Alarm: < 0 or > 500 NTU",
      "  ⚠️ Warning: 100-500 NTU",
      "  ✅ OK: 0-100 NTU",
      "",
      "💨 Dissolved O2:",
      "  🔴 Alarm: < 0 or > 625 µM",
      "  ⚠️ Warning: < 120 or > 360 µM",
      "  ✅ OK: 120-360 µM",
      "",
      "🫧 CO2:",
      "  🔴 Alarm: < 150 or > 5000 ppm",
      "  ⚠️ Warning: 4000-5000 ppm",
      "  ✅ OK: 150-4000 ppm",
      "",
      "⚡ Conductivity:",
      "  🔴 Alarm: < 0 or > 1000 µS/cm",
      "  ⚠️ Warning: < 100 or > 900 µS/cm",
      "  ✅ OK: 100-900 µS/cm",
      "",
      "🌡️ Water Temperature:",
      "  🔴 Alarm: < 0 or > 25 °C",
      "  ⚠️ Warning: < 0.5 or > 20 °C",
      "  ✅ OK: 0.5-20 °C",
      sep = "\n"
    )
    send_message(chat_id, threshold_text)
    
  } else if (cmd == "/help") {
    help_text <- paste(
      "🤖 River Bot Commands:",
      "",
      "📊 Data Commands:",
      "  /status - Show all sensor readings",
      "  /alarms - Show only active alarms",
      "  /stations - List all available stations",
      "  /latest <station> - Show specific station",
      "  /1d <station> <param> - Plot last 1 day",
      "  /3d <station> <param> - Plot last 3 days",
      "  /7d <station> <param> - Plot last 7 days",
      "  /30d <station> <param> - Plot last 30 days",
      "  /battery [station] - Forecast battery (all or one)",
      "",
      "🔔 Alarm Management:",
      "  /mute <station> <param> [Xd] - Mute alarms",
      "  /muted - List active mutes",
      "  /clearmute - Clear all muted alarms",
      "",
      "🔧 System:",
      "  /ping - Test bot connectivity",
      "  /server - Check device status",
      "  /thresholds - Show alarm thresholds",
      "  /help - Show this message",
      "",
      "📝 Examples:",
      "  /latest martigny",
      "  /battery verbier",
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
    
    # Skip messages from the bot itself
    if ("message.from.is_bot" %in% names(updates)) {
      if (isTRUE(updates$message.from.is_bot[i])) next
    }
    
    # Skip non-command messages
    if (!startsWith(text, "/")) next
    
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

# Check battery forecasts and send alarms
check_battery_forecasts <- function() {
  tryCatch({
    data <- read_status()
    if (is.null(data) || nrow(data) == 0) return()
    
    # Get all stations with battery voltage
    battery_stations <- unique(data[grepl("battery", data$parameter, ignore.case = TRUE), "station"])
    
    if (length(battery_stations) == 0) return()
    
    critical_forecasts <- list()
    
    for (station in battery_stations) {
      result <- tryCatch({
        forecast_battery(station)
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(result) && result$status == "depleting") {
        # Check if battery will reach 10.5V within 7 days
        if (result$days_remaining <= 7) {
          critical_forecasts[[station]] <- result
        }
      }
    }
    
    if (length(critical_forecasts) == 0) return()
    
    # Check which forecasts are new (not already alarmed)
    for (station in names(critical_forecasts)) {
      forecast_key <- paste(station, "Battery Forecast", sep = "|")
      
      # Check if already muted
      if (is_muted(station, "Battery Forecast")) {
        log_message("Skipped muted battery forecast alarm:", station)
        next
      }
      
      result <- critical_forecasts[[station]]
      
      msg <- sprintf("⚠️ BATTERY FORECAST ALARM\n📍 Station: %s\n🔋 Current: %.2f V\n📉 Trend: %.3f V/day\n⏰ Will reach 10.5V in ~%d days\n📅 Estimated: %s",
                    station, 
                    result$current, 
                    result$slope, 
                    round(result$days_remaining),
                    format(result$forecast_date, "%Y-%m-%d"))
      
      for (chat_id in allowed_chat_ids) {
        send_message(chat_id, msg)
      }
      
      # Auto-mute this forecast alarm for this station (mute for 24 hours)
      mute_alarm(station, "Battery Forecast", 24)
      
      log_message("Sent battery forecast alarm for:", station, "- Auto-muted for 24h")
    }
    
  }, error = function(e) {
    log_message("Error in check_battery_forecasts:", e$message)
  })
}

# Device alarm state file with timestamps
DEVICE_STATE_FILE <- file.path(DATA_DIR, "device_state.rds")

load_device_state <- function() {
  if (!file.exists(DEVICE_STATE_FILE)) return(list())
  tryCatch(readRDS(DEVICE_STATE_FILE), error = function(e) list())
}

save_device_state <- function(state) {
  tryCatch(saveRDS(state, DEVICE_STATE_FILE), error = function(e) {
    log_message("Failed to save device state:", e$message)
  })
}

# Check devices and send alarms for offline devices (only after 1 hour)
check_infrastructure_alarms <- function() {
  tryCatch({
    device_status <- check_infrastructure()
    current_time <- Sys.time()
    
    # Load previous state (list with device_name -> list(offline_since, alarm_sent))
    device_state <- load_device_state()
    
    # Track which devices are currently offline
    current_offline <- list()
    
    for (key in names(device_status)) {
      device <- device_status[[key]]
      device_name <- device$name
      
      if (!device$status) {
        # Device is offline
        if (device_name %in% names(device_state)) {
          # Device was already offline - keep existing timestamp
          current_offline[[device_name]] <- device_state[[device_name]]
        } else {
          # Device just went offline - record timestamp
          current_offline[[device_name]] <- list(
            offline_since = current_time,
            alarm_sent = FALSE
          )
          log_message("Device went offline:", device_name)
        }
        
        # Check if device has been offline for more than 1 hour
        offline_duration <- as.numeric(difftime(current_time, current_offline[[device_name]]$offline_since, units = "hours"))
        
        if (offline_duration >= 1 && !current_offline[[device_name]]$alarm_sent) {
          # Check if muted
          if (is_muted(device_name, "Device")) {
            log_message("Skipped muted device alarm:", device_name)
            current_offline[[device_name]]$alarm_sent <- TRUE
            next
          }
          
          # Send alarm
          msg <- sprintf("🔴 DEVICE ALARM\n📍 Device: %s\n❌ Status: OFFLINE for %.1f hours",
                        device_name,
                        offline_duration)
          
          for (chat_id in allowed_chat_ids) {
            send_message(chat_id, msg)
          }
          log_message("Sent device alarm:", device_name, "- offline for", round(offline_duration, 1), "hours")
          
          # Mark alarm as sent
          current_offline[[device_name]]$alarm_sent <- TRUE
        }
      } else {
        # Device is online
        if (device_name %in% names(device_state)) {
          # Device recovered
          if (device_state[[device_name]]$alarm_sent) {
            # Only send recovery if alarm was sent
            msg <- sprintf("✅ DEVICE RECOVERED\n📍 Device: %s\n✓ Status: ONLINE", device_name)
            
            for (chat_id in allowed_chat_ids) {
              send_message(chat_id, msg)
            }
            log_message("Sent device recovery notification:", device_name)
          } else {
            log_message("Device recovered before 1 hour:", device_name)
          }
        }
        # Don't add to current_offline (device is online)
      }
    }
    
    # Save current offline state
    save_device_state(current_offline)
    
  }, error = function(e) {
    log_message("Error in check_infrastructure_alarms:", e$message)
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
        check_battery_forecasts()  # Check battery forecast alarms
        check_infrastructure_alarms()  # Check device status
        last_alarm_check <- current_time
      }
      
      # Poll Telegram
      updates <- tryCatch({
        get_updates(if (last_update_id == 0) NULL else last_update_id + 1)
      }, error = function(e) {
        log_message("Error in get_updates:", e$message)
        data.frame()
      })
      
      if (!is.null(updates) && nrow(updates) > 0 && "update_id" %in% names(updates)) {
        log_message("Received", nrow(updates), "update(s)")
        last_update_id <- max(updates$update_id, na.rm = TRUE)
        process_updates(updates)
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
