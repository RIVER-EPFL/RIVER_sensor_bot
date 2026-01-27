# ViewLinc Telegram Bot

## Overview

A Telegram bot that fetches ViewLinc sensor data every 60 seconds and provides real-time monitoring through Telegram commands. Runs independently as a systemd service without requiring a Shiny app or browser.

## Features

- Direct ViewLinc API integration with 60-second refresh intervals
- Real-time alarm notifications and monitoring
- **Time-series plotting** with on-demand historical data fetching (1d, 3d, 7d, 30d)
- **Time-based alarm muting** (mute for X days with automatic expiration)
- Telegram command interface for sensor queries
- Runs as a systemd service with auto-restart
- No external dependencies beyond ViewLinc API access

## Files

- `telegram_bot_linux.R` - Production bot script
- `viewlinc-telegram.service` - Systemd service configuration
- `.Renviron.example` - Environment variables template
- `INSTALL.md` - Detailed installation guide

## Requirements

- R with packages: `httr`, `jsonlite`, `lubridate`, `grDevices`, `base64enc`
- Network access to ViewLinc server
- Telegram bot token and chat ID

## Configuration

Copy `.Renviron.example` to `.Renviron` and configure:
```bash
TELEGRAM_BOT_TOKEN=your_bot_token
TELEGRAM_ALLOWED_CHAT_IDS=your_chat_id
VIEWLINC_TOKEN=your_viewlinc_token
VIEWLINC_SERVER=https://your-viewlinc-server
BOT_BASE_DIR=/path/to/bot/directory
```

See `INSTALL.md` for detailed setup instructions.

## Available Commands

**Data & Monitoring:**
```
/status                    - Display all sensor readings
/alarms                    - Show active alarms only
/stations                  - List all available stations
/latest <station>          - Show readings for specific station
/1d <station> <param>      - Plot last 1 day of data
/3d <station> <param>      - Plot last 3 days of data
/7d <station> <param>      - Plot last 7 days of data
/30d <station> <param>     - Plot last 30 days of data
```

**Alarm Management:**
```
/mute <site> <param> [Xd]  - Mute alarms (optional: for X days)
/muted                     - List active mutes with time remaining
/clearmute                 - Clear all muted alarms
```

**System & Utilities:**
```
/ping                      - Test bot connectivity
/help                      - Display help message
```

**Examples:**
```
/latest station1
/7d station2 volt
/mute station3 turb 7d
```

## Service Management

**Check status:**
```bash
sudo systemctl status viewlinc-telegram
```

**View logs:**
```bash
sudo journalctl -u viewlinc-telegram -f
```

**Restart service:**
```bash
sudo systemctl restart viewlinc-telegram
```

**Verify data updates:**
```bash
ls -lh $BOT_BASE_DIR/data/latest_status.rds
```

## Troubleshooting

**Bot fails to start**
- Verify using `telegram_bot_linux.R` for full features
- Check all environment variables are set (including `BOT_BASE_DIR` and `VIEWLINC_SERVER`)
- Ensure data directory exists and is writable

**Telegram 409 error (multiple instances)**
```bash
sudo systemctl stop viewlinc-telegram
pkill -f telegram_bot_linux.R
sleep 5
sudo systemctl start viewlinc-telegram
```

**Plotting fails**
- Check that `location_id` is being tracked in status data
- Verify ViewLinc API access for historical data endpoint
- Check logs for API response details

**Data not updating**
- Check service status and logs
- Verify data file timestamp
- Test ViewLinc API connectivity

## Architecture

The bot operates three concurrent loops:
- **Data refresh**: Fetches from ViewLinc API every 60 seconds
- **Command polling**: Checks Telegram for commands every 2 seconds  
- **Alarm monitoring**: Evaluates and notifies on alarms every 10 minutes
- **On-demand plotting**: Fetches historical data from ViewLinc API when plot commands are used

