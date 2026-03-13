# ViewLinc Telegram Bot

## Overview

A Telegram bot that fetches ViewLinc sensor data every 60 seconds and provides real-time monitoring through Telegram commands. Runs independently as a systemd service without requiring a Shiny app or browser.

## Features

- Direct ViewLinc API integration with 60-second refresh intervals
- Real-time alarm notifications and monitoring
- **Device monitoring** - Automatic monitoring of VPN server, field routers, and vNETs with 1-hour offline delay before alarms
- **Battery forecast alarms** - Automatic alerts when batteries predicted to fail within 7 days
- **CO2 monitoring** - Full support for CO2 parameter with configurable alarm thresholds
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

# Device monitoring hosts
VPN_SERVER=your.vpn.server.com
ROUTER_MARTIGNY=10.0.0.1
ROUTER_SAXON=10.0.0.2
ROUTER_BAGNES=10.0.0.3
ROUTER_DAILLES=10.0.0.4
VNET_MARTIGNY1=192.168.1.1
VNET_MARTIGNY2=192.168.1.2
VNET_SAXON1=192.168.2.1
VNET_SAXON2=192.168.2.2
VNET_VERBIER1=192.168.3.1
VNET_DAILLES1=192.168.4.1
```

See `INSTALL.md` for detailed setup instructions.

## Available Commands

**Data & Monitoring:**
```
/status                    - Display all sensor readings
/alarms                    - Show active alarms only
/stations                  - List all available stations
/latest <station>          - Show readings for specific station
/battery [station]         - Battery forecast (all stations or specific)
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
/server                    - Check device status (VPN, routers, vNETs)
/thresholds                - Show all parameter alarm thresholds
/help                      - Display help message
```

**Examples:**
```
/latest Martigny
/battery                   - Forecast all battery stations
/battery Verbier           - Forecast specific station
/7d Saxon volt
/mute "Les Dailles" turb 7d
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
  - Sensor parameter alarms (Depth, CDOM, Turbidity, DO, CO2, Conductivity, Temperature, Battery)
  - Battery forecast alarms (auto-muted for 24h after notification)
  - Device monitoring alarms (VPN, routers, vNETs - only after 1 hour offline)
  - Alerts when batteries predicted to reach 10.5V within ≤7 days
- **On-demand plotting**: Fetches historical data from ViewLinc API when plot commands are used

## Battery Forecast System

The bot automatically monitors battery health using linear regression on historical voltage data:

- **Forecast calculation**: Uses 7 days of 2 AM voltage readings to predict battery depletion
- **Automatic alarms**: Sends alert when battery predicted to reach 10.5V within 7 days or less
- **Auto-mute**: Each battery forecast alarm is automatically muted for 24 hours to prevent spam
- **Manual check**: Use `/battery` to view forecasts for all stations anytime
- **Trend display**: Shows voltage trend (V/day) for declining batteries

**Forecast message format:**
```
⚠️ BATTERY FORECAST ALARM
📍 Station: Verbier
🔋 Current: 11.62 V
📉 Trend: -0.015 V/day
⏰ Will reach 10.5V in ~7 days
📅 Estimated: 2026-03-04
```

## Device Monitoring System

The bot automatically monitors network infrastructure health:

- **Monitored devices**: VPN server, 4 field routers (Martigny, Saxon, Bagnes, Les Dailles), 6 vNETs, ViewLinc VM
- **Ping frequency**: Every 10 minutes
- **Alarm delay**: Only sends alarm after device is offline for ≥1 hour (prevents false alarms)
- **Offline tracking**: Tracks offline duration with timestamps
- **Recovery notifications**: Sends alert when device comes back online (only if alarm was sent)
- **Manual check**: Use `/server` to view current status of all devices anytime

**Device alarm message format:**
```
🔴 DEVICE ALARM
📍 Device: Martigny Router
❌ Status: OFFLINE for 1.2 hours
```

**Device recovery message format:**
```
✅ DEVICE RECOVERED
📍 Device: Martigny Router
✓ Status: ONLINE
```

## Parameter Thresholds

Use `/thresholds` command to view all alarm thresholds, including:

- **Battery Voltage**: Alarm <10.8V, Warning 10.8-11.4V, OK ≥11.4V
- **CO2**: Alarm <150 or >5000 ppm, Warning 3000-5000 ppm, OK 150-3000 ppm
- **Dissolved O2**: Alarm <0 or >625 µM, Warning <120 or >360 µM, OK 120-360 µM
- **Conductivity**: Alarm <0 or >1000 µS/cm, Warning <100 or >900 µS/cm, OK 100-900 µS/cm
- **Water Temperature**: Alarm <0 or >25°C, Warning <0.5 or >20°C, OK 0.5-20°C
- And more...

