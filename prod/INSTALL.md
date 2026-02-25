# Installation Guide

## Prerequisites

1. **Linux server** with network access to ViewLinc server
2. **R installed** (version 4.0+)
3. **Telegram bot token** (create via @BotFather)
4. **ViewLinc API token**

## Step 1: Install R Packages

```bash
sudo R -e "install.packages(c('httr','jsonlite','lubridate','grDevices','base64enc'), repos='https://cloud.r-project.org/')"
```

## Step 2: Setup Directory Structure

```bash
# Create bot directory (adjust path as needed)
mkdir -p /path/to/bot/directory
cd /path/to/bot/directory

# Create data directory
mkdir -p data
chmod 755 data
```

## Step 3: Upload Files

Upload the following files to your bot directory:
- `telegram_bot_linux.R`
- `.Renviron` (copy from `.Renviron.example` and fill in your values)

```bash
# Make script executable
chmod +x telegram_bot_linux.R
```

## Step 4: Configure Environment Variables

Edit `.Renviron` file with your credentials:

```bash
nano .Renviron
```

Required variables:
```
TELEGRAM_BOT_TOKEN=your_bot_token_here
TELEGRAM_ALLOWED_CHAT_IDS=your_chat_id_here
VIEWLINC_TOKEN=your_viewlinc_token_here
VIEWLINC_SERVER=https://your-viewlinc-server
BOT_BASE_DIR=/path/to/bot/directory
```

**Note:** Replace all values with your actual configuration.

## Step 5: Test the Bot

Run manually first to verify everything works:

```bash
cd /path/to/bot/directory
Rscript telegram_bot_linux.R
```

You should see:
```
========================================
ViewLinc Telegram Bot Started
Working directory: /path/to/bot/directory
...
========================================
```

Test with `/ping` command in Telegram. Press `Ctrl+C` to stop.

## Step 6: Install as Systemd Service

```bash
# Copy service file
sudo cp viewlinc-telegram.service /etc/systemd/system/

# Reload systemd
sudo systemctl daemon-reload

# Enable auto-start on boot
sudo systemctl enable viewlinc-telegram

# Start the service
sudo systemctl start viewlinc-telegram
```

## Step 7: Verify Service is Running

```bash
# Check status
sudo systemctl status viewlinc-telegram

# View logs
sudo journalctl -u viewlinc-telegram -f

# Check data file is being created
ls -lh $BOT_BASE_DIR/data/latest_status.rds
```

## Troubleshooting

### Service won't start
```bash
# Check logs for errors
sudo journalctl -u viewlinc-telegram -n 50

# Verify environment file exists
ls -la $BOT_BASE_DIR/.Renviron

# Test script manually
cd $BOT_BASE_DIR
Rscript telegram_bot_linux.R
```

### Network connectivity issues
```bash
# Test ViewLinc API access (replace with your server)
curl -k $VIEWLINC_SERVER/rest/v1/locations

# Check firewall rules
sudo iptables -L
```

### Multiple instances (409 error)
```bash
# Stop all instances
sudo systemctl stop viewlinc-telegram
pkill -f telegram_bot_linux.R
sleep 5
sudo systemctl start viewlinc-telegram
```

## Service Management Commands

```bash
# Start
sudo systemctl start viewlinc-telegram

# Stop
sudo systemctl stop viewlinc-telegram

# Restart
sudo systemctl restart viewlinc-telegram

# View status
sudo systemctl status viewlinc-telegram

# View logs (live)
sudo journalctl -u viewlinc-telegram -f

# View recent logs
sudo journalctl -u viewlinc-telegram -n 100
```

## Updating the Bot

```bash
# Stop the service
sudo systemctl stop viewlinc-telegram

# Backup current version
cp telegram_bot_linux.R telegram_bot_linux.R.backup

# Upload new version
# (use scp or your preferred method)

# Restart service
sudo systemctl start viewlinc-telegram

# Check logs
sudo journalctl -u viewlinc-telegram -f
```
