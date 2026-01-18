#!/usr/bin/env bash
set -euo pipefail
# Deployment helper for running the Python Shiny app as a systemd service
# Usage (from repo root): sudo bash shiny_app/deploy.sh

REPO_ROOT=$(cd "$(dirname "$0")/.." && pwd)
SHINY_SERVER_DIR="/srv/shiny-server"
APP_NAME=AQUABC
TARGET_DIR="${SHINY_SERVER_DIR}/${APP_NAME}"
VENV=${REPO_ROOT}/.venv
SHINY_USER="shiny"

echo "Repository root:    $REPO_ROOT"
echo "Target directory:   $TARGET_DIR"
echo "Service user:       $SHINY_USER"

# 1. Check for Shiny Server
if [ ! -d "$SHINY_SERVER_DIR" ]; then
    echo "ERROR: $SHINY_SERVER_DIR not found. Is Shiny Server installed?"
    echo "This script is intended for deployment to a local Shiny Server instance."
    exit 1
fi

# 2. Setup Virtual Environment
echo "1) Checking/Creating virtual environment..."
if [ ! -d "$VENV" ]; then
  echo "Creating venv at $VENV"
  python3 -m venv "$VENV"
fi
# Activate and install
set +u
source "$VENV/bin/activate"
set -u
pip install --upgrade pip
pip install -r "$REPO_ROOT/shiny_app/requirements.txt"
# Ensure shiny is installed (critical for Shiny Server detection)
pip install shiny

# 3. Deployment via Symlink
echo "2) Deploying to Shiny Server..."

if [ -e "$TARGET_DIR" ]; then
    # Check if it's a directory (or symlink to directory)
    if [ -L "$TARGET_DIR" ]; then
        echo "Removing existing symlink at $TARGET_DIR"
        sudo rm "$TARGET_DIR"
    elif [ -d "$TARGET_DIR" ]; then
        echo "WARNING: $TARGET_DIR exists and is a directory (not a symlink)."
        echo "Renaming it to ${TARGET_DIR}.bak"
        sudo mv "$TARGET_DIR" "${TARGET_DIR}.bak"
    else
        echo "ERROR: $TARGET_DIR exists and is not a symlink or directory. Please remove it manually."
        exit 1
    fi
fi

# We symlink the 'shiny_app' folder. Python's __file__ resolution usually handles
# relative paths correctly (resolving to the real path), allowing app.py to find ../INPUTS.
echo "Creating symlink: $TARGET_DIR -> $REPO_ROOT/shiny_app"
sudo ln -s "$REPO_ROOT/shiny_app" "$TARGET_DIR"

# 4. Handle Permissions
echo "3) adjusting permissions..."
echo "Giving $SHINY_USER group access to repository files (needed for reading inputs/writing outputs)..."

# Ensure the shiny user can traverse to the repo
# WARNING: This might expose the parent directories. Adjust as needed for strict security.
NAME_PATH="$REPO_ROOT"
while [ "$NAME_PATH" != "/" ]; do
    sudo chmod o+x "$NAME_PATH" || true
    NAME_PATH=$(dirname "$NAME_PATH")
done

# Grant group ownership to shiny for the repo (so it can write changes via the app)
# We add shiny to the group owning the files, or change the group to shiny.
# Changing group to shiny is often safer than changing owner.
CURRENT_GROUP=$(stat -c '%G' "$REPO_ROOT")
if [ "$CURRENT_GROUP" != "$SHINY_USER" ]; then
    echo "Changing group ownership of $REPO_ROOT to '$SHINY_USER'..."
    sudo chgrp -R "$SHINY_USER" "$REPO_ROOT"
    sudo chmod -R g+rwX "$REPO_ROOT"
    # Ensure setgid bit is set so new files inherit the group
    sudo find "$REPO_ROOT" -type d -exec chmod g+s {} +
else
    echo "Group is already $SHINY_USER. Ensuring write permissions..."
    sudo chmod -R g+rwX "$REPO_ROOT"
fi

# 5. Check for conflicting app.R
if [ -f "$REPO_ROOT/shiny_app/app.R" ]; then
    echo "WARNING: found app.R in shiny_app/. Shiny Server might execute R instead of Python."
    echo "Renaming app.R to app.R.disabled to ensure Python app loads..."
    mv "$REPO_ROOT/shiny_app/app.R" "$REPO_ROOT/shiny_app/app.R.disabled"
fi

echo "Deployment complete."
echo "App should be available at http://localhost:3838/$APP_NAME (or your server's URL)"
echo "Logs: /var/log/shiny-server/"
