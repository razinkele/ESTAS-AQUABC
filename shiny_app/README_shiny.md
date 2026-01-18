# AQUABC Shiny Front End (Python)

This is a minimal Python Shiny front-end to run and inspect the AQUABC model in this repository.

## Features
- Build and run the model using Make targets from the UI
- Browse and edit files inside `INPUTS/` (backups created with `.bak`)
- Preview `OUTPUT.csv` and plot variables vs TIME with Plotly (supports multi-series selection, dual-axis plotting, and optional rolling mean smoothing)

## Quick start
1. Create a Python environment (recommended):
   python -m venv .venv && source .venv/bin/activate
2. Install dependencies:
   pip install -r shiny_app/requirements.txt
3. Run the Shiny app (dev):
   shiny run --reload shiny_app:app

Open the local address printed (by default http://127.0.0.1:8000).

---

## Deployment on this machine (systemd + nginx) 🔧
A helper script `shiny_app/deploy.sh` can install dependencies into the repository `.venv`, create a systemd service that runs the app, and optionally create a basic nginx reverse-proxy site.

Basic usage (run from repo root):

1. Make the script executable and run it with sudo:
   - chmod +x shiny_app/deploy.sh
   - sudo bash shiny_app/deploy.sh

What the script does:
- Creates (if missing) `.venv` in the repository and installs `shiny_app/requirements.txt`
- Writes `/etc/systemd/system/shiny_aquabc.service` and enables+starts it (service name: `shiny_aquabc`)
- Optionally writes `/etc/nginx/sites-available/shiny_aquabc` and enables it (reloads nginx) if `nginx` is installed

After deploying:
- Check the service status: `sudo systemctl status shiny_aquabc`
- Follow logs: `sudo journalctl -u shiny_aquabc -f`
- If using nginx, open this machine's HTTP address (port 80) or configure a DNS name and obtain an HTTPS cert (e.g., using certbot).

Notes & security
- The service runs the app as the user that invoked `sudo` (so files remain writable by that user). If you want a dedicated user, edit the systemd unit created by the script.
- The default proxy binds the Shiny app to `127.0.0.1:8000` and uses nginx to expose port 80; you can change port or host if needed.
- Use firewall and proper TLS (certbot) when exposing to public networks.


## Notes & safety
- The app saves changes directly into the `INPUTS/` files. A `.bak` is created for each file on first save.
- The build/run buttons run `make` targets in the repository root. Use with caution; long or interactive runs may block.
- The app is a local development tool. Do not expose to public networks without additional security.

## Improvements you may want
- Add authentication and run queueing for safe multi-user use
- Add visualizations of `OUTPUTS/` binary formats (mtrx / .out parsers)
- Provide templating and differential comparisons for inputs
