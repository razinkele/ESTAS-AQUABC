# ESTAS-AQUABC Tutorial: Getting Started

**A step-by-step practical guide to running the ESTAS-AQUABC ecological model
using the Shiny web interface**

---

## Table of Contents

1. [Overview](#1-overview)
2. [Prerequisites](#2-prerequisites)
3. [Starting the Application](#3-starting-the-application)
4. [Application Layout](#4-application-layout)
5. [Tutorial Exercise: 30-Day Simulation](#5-tutorial-exercise-30-day-simulation)
   - [Step 1: Dashboard Overview](#step-1-dashboard-overview)
   - [Step 2: Building the Model](#step-2-building-the-model)
   - [Step 3: Exploring Input Files](#step-3-exploring-input-files)
   - [Step 4: Reviewing Parameters](#step-4-reviewing-parameters)
   - [Step 5: Setting Initial Conditions](#step-5-setting-initial-conditions)
   - [Step 6: Configuring Model Options](#step-6-configuring-model-options)
   - [Step 7: Configuring the Simulation](#step-7-configuring-the-simulation)
   - [Step 8: Running the Simulation](#step-8-running-the-simulation)
   - [Step 9: Viewing Results](#step-9-viewing-results)
   - [Step 10: Analysing Mass Balance](#step-10-analysing-mass-balance)
   - [Step 11: Comparing with Observations](#step-11-comparing-with-observations)
6. [Working with Scenarios](#6-working-with-scenarios)
7. [Geographic Visualization](#7-geographic-visualization)
8. [Tips and Troubleshooting](#8-tips-and-troubleshooting)
9. [Data Files Reference](#9-data-files-reference)

---

## 1. Overview

ESTAS-AQUABC is a coupled hydrodynamic–biogeochemical model for simulating
water quality in aquatic systems. The model couples:

- **ESTAS** — a finite-element hydrodynamic transport solver
  (25-box Curonian Lagoon domain, advective + dispersive transport)
- **AQUABC** — an ecological/biogeochemical module with 36 state variables
  and 318+ calibratable parameters covering phytoplankton, zooplankton,
  nutrients, dissolved oxygen, carbon cycling, metals, and sediment fluxes

This tutorial uses the **Python Shiny web interface** to walk you through a
complete model run using the pre-configured data files shipped with the
repository. By the end you will have:

- Built the Fortran executable from source
- Configured and executed a 30-day simulation
- Inspected model parameters and initial conditions
- Plotted output variables and input forcing timeseries
- Evaluated mass balance results
- Compared model output against observations

### What data files are used?

All input data is already present in the `INPUTS/` directory (89 files).
The tutorial uses `INPUT_30day.txt` — a lightweight 30-day configuration
(days 6209–6239, year 1998) that completes in a few minutes on a modern
machine. The full 365-day configuration (`INPUT.txt`) is also available
for later exploration.

---

## 2. Prerequisites

| Requirement | Details |
|---|---|
| **Operating System** | Linux (tested on Ubuntu 22.04+) |
| **Fortran Compiler** | `gfortran` (GCC 9+) or Intel `ifort`/`ifx` |
| **Python** | 3.10 or newer |
| **Python packages** | `shiny`, `plotly`, `pandas`, `numpy`, `openpyxl`, `ipywidgets`, `shinywidgets`, `fpdf2` (see `shiny_app/requirements.txt`) |
| **Make** | GNU Make |
| **Browser** | Any modern browser (Chrome, Firefox, Edge) |

### Quick environment setup

```bash
# Navigate to the project root
cd /path/to/AQUABCv0.2

# Create a virtual environment (one-time)
python -m venv .venv
source .venv/bin/activate

# Install Python dependencies
pip install -r shiny_app/requirements.txt

# Verify gfortran is available
gfortran --version
```

If you are using the pre-configured server deployment, the environment is
already set up at `/opt/micromamba/envs/shiny/` and the app runs as a
systemd service.

---

## 3. Starting the Application

### Option A: Development mode (recommended for this tutorial)

```bash
cd /path/to/AQUABCv0.2
source .venv/bin/activate
shiny run --reload shiny_app:app
```

Open your browser to **http://127.0.0.1:8000**.

### Option B: Production deployment

If deployed via `shiny_app/deploy.sh`, the app is accessible on the
machine's HTTP address (port 80 via nginx reverse proxy). Check status:

```bash
sudo systemctl status shiny_aquabc
```

---

## 4. Application Layout

The application has a **sidebar navigation** on the left and a **main
content area** on the right. The header bar at the top provides access
to the **Changelog** (journal icon), **State Variables Reference** (question
mark icon), and **Settings** (gear icon).

### Navigation Pages

| # | Page | Purpose |
|---|------|---------|
| 1 | **Dashboard** | Quick Run button, system status, simulation config summary, run log |
| 2 | **Model Structure** | Interactive network diagram of all state variables and interactions |
| 3 | **Model Build** | Compile the Fortran source code (gfortran/ifort, release/debug/fast) |
| 4 | **Model Config** | Configure simulation parameters, run the model, set output options |
| 5 | **Input Files** | Browse, preview, and analyse all files in `INPUTS/` with map display |
| 6 | **Parameters** | View and edit the 318 model constants in `WCONST_04.txt` by category |
| 7 | **Initial Cond.** | View and edit initial concentrations for all 36 state variables |
| 8 | **Model Options** | Toggle model switches (zooplankton, redox, light, cyanobacteria, etc.) |
| 9 | **Scenarios** | Save/load named scenario presets (parameters + ICs + options) |
| 10 | **Plots** | Visualise model output, input timeseries, and data previews |
| 11 | **Mass Balance** | Calculate and display element mass balance (N, C, P, Si) |
| 12 | **Observations** | Load observation data and compare against model output |
| 13 | **Map** | Geographic 3D map visualization of the model domain |

---

## 5. Tutorial Exercise: 30-Day Simulation

This exercise walks through every major page of the application using the
bundled Curonian Lagoon dataset.

---

### Step 1: Dashboard Overview

1. **Click "Dashboard"** in the sidebar (it is selected by default on startup).

2. You will see three main sections:
   - **Quick Run / Stop buttons** and a run timer at the top
   - **System Status** (left) — shows OS, Python version, available executables,
     current model configuration, and the command that will be executed
   - **Simulation Config** (centre) — displays key parameters loaded from the
     selected `INPUT*.txt` file (base year, time period, steps/day, folders)
   - **Run Log** (right) — shows real-time output when a simulation is running

3. Review the **System Status** panel. It shows:
   - The selected executable (default: `ESTAS_II`)
   - The selected input file (default: `INPUT.txt`)
   - The number of input files found in `INPUTS/`

4. **Do not click Quick Run yet** — we will first build the executable and
   configure the simulation.

> **Tip:** The "Model Config" button below the System Status panel is a
> shortcut to the Model Config page.

---

### Step 2: Building the Model

Before running a simulation, you need a compiled executable.

1. **Click "Model Build"** in the sidebar.

2. The page has three columns:
   - **Build Configuration** (left) — compiler, build type, and options
   - **Available Executables** (centre) — list of compiled executables
   - **Build Log** (right) — compilation output

3. **Configure the build:**
   - **Compiler:** Select `gfortran (GNU)` (default, recommended)
   - **Build Type:** Select `Release` (optimised, good for production runs)
   - Leave **"Clean before build"** unchecked for a normal build
   - The **Target Executable** will show `ESTAS_gf_release`

4. **Click the "Build" button.**

5. Watch the **Build Log** panel on the right. The compilation takes approximately
   30–60 seconds depending on your machine. You will see:
   - Individual Fortran source files being compiled
   - Library archive creation
   - Final linking step
   - A success or failure message

6. Once the build completes successfully:
   - The new executable appears in the **Available Executables** list
   - Use the **"Select for Run"** dropdown to choose the newly built executable
   - If `ESTAS_II` already exists (pre-compiled), you may use that instead

> **Note:** If you already have a compiled `ESTAS_II` executable (shown in
> the Available Executables list), you can skip building and proceed directly
> to **Step 3**. The repository includes a pre-compiled `ESTAS_II`
> (gfortran, 64-bit Linux).

---

### Step 3: Exploring Input Files

1. **Click "Input Files"** in the sidebar.

2. The page has two main sections:
   - **File Browser** (left) — file list with category filter and file information
   - **File Contents / Map Display** (right) — preview panel with two tabs

3. **Use the category filter** dropdown to explore files by type:
   - *Forcing Data* — meteorological and hydrological timeseries
   - *Geometry* — box connectivity and bathymetry
   - *Initial Conditions* — starting concentrations
   - *Constants* — model parameter files
   - *Sediment* — prescribed sediment flux files

4. **Select and preview key files:**

   | File | Description | What to look for |
   |------|-------------|------------------|
   | `PELAGIC_INPUTS.txt` | Master pelagic config | Box count, layer structure, file references |
   | `ADVECTIVE_LINKS.txt` | Box connectivity | Transport link definitions (from → to) |
   | `TEMP_TS.txt` | Water temperature | Time-varying forcing for all 25 boxes |
   | `FLOW_TS.txt` | Water flow rates | Hydrodynamic forcing timeseries |
   | `WCONST_04.txt` | Model constants | All 323 calibrated parameter values |
   | `INIT_CONC_1.txt` | Initial conditions | Starting concentrations for 36 variables |

5. **Check the File Information** panel below the file list — it shows file
   size, line count, detected format, and data range for timeseries files.

6. **Switch to the "Map Display" tab** to see:
   - **Box Network** — connectivity diagram of the 25 model boxes
   - **Bathymetry Profile** — depth profile for a selected box
   - **Box Depths Overview** — comparative depth view across all boxes

---

### Step 4: Reviewing Parameters

1. **Click "Parameters"** in the sidebar.

2. The page shows parameter values from the constants file (`WCONST_04.txt`),
   organised into 14 categories:

   | Category | Parameters | Description |
   |----------|-----------|-------------|
   | General | 1–17 | Light, temperature functions, global switches |
   | Diatoms | 18–54 | Diatom growth, respiration, mortality |
   | Non-fixing Cyanobacteria | 55–89 | Non-N2-fixing cyano processes |
   | Fixing Cyanobacteria | 90–124 | N2-fixing cyanobacteria processes |
   | Other Phytoplankton | 125–159 | Other algae (OPA) kinetics |
   | Zooplankton | 160–192 | Grazing, respiration, mortality |
   | Detritus | 193–218 | Particulate organic matter |
   | Dissolved Organics | 219–234 | DOM cycling |
   | Nitrification | 235–250 | NH4 → NO3 transformation |
   | Redox Chemistry | 251–272 | Fe, Mn, S redox |
   | Methane | 273–283 | CH4 production and oxidation |
   | Settling | 284–297 | Particle settling velocities |
   | pH Effects | 298–305 | Carbonate system, alkalinity |
   | Nostocales | 306–323 | Nostocales-specific parameters |

3. **Select a category** (e.g., "Diatoms") and click **"Load"**.

4. The **Parameters** table shows:
   - Parameter index number
   - Current value
   - An editable input field for each parameter

5. **For this tutorial, do not modify any parameters.** Simply review the
   values to familiarise yourself with the model calibration. The supplied
   `WCONST_04.txt` contains the latest calibrated parameter set.

> **Tip:** If you do modify parameters, click **"Save All Changes"** to
> write the updated values back to the file. A `.bak` backup is created
> automatically before any save.

---

### Step 5: Setting Initial Conditions

1. **Click "Initial Cond."** in the sidebar.

2. Select the IC file: **`INIT_CONC_1.txt`** (default).

3. Choose a category to view. State variables are grouped into 11 categories:

   | Category | Variables |
   |----------|-----------|
   | Nutrients | NH4-N, NO3-N, PO4-P, Dissolved Si |
   | Dissolved Gases | Dissolved Oxygen |
   | Phytoplankton | DIA_C, CYN_C, OPA_C, FIX_CYN_C, NOST_VEG_HET_C, AKI_C |
   | Zooplankton | ZOO_C, ZOO_N, ZOO_P |
   | Particulate Organics | DET_PART_ORG_C, DET_PART_ORG_N, DET_PART_ORG_P, PART_Si |
   | Dissolved Organics | DISS_ORG_C, DISS_ORG_N, DISS_ORG_P |
   | Carbonate System | INORG_C, TOT_ALK |
   | Metals | FE_II, FE_III, MN_II, MN_IV, CA, MG |
   | Sulphur | S_PLUS_6, S_MINUS_2 |
   | Allelopathy | SEC_METAB_DIA, SEC_METAB_NOFIX_CYN, SEC_METAB_FIX_CYN, SEC_METAB_NOST |
   | Other | CH4_C |

4. Click **"Load"** to display the current initial values.

5. Review the concentrations. The values represent starting conditions at
   simulation time 6209 (approximately 1 January of the simulation year).

6. **For this tutorial, keep the default values and proceed.**

---

### Step 6: Configuring Model Options

1. **Click "Model Options"** in the sidebar.

2. Select a category from the dropdown. Available categories:

   | Category | What it controls |
   |----------|-----------------|
   | Zooplankton | Feeding preference, switching function selection |
   | Redox Chemistry | Fe/Mn/S cycling switches |
   | Light | Light limitation formulation |
   | Cyanobacteria | N2 fixation switches, CTMI temperature model |
   | Allelopathy | Secondary metabolite interactions |
   | Organic Matter | DOM/POM processing options |

3. Click **"Load Options"** to display the current settings.

4. The page shows two panels:
   - **Model Switches** — on/off toggles for various model features
   - **Extra Constants** — additional constants from `EXTRA_WCONST.txt`

5. **For this tutorial, keep the default options and proceed.**

---

### Step 7: Configuring the Simulation

1. **Click "Model Config"** in the sidebar.

2. The page has three tabs: **Simulation Config**, **Run Model**, and
   **Output Config**.

#### Tab 1: Simulation Config

3. Click **"Load Configuration"** to read the current `INPUT.txt` settings.

4. Configure the 30-day tutorial run:

   | Setting | Value | Notes |
   |---------|-------|-------|
   | **Base Year** | `1998` | Reference year for forcing data |
   | **Start Date** | Corresponds to day 6209 | ~1 Jan of the simulation year |
   | **End Date** | Corresponds to day 6239 | ~31 Jan (30-day run) |
   | **Time Step** | `6 minutes` (240 steps/day) | Recommended default |
   | **Output Frequency** | `Hourly` (print interval = 10) | 10 × 6 min = 1 hour |
   | **Enable Sediment Model** | `Off` | Faster run without sediment diagenesis |
   | **Resuspension Option** | `0 (Disabled)` | Simplest configuration for tutorial |

   Alternatively — and **recommended for this tutorial** — you can simply
   select `INPUT_30day.txt` as the input file in the Run Model tab (next
   step), which already has these settings pre-configured.

5. If you modified settings, click **"Save Configuration"** to write them
   to the input file.

#### Tab 2: Run Model

6. Switch to the **"Run Model"** tab.

7. Configure the run parameters:

   - **Executable:** Select the executable you built in Step 2 (e.g.,
     `ESTAS_gf_release`) or use the default `ESTAS_II`
   - **Input Configuration File:** Select **`INPUT_30day.txt`** from the
     dropdown — this is the pre-configured 30-day tutorial run
   - **Pelagic Constants File:** Select **`WCONST_04.txt`** (recommended) or
     leave as "(not used - use defaults)" to use built-in constants
   - **Enable Binary Output:** Leave `Off` for this tutorial

8. Check the **Command Preview** box at the bottom of the left panel. It
   should show something like:

   ```
   ./ESTAS_II INPUT_30day.txt WCONST_04.txt
   ```

   If using a custom-named executable:
   ```
   ./ESTAS_gf_release INPUT_30day.txt WCONST_04.txt
   ```

#### Tab 3: Output Config

9. Switch to the **"Output Config"** tab (optional but informative).

10. Review the **Output Boxes** selection. By default, boxes 5, 6, 8, 9, 14,
    17, and 25 are selected. These are the boxes for which output files
    (`PELAGIC_BOX_*.out`) will be written.

11. Review the **Output Directory**. For `INPUT_30day.txt`, output goes to
    `OUTPUTS_30day/`.

12. The **Output Types** section lets you select:
    - **State Variables** — concentration timeseries (always recommended)
    - **Process Rates** — biogeochemical flux timeseries
    - **Mass Balance** — conservation check timeseries

---

### Step 8: Running the Simulation

You can run the simulation from either the **Dashboard** or the **Model
Config → Run Model** tab.

#### Method A: Quick Run from Dashboard

1. Navigate to the **Dashboard**.
2. Verify the System Status panel shows the correct executable and input file.
3. Click the green **"Quick Run"** button.

#### Method B: Run from Model Config

1. Navigate to **Model Config → Run Model** tab.
2. Verify the command preview is correct.
3. Click the green **"Run Model"** button.

#### Monitoring the Run

4. The **Run Log** panel (right side of Dashboard, or right side of Run Model
   tab) shows real-time output:

   ```
   Starting quick run...
   Validating input files...
   OK Input files validated
   OK Constants file validated: WCONST_04.txt (323 constants)

   Command: ./ESTAS_II INPUT_30day.txt WCONST_04.txt
   ------------------------------------
   Starting model execution...
   ```

5. The model will output progress as it processes each simulation day. A
   30-day run typically completes in **2–5 minutes** depending on hardware.

6. On the Dashboard, a **timer** display shows elapsed time.

7. When the run finishes, the log shows:
   ```
   OK Model completed successfully
   ```

8. To **stop a running simulation**, click the red **"Stop"** button.

> **What if the run fails?** Check the Run Log for error messages. Common
> issues:
> - "Executable not found" → Go to Model Build and compile first
> - "Missing required file" → Check that `INPUTS/` directory has all files
> - "Constants file validation failed" → Select `WCONST_04.txt` which has
>   all 323 required constants

---

### Step 9: Viewing Results

1. **Click "Plots"** in the sidebar.

2. The Plots page has four tabs:
   - **Output Directory** — select which output folder to analyse
   - **Model Output** — plot state variable timeseries
   - **Input Timeseries** — visualise forcing data
   - **Data Preview** — tabular view of raw data

#### Selecting the Output Directory

3. In the **Output Directory** tab:
   - Select **`OUTPUTS_30day`** from the dropdown (or click "Refresh
     Directories" if it's not listed)
   - Click **"Analyze Directory"** to see a summary of available output files

#### Plotting Model Output

4. Switch to the **Model Output** tab.

5. Configure the plot:
   - **File format:** Select `Text (.out)`
   - **Output file:** Select a box file, e.g., `PELAGIC_BOX_00005.out`
     (Box 5 — central lagoon area)

6. Select variables to plot:
   - **Left axis:** Choose one or more variables, e.g.:
     - `DISS_OXYGEN` — dissolved oxygen
     - `NH4_N` — ammonium nitrogen
   - **Right axis:** Optionally add a variable on a different scale, e.g.:
     - `DIA_C` — diatom carbon biomass

7. Set plot options:
   - **Apply rolling mean:** Enable and set window size (e.g., 5) to smooth
     noisy output
   - **Log scale:** Enable for variables spanning several orders of magnitude

8. Click **"Refresh Plot"** or the plot updates automatically.

9. The interactive Plotly chart allows:
   - **Hover** over data points to see exact values and time
   - **Zoom** by clicking and dragging to select a region
   - **Pan** by holding shift and dragging
   - **Reset zoom** by double-clicking
   - **Download** the plot as PNG using the camera icon in the toolbar

#### Suggested Variables to Explore

| Variable | Unit | What it shows |
|----------|------|---------------|
| `NH4_N` | mg N/L | Ammonium — key nutrient |
| `NO3_N` | mg N/L | Nitrate — nutrient, nitrification product |
| `PO4_P` | mg P/L | Phosphate — limiting nutrient |
| `DISS_OXYGEN` | mg O2/L | Dissolved oxygen — ecosystem health indicator |
| `DIA_C` | mg C/L | Diatom biomass |
| `CYN_C` | mg C/L | Non-fixing cyanobacteria biomass |
| `FIX_CYN_C` | mg C/L | Nitrogen-fixing cyanobacteria biomass |
| `ZOO_C` | mg C/L | Zooplankton biomass |
| `INORG_C` | mg C/L | Dissolved inorganic carbon |
| `TOT_ALK` | meq/L | Total alkalinity |

#### Plotting Input Timeseries

10. Switch to the **Input Timeseries** tab.

11. Select a forcing file:
    - `Temperature` — water temperature (°C)
    - `Salinity` — water salinity (PSU)
    - `Flow` — water flow rates (m³/s)
    - `Solar Radiation` — incoming solar radiation (W/m²)
    - `Wind Speed` — wind speed (m/s)
    - `Air Temperature` — air temperature (°C)
    - `Shear Stress` — bottom shear stress (Pa)

12. Select boxes to display (e.g., "5", "14", "25" to compare lake regions).

13. Click **"Plot Timeseries"** to generate the chart.

#### Data Preview

14. The **Data Preview** tab shows raw tabular data from the currently
    loaded output file. Use this to inspect exact values.

---

### Step 10: Analysing Mass Balance

1. **Click "Mass Balance"** in the sidebar.

2. Click the **"Calculate Mass Balance"** button.

3. The page shows:
   - **Summary table** — mass balance for Nitrogen, Carbon, Phosphorus, and
     Silicon showing initial mass, final mass, and relative error
   - **Element Details** — select an element (e.g., Nitrogen) to see a
     breakdown of all contributing state variables
   - **Time Series** — temporal evolution of total element mass over the
     simulation period

4. A well-calibrated run should show mass balance relative errors below 1%.
   Larger errors may indicate numerical instability or configuration issues.

---

### Step 11: Comparing with Observations

1. **Click "Observations"** in the sidebar.

2. The page provides two ways to load observation data:

   **Method A: From OBSERVATIONS directory**
   - Click **"Scan OBSERVATIONS Directory"** to find available files
   - The bundled files include:
     - `Water_column_chemistry_2015.xlsx` — field measurements
     - `2015ST14.xlsx` — station 14 observation data
     - `PelagicProcesses.xlsx` — measured pelagic process rates
     - `.dates` files — date-indexed observation records
   - Select a file and click **"Load Selected File"**

   **Method B: Upload your own**
   - Use the file upload widget to load CSV or Excel files

   **Method C: Generate synthetic data**
   - Click **"Generate Sample Data"** to create synthetic observations for
     testing the comparison workflow

3. After loading observations:
   - **File Preview** shows the data structure and available columns
   - **Comparison Summary** shows statistical metrics (RMSE, R², bias) for
     matched variables
   - **Variable Details** — select a variable for detailed comparison metrics
   - **Scatter Plot Info** — model vs. observation scatter analysis

---

## 6. Working with Scenarios

The **Scenarios** page lets you save and restore complete model configurations.

### Saving a Scenario

1. **Click "Scenarios"** in the sidebar.

2. In the **"Save Current Configuration"** panel:
   - Enter a **name** (e.g., "Tutorial_baseline")
   - Enter a **description** (e.g., "Default 30-day run with WCONST_04")
   - Select what to include:
     - [x] Parameters (WCONST_04.txt)
     - [x] Initial Conditions (select INIT_CONC_1.txt)
     - [x] Model Options & Constants
   - Click **"Save as New Scenario"**

### Loading a Scenario

3. Use the **"Select Scenario"** dropdown to choose a previously saved
   configuration.

4. Click **"Load"** to replace the current configuration with the saved one.

5. A scenario can be **deleted** using the "Delete" button if no longer needed.

> **Why use scenarios?** Scenarios are particularly useful for:
> - Saving a calibrated baseline before making changes
> - Comparing different parameter sets
> - Quickly switching between configurations for sensitivity analysis

---

## 7. Geographic Visualization

1. **Click "Map"** in the sidebar.

2. The map shows the Curonian Lagoon domain centred at approximately
   55.32°N, 21.10°E.

3. **Map controls:**
   - **Map Style:** Choose from OpenStreetMap, Light/Dark (Carto), Satellite
     (Esri), or Topographic
   - **Centre Latitude/Longitude:** Adjust map centre coordinates
   - **Zoom Level:** 1 (world) to 18 (street level); default 10
   - **Pitch (3D tilt):** Angle for 3D perspective view (default 45°)
   - **Point Radius:** Size of data points on the map
   - **Elevation Scale:** Vertical exaggeration for 3D visualisation

4. The map also displays sample data points corresponding to model box
   locations.

---

## 8. Tips and Troubleshooting

### Performance

- The **30-day simulation** (`INPUT_30day.txt`) is recommended for learning
  and testing. It completes in 2–5 minutes.
- The **365-day simulation** (`INPUT.txt`) takes 30–60 minutes and produces
  ~8,700 output lines per box.
- Use **Release** build type for production runs. Debug builds are 5–10×
  slower but provide better error diagnostics.

### Common Issues

| Problem | Solution |
|---------|----------|
| "Executable not found" | Go to Model Build and compile, or verify ESTAS_II exists |
| "Missing required file" | Check `INPUTS/` directory has PELAGIC_INPUTS.txt, PELAGIC_MODEL_OPTIONS.txt, TEMP_TS.txt, FLOW_TS.txt, ADVECTIVE_LINKS.txt, INIT_CONC_1.txt |
| "Constants file validation failed" | Use `WCONST_04.txt` which has all 323 required constants |
| Run produces NaN or crashes | Try Debug build type to get detailed error output; check initial conditions for zero or negative values |
| "Intel runtime libraries not found" | Only relevant for ifort-compiled executables; use gfortran instead |
| Plot shows no data | Verify the output directory matches the INPUT file configuration; click "Refresh Files" |
| App won't start | Check Python environment: `pip install -r shiny_app/requirements.txt` |

### Navigating Efficiently

- Use the **Dashboard** for a quick overview and one-click model run
- The **Model Structure** diagram provides a visual reference of all
  biogeochemical interactions — useful when interpreting output variables
- The **State Variables Reference** (question mark icon in header) provides
  a quick-reference table of all 36 state variables
- The **Settings** panel (gear icon) allows theme customisation

### File Safety

- All parameter and IC file edits create automatic `.bak` backups
- The app writes directly to `INPUTS/` files — use Scenarios to save
  checkpoints before major changes
- Output files are overwritten on each new run into the configured directory

---

## 9. Data Files Reference

### Input Configuration Files (`INPUT*.txt`)

These files sit in the project root and control the overall simulation.

| File | Duration | Resuspension | Sediment Model | Output Folder | Notes |
|------|----------|-------------|----------------|---------------|-------|
| `INPUT_30day.txt` | 30 days | Disabled (0) | Off | `OUTPUTS_30day/` | **Tutorial — recommended** |
| `INPUT.txt` | 365 days | Semi-prescribed (2) | Off | `OUTPUTS/` | Full annual simulation |
| `INPUT_gf_release.txt` | 365 days | — | — | — | Release build config |
| `INPUT_gf_debug.txt` | 365 days | — | — | — | Debug build config |
| `INPUT_gf_fast.txt` | 365 days | — | — | — | Fast build config |
| `INPUT_debug_run.txt` | — | — | — | `OUTPUTS_debug_run/` | Debugging configuration |
| `INPUT_sediment_test.txt` | — | — | On | — | Sediment model testing |

### Key Input Data Files (`INPUTS/`)

| File(s) | Category | Description |
|---------|----------|-------------|
| `PELAGIC_INPUTS.txt` | Core | Master pelagic model configuration; references all other input files |
| `PELAGIC_MODEL_OPTIONS.txt` | Core | Model on/off switches and feature toggles |
| `WCONST_04.txt` | Constants | 323 calibrated model constants (recommended set) |
| `EXTRA_WCONST.txt` | Constants | Additional constants for extended features |
| `INIT_CONC_1.txt` | Initial Cond. | Initial state variable concentrations (set 1) |
| `INIT_CONC_2.txt` | Initial Cond. | Initial state variable concentrations (set 2) |
| `ADVECTIVE_LINKS.txt` | Geometry | Advective transport links between boxes |
| `DISPERSIVE_LINKS.txt` | Geometry | Dispersive transport links between boxes |
| `BATHYMETRY_1.txt` – `BATHYMETRY_25.txt` | Geometry | Depth profiles for each of the 25 boxes |
| `TEMP_TS.txt` | Forcing | Water temperature timeseries (all boxes) |
| `SALT_TS.txt` | Forcing | Salinity timeseries |
| `FLOW_TS.txt` | Forcing | Water flow rates |
| `SOLAR_RAD_TS.txt` | Forcing | Solar radiation timeseries |
| `WIND_SPEED_TS.txt` | Forcing | Wind speed timeseries |
| `AIR_TEMP_TS.txt` | Forcing | Air temperature timeseries |
| `CLOUD_COVER_TS.txt` | Forcing | Cloud cover timeseries |
| `EVAPORATION_TS.txt` | Forcing | Evaporation timeseries |
| `BOUNDARY_FLOW_TS.txt` | Forcing | Boundary flow timeseries |
| `FORC_TS_1.txt` – `FORC_TS_9.txt` | Forcing | Boundary concentration forcing timeseries |
| `SETTLING_VELOCITY_TS_1.txt` – `_6.txt` | Forcing | Variable settling velocity timeseries |
| `SHEAR_STRESSES_TS.txt` | Forcing | Bottom shear stress timeseries |
| `ICE_COVER.txt` | Forcing | Ice cover timeseries |
| `ALLELOPATHIC_INFORMATION.txt` | Config | Allelopathic interaction parameters |
| `PRESCRIBED_SEDIMENT_FLUXES.txt` | Sediment | Prescribed benthic sediment fluxes |
| `PRESCRIBED_SEDIMENT_FLUXES_HYPOXIA.txt` | Sediment | Hypoxic sediment flux set |
| `FLUXES_FOR_MUDDY_SEDIMENTS*.txt` | Sediment | Muddy sediment flux data (various sets) |
| `FLUXES_FOR_SANDY_SEDIMENTS*.txt` | Sediment | Sandy sediment flux data |
| `PELAGIC_OUTPUT_INFORMATION_FILE.txt` | Output | Controls which boxes produce output |
| `RESUSPENSION_INPUTS_2.txt` | Sediment | Resuspension configuration |

### Output Files (`OUTPUTS_30day/`)

After a successful 30-day run:

| File | Content |
|------|---------|
| `PELAGIC_BOX_00005.out` | Box 5 state variable timeseries |
| `PELAGIC_BOX_00006.out` | Box 6 state variable timeseries |
| `PELAGIC_BOX_00008.out` | Box 8 state variable timeseries |
| `PELAGIC_BOX_00009.out` | Box 9 state variable timeseries |
| `PELAGIC_BOX_00014.out` | Box 14 state variable timeseries |
| `PELAGIC_BOX_00017.out` | Box 17 state variable timeseries |
| `PELAGIC_BOX_00025.out` | Box 25 state variable timeseries |
| `PELAGIC_BOX_*.mtrx` | Process rate matrices (if enabled) |
| `MASS_BALANCES.out` | Element mass balance timeseries |
| `WATER_LEVELS.out` | Water level timeseries for all boxes |

### Observation Files (`OBSERVATIONS/`)

| File | Description |
|------|-------------|
| `Water_column_chemistry_2015.xlsx` | Field water chemistry measurements |
| `2015ST14.xlsx` | Station 14 monitoring data |
| `PelagicProcesses.xlsx` | Measured pelagic process rates |
| `Growth_fixation_light_mixing.xlsx` | Growth and fixation measurements |
| `N2_fix_rates for model calibration.xlsx` | N2 fixation rate data |
| `sta1ND.dates`, `sta2VM.dates` | Station date index files |

---

*This tutorial was prepared for ESTAS-AQUABC v0.2. For model equations and
scientific background, see the*
[AQUABC Reference Manual](AQUABC_Reference_Manual.md) *and*
[ESTAS Reference Manual](ESTAS_Reference_Manual.md).
