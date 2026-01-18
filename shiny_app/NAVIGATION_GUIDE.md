# AQUABC Shiny Frontend - Navigation Guide

## New Left Pane Menu Structure

The AQUABC Shiny frontend has been refactored with a clean, organized left-pane navigation menu that separates functionality into logical sections.

---

## Navigation Sections

### 1. **Dashboard** (Home)
**Purpose:** Quick overview and status information

**Features:**
- System status display
  - Working directory
  - Last model run timestamp
  - Number of output rows
  - Input file count
- Quick Actions:
  - **Quick Run** - Executes model immediately
  - **Refresh Plot** - Updates visualization

**Use Case:** Get a quick snapshot of system state and perform common actions without navigating.

---

### 2. **Model Control**
**Purpose:** Build and execute the AQUABC model

**Features:**
- **Build & Run** button
  - Compiles Fortran code
  - Builds example executable
  - Runs simulation
  - Command: `make build-lib && build-example && run-example`

- **Run Model** button
  - Runs pre-built model
  - Command: `make run-example`

- Mini run log (last 10 lines)
  - Real-time execution feedback
  - Compact view for monitoring

**Use Case:** Compile changes and execute model runs. Monitor execution progress inline.

---

### 3. **Input Files**
**Purpose:** Browse and edit model input files

**Features:**
- File browser dropdown (92 input files)
- **Refresh List** button - Updates file list
- Text editor with syntax highlighting
- **Save File** button with automatic timestamped backups
- Save status feedback

**Supported Files:**
- Configuration files (INPUT.txt, PELAGIC_INPUTS.txt)
- Parameter files (WCONST_04.txt, EXTRA_WCONST.txt)
- Initial conditions (INIT_CONC_*.txt)
- Time series (AIR_TEMP_TS.txt, CLOUD_COVER_TS.txt)
- Bathymetry files (BATHYMETRY_*.txt)

**Use Case:** Quick edits to configuration and forcing files. Backups ensure version history.

---

### 4. **Parameters** (Coming Soon)
**Purpose:** Structured editing of 318 model parameters

**Planned Features:**
- Category-based organization:
  - General (5 params)
  - Diatoms (27 params)
  - Non-fixing Cyanobacteria (22 params)
  - Fixing Cyanobacteria (24 params)
  - Other Phytoplankton (22 params)
  - Zooplankton (37 params)
  - Detritus (12 params)
  - Dissolved Organics (7 params)
  - Nitrification (6 params)
  - Redox Chemistry (52 params)
  - Methane (25 params)
  - Settling (16 params)
  - pH Effects (16 params)
  - Nostocales (23 params)

- File selection (WCONST_04.txt, EXTRA_WCONST.txt)
- **Edit Parameters** button (launches modal editor)
- Value validation
- Unit display
- Parameter search

**Use Case:** Calibration workflows, scenario testing, sensitivity analysis.

---

### 5. **Initial Conditions** (Coming Soon)
**Purpose:** Edit initial concentrations for 36 state variables

**Planned Features:**
- IC set selection (1 or 2)
- Table-based editor
- Per-box spatial editing
- Validation (non-negative, reasonable ranges)
- Scenario presets

**State Variable Groups:**
- Nutrients (NH4, NO3, PO4, Si)
- Phytoplankton (Diatoms, Cyano, OPA, etc.)
- Zooplankton (C, N, P)
- Organic matter (POC, PON, POP, DOC, DON, DOP)
- Dissolved gases (O2, CO2, CH4)
- Redox species (Fe, Mn, S)

**Use Case:** Set up different starting conditions for scenarios (pristine, eutrophic, hypoxic).

---

### 6. **Model Options** (Coming Soon)
**Purpose:** Toggle model features and switches

**Planned Features:**
- Zooplankton CNP partitioning (realistic vs. simple)
- Advanced redox simulation (Fe, Mn, S, CH4)
- Light extinction method (empirical vs. calculated)
- Cyanobacteria buoyancy simulation
- Allelopathy consideration
- Nostocales/heterocyst dynamics
- Non-obligatory N-fixers

**Use Case:** Enable/disable model components for different applications or debugging.

---

### 7. **Plot & Visualization**
**Purpose:** Configure and generate output plots

**Features:**
- Variable selection:
  - **Left axis** - Multiple variables (multi-select)
  - **Right axis** - Multiple variables (independent scale)

- Scale options:
  - Log scale for left axis
  - Log scale for right axis

- Smoothing:
  - Apply rolling mean
  - Adjustable window size (2-101 rows)

- Data controls:
  - Preview rows (10-1000)

- **Refresh Plot** button - Manual plot generation

**Available Variables (21):**
- TIME, NH4N, NO3N, PO4P, DOXY
- DIAC, ZOOC, ZOON, ZOOP
- DETC, DETN, DETP
- DOC, DON, DOP
- NOFIX_CYNC, OPA, DISSOLVED_SILICA
- PARTICULATE_SILICA, FIX_CYNC, DIC, ALKALINITY

**Use Case:** Explore model output, compare variables, identify trends and patterns.

---

### 8. **Settings**
**Purpose:** Application configuration

**Features:**
- **Appearance:**
  - Theme selector (26 Bootstrap themes)
  - **Apply Theme** button (page reload required)
  - Theme persistence across sessions

- **About:**
  - Application version
  - Model information

**Available Themes:**
- Light: default, cerulean, cosmo, flatly, journal, litera, lumen, lux, materia, minty, pulse, sandstone, simplex, spacelab, united, yeti, zephyr
- Dark: cyborg, darkly, quartz, slate, solar, superhero, vapor
- Unique: morph, sketchy

**Use Case:** Customize appearance for comfort during long calibration sessions.

---

## Main Content Area

The main content area (right side) displays:

1. **Run Log** (full view)
   - Complete execution output
   - Build messages
   - Model progress
   - Error messages

2. **Plot**
   - Interactive Plotly chart
   - Zoom, pan, hover tooltips
   - Export options (PNG, SVG)

3. **Output Preview**
   - Table view of OUTPUT.csv
   - Configurable row count
   - All state variables

---

## Navigation Tips

### Keyboard Shortcuts
- **Tab** - Navigate between inputs
- **Enter** - Activate focused button
- **Escape** - Close modals

### Workflow Recommendations

**Typical Calibration Workflow:**
1. Go to **Dashboard** - Check current status
2. Navigate to **Parameters** - Adjust growth rates
3. Switch to **Model Control** - Run simulation
4. Check **Plot & Visualization** - Review results
5. Iterate as needed

**Scenario Setup:**
1. **Initial Conditions** - Set starting state
2. **Model Options** - Enable/disable features
3. **Parameters** - Tune for scenario
4. **Model Control** - Execute
5. **Plot & Visualization** - Analyze

**Quick Edit Workflow:**
1. **Input Files** - Select file
2. Edit text directly
3. **Save File** (auto-backup)
4. **Dashboard** → **Quick Run**
5. **Dashboard** → **Refresh Plot**

---

## Advantages of New Structure

### Organization
- **Logical grouping** - Related functions together
- **Reduced clutter** - Sidebar no longer overcrowded
- **Clear hierarchy** - Easy to find features

### Usability
- **Context preservation** - Main content stays visible
- **Quick access** - Jump between sections instantly
- **Status awareness** - Dashboard shows system state

### Scalability
- **Room for growth** - Easy to add new sections
- **Modular design** - Each section is self-contained
- **Placeholder sections** - Clear roadmap for features

### Efficiency
- **Fewer clicks** - Quick actions on dashboard
- **Parallel workflows** - Edit files while monitoring runs
- **Visual feedback** - Status indicators throughout

---

## Future Enhancements

Planned additions to navigation:

1. **Analysis** section
   - Mass balance checking
   - Sensitivity analysis
   - Multi-run comparison

2. **Validation** section
   - Observation upload
   - Model-data comparison
   - Goodness-of-fit metrics

3. **Scenarios** section
   - Preset configurations
   - Scenario comparison
   - Batch execution

4. **Help** section
   - Parameter documentation
   - Model equations
   - Troubleshooting guide

---

## Technical Details

**Implementation:**
- Uses Shiny `ui.navset_pill_list()` for vertical navigation
- Responsive layout with `ui.layout_sidebar()`
- Fixed sidebar width (300px) for consistent navigation
- Main content area adjusts dynamically

**Theme Support:**
- 26 Bootstrap themes via shinyswatch
- Theme persistence using `.aquabc_theme` file
- JavaScript-based page reload for theme application

**State Management:**
- Reactive values for dynamic updates
- Event handlers for user actions
- Debounced plot updates for performance

---

## Getting Started

To launch the application:

```bash
# From repository root
source .venv/bin/activate
shiny run --reload --port 5001 shiny_app.app:app
```

Then navigate to: http://localhost:5001

**First Steps:**
1. Start on **Dashboard** to see system status
2. Check **Model Control** to verify last run
3. Explore **Plot & Visualization** to see output
4. Try **Settings** to customize theme

---

## Support

For issues or questions:
- GitHub Issues: https://github.com/anthropics/aquabc/issues
- Documentation: See AQUABC_FRONTEND_ANALYSIS_AND_PLAN.md
- Parameter Reference: See PARAMETER_REFERENCE.md

---

**Last Updated:** 2026-01-16
**Version:** 0.2
