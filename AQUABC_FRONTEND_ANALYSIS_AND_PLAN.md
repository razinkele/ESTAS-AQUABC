# AQUABC v0.2 - Codebase Analysis and Frontend Implementation Plan

**Date:** 2026-01-16
**Analysis by:** Claude Sonnet 4.5

---

## Executive Summary

AQUABC is a sophisticated **aquatic biogeochemical model** written in Fortran 90 that simulates water quality dynamics in aquatic ecosystems. It models complex interactions between:
- Multiple phytoplankton groups (diatoms, cyanobacteria, other algae)
- Zooplankton
- Nutrients (nitrogen, phosphorus, silica)
- Dissolved oxygen and carbon cycling
- Sediment interactions
- Redox processes (iron, manganese, sulfur)

The model is currently controlled via text input files. The Python Shiny frontend provides basic functionality but lacks comprehensive parameter control capabilities.

---

## 1. Codebase Structure Analysis

### 1.1 Directory Organization

```
AQUABC v0.2/
├── SOURCE_CODE/
│   ├── AQUABC/
│   │   ├── PELAGIC/              # Water column biogeochemistry
│   │   │   ├── AQUABC_PELAGIC_LIBRARY/  # Process libraries
│   │   │   └── aquabc_II_pelagic_*.f90  # Core pelagic model
│   │   ├── SEDIMENTS/            # Sediment processes
│   │   └── AQUABC_EXAMPLES/
│   │       └── AQUABC_PELAGIC_0D/  # 0D example (single box)
│   ├── ESTAS/                    # Model framework/driver
│   ├── ALLELOPATHY/              # Allelopathic interactions
│   ├── MACROALGAE/               # Macroalgae module
│   └── CORE_UTILS/               # Utility functions
├── INPUTS/                       # 92 input text files
├── OUTPUTS/                      # Model outputs
├── shiny_app/                    # Python Shiny frontend
└── Makefile                      # Build system
```

### 1.2 Fortran Code Architecture

**Key Modules:**

1. **mod_AQUABC_II_GLOBAL.f90** - Global model constants and parameters
2. **aquabc_II_pelagic_interface.f90** - Main pelagic model interface
3. **aquabc_II_pelagic_model_constants.f90** - Parameter definitions
4. **AQUABC_PELAGIC_LIBRARY/** - Process-specific subroutines:
   - Phytoplankton growth (diatoms, cyanobacteria, other algae)
   - Zooplankton grazing
   - Organic matter mineralization
   - Nutrient cycling
   - Dissolved oxygen dynamics
   - pH and carbonate system

**Model Structure:**
- 0D box model (can be extended to multiple boxes/3D)
- Time-stepping integration with configurable time step
- Process-based formulations (Monod kinetics, temperature corrections)
- Comprehensive stoichiometric tracking (C:N:P ratios)

### 1.3 Input File Structure

#### Main Configuration Files:

1. **INPUT.txt** - Master control file
   - Simulation period (start/end dates)
   - Time step configuration
   - Input/output folders
   - Model options

2. **PELAGIC_INPUTS.txt** - Pelagic model configuration
   - 36 state variables defined
   - 318 model constants
   - 25 pelagic boxes/basins
   - Initial conditions
   - Boundary conditions
   - Forcing functions (19 time series)

3. **PELAGIC_MODEL_OPTIONS.txt** - Model switches
   - Zooplankton partitioning option
   - Advanced redox simulation
   - Light extinction calculation method
   - Cyanobacteria buoyancy
   - Allelopathy modeling

4. **WCONST_04.txt** - Model parameters (318 constants)
   - Growth rates for all phytoplankton groups
   - Temperature coefficients
   - Half-saturation constants
   - Mortality rates
   - Stoichiometric ratios
   - Mineralization rates
   - Nitrification parameters
   - Redox process rates

#### Parameter Categories in WCONST_04.txt:

| Category | Parameters | Count | Line Range |
|----------|-----------|-------|------------|
| **General** | Aeration, light extinction | ~5 | 1-5 |
| **Diatoms** | Growth, respiration, mortality, nutrients | ~27 | 5-31 |
| **Non-fixing Cyanobacteria** | Growth, respiration, mortality | ~22 | 29-50 |
| **Fixing Cyanobacteria** | Growth, N-fixation, mortality | ~24 | 51-74 |
| **Other Phytoplankton (OPA)** | Growth, respiration, mortality | ~22 | 75-96 |
| **Zooplankton** | Grazing, preferences, mortality | ~37 | 97-133 |
| **Detritus (POC, PON, POP)** | Dissolution rates | ~12 | 134-145 |
| **Dissolved Organics (DOC, DON, DOP)** | Mineralization | ~7 | 146-152 |
| **Nitrification** | NH4 oxidation | ~6 | 152-157 |
| **Redox Chemistry** | Fe, Mn, S cycling | ~50 | 158-207 |
| **DOC Mineralization** | Electron acceptors (O2, NO3, Fe, Mn, S) | ~46 | 164-209 |
| **Methane** | Production/oxidation | ~25 | 210-234 |
| **Settling** | Particle settling velocities | ~16 | 235-250 |
| **pH Effects** | pH correction factors | ~16 | 251-266 |
| **Dissolved Metals** | Fe, Mn dissolution | ~10 | 267-276 |
| **Nostocales** | Heterocystous cyanobacteria | ~23 | 276-298 |

#### Additional Input Files:

- **INIT_CONC_*.txt** - Initial concentrations (36 state variables)
- **BATHYMETRY_*.txt** - Basin depths and volumes
- **AIR_TEMP_TS.txt** - Air temperature time series (~32 MB)
- **CLOUD_COVER_TS.txt** - Solar radiation
- **W_SED_CONST.txt** - Sediment parameters
- **ALLELOPATHIC_INFORMATION.txt** - Species interactions

### 1.4 State Variables (36 total)

**Nutrients:**
1. NH4_N - Ammonium nitrogen
2. NO3_N - Nitrate nitrogen
3. PO4_P - Phosphate phosphorus
4. DISS_Si - Dissolved silica
5. PART_Si - Particulate silica

**Dissolved Gases:**
6. DISS_OXYGEN - Dissolved oxygen
20. INORG_C - Dissolved inorganic carbon
30. CH4_C - Methane carbon

**Phytoplankton (C biomass):**
5. DIA_C - Diatoms
15. CYN_C - Non-fixing cyanobacteria
16. OPA_C - Other phytoplankton
19. FIX_CYN_C - Nitrogen-fixing cyanobacteria
31. NOST_VEG_HET_C - Nostocales (vegetative + heterocysts)
32. AKI_C - Akinetes (resting stages)

**Zooplankton:**
6. ZOO_C - Zooplankton carbon
7. ZOO_N - Zooplankton nitrogen
8. ZOO_P - Zooplankton phosphorus

**Organic Matter:**
9-11. DET_PART_ORG_C/N/P - Particulate detritus
12-14. DISS_ORG_C/N/P - Dissolved organics

**Carbonate System:**
21. TOT_ALK - Total alkalinity

**Metals/Redox:**
22. FE_II - Ferrous iron
23. FE_III - Ferric iron
24. MN_II - Manganous manganese
25. MN_IV - Manganic manganese
26. CA - Calcium
27. MG - Magnesium
28. S_PLUS_6 - Sulfate
29. S_MINUS_2 - Sulfide

**Allelopathy:**
33-36. SEC_METAB_* - Secondary metabolites

### 1.5 Output Structure

**OUTPUT.csv** contains:
- TIME column
- All 36 state variables
- Chlorophyll-a calculated from phytoplankton C
- Generated at intervals defined by PRINT_INTERVAL
- Current example: 929 rows (3 years at ~24 hour intervals)

---

## 2. Current Shiny Frontend Analysis

### 2.1 Implemented Features

**✓ Model Execution:**
- "Build & Run" button (make build-lib && build-example && run-example)
- "Run" button (make run-example only)
- Real-time log streaming

**✓ File Management:**
- Browse/edit any file in INPUTS/ directory
- Text editor with syntax highlighting
- Save with timestamped backups
- File list refresh

**✓ Visualization:**
- Interactive Plotly charts
- Dual y-axis support
- Variable selection (left/right axis)
- Log scale options
- Rolling mean smoothing
- Manual plot refresh
- Output table preview

**✓ UI Enhancements:**
- 26 Bootstrap themes via shinyswatch
- Theme persistence
- Responsive layout
- Comprehensive logging

### 2.2 Missing/Limited Features

**✗ Parameter Control:**
- No structured parameter editing
- No parameter categories/grouping
- No validation or range checking
- No parameter presets/scenarios

**✗ Initial Conditions:**
- No visual IC editor
- No spatial distribution editing

**✗ Model Options:**
- Binary switches in PELAGIC_MODEL_OPTIONS.txt not exposed
- No simulation period controls

**✗ Advanced Analysis:**
- No mass balance checking
- No sensitivity analysis tools
- No parameter comparison
- No statistical summaries

**✗ Time Series Forcing:**
- No editing of large time series files (AIR_TEMP_TS.txt is 32 MB)
- No temporal interpolation tools

**✗ Validation:**
- No comparison with observations
- No calibration tools
- No goodness-of-fit metrics

---

## 3. Implementation Plan: Enhanced Parameter Control

### Phase 1: Parameter File Parser and Editor (Priority: HIGH)

#### 3.1.1 Backend Functionality

**Create `shiny_app/parameter_parser.py`:**

```python
class ParameterFile:
    """Parse and manage AQUABC parameter files"""

    def __init__(self, filepath):
        self.filepath = filepath
        self.parameters = []  # List of Parameter objects
        self.comments = {}

    def parse(self):
        """Parse WCONST_04.txt format"""
        # Handle: "ID  NAME  VALUE  !COMMENT" format

    def validate(self, param_name, value):
        """Validate parameter value ranges"""

    def update_parameter(self, param_id, new_value):
        """Update single parameter"""

    def save(self, backup=True):
        """Save with optional backup"""

class Parameter:
    """Individual parameter with metadata"""

    def __init__(self, id, name, value, comment, category):
        self.id = id
        self.name = name
        self.value = float(value)
        self.comment = comment
        self.category = category
        self.min_value = None  # Optional constraints
        self.max_value = None
        self.units = self._parse_units()

    def _parse_units(self):
        """Extract units from comment"""
```

**Parameter Categories Definition:**

```python
PARAMETER_CATEGORIES = {
    "General": (1, 5),
    "Diatoms": (5, 31),
    "Non-fixing Cyanobacteria": (29, 50),
    "Fixing Cyanobacteria": (51, 74),
    "Other Phytoplankton": (75, 96),
    "Zooplankton": (97, 133),
    "Detritus": (134, 145),
    "Dissolved Organics": (146, 152),
    "Nitrification": (152, 157),
    "Redox Chemistry": (158, 207),
    "Methane": (210, 234),
    "Settling": (235, 250),
    "pH Effects": (251, 266),
    "Nostocales": (276, 298),
}
```

#### 3.1.2 UI Components

**Add to sidebar:**

```python
ui.h4("Parameters"),
ui.input_select("param_category", "Category:",
                choices=list(PARAMETER_CATEGORIES.keys())),
ui.input_select("param_file", "Constants File:",
                choices=["WCONST_04.txt", "EXTRA_WCONST.txt"]),
ui.input_action_button("edit_params", "Edit Parameters",
                      class_="btn-primary w-100"),
```

**Modal Dialog for Parameter Editing:**

```python
@reactive.effect
@reactive.event(input.edit_params)
def show_param_editor():
    category = input.param_category()
    params = load_category_parameters(category)

    ui.modal_show(
        ui.modal(
            ui.h4(f"Edit {category} Parameters"),
            # Generate input fields dynamically
            *[
                ui.row(
                    ui.column(6, ui.tags.label(p.name)),
                    ui.column(3, ui.input_numeric(f"param_{p.id}", "", p.value)),
                    ui.column(3, ui.tags.small(p.comment))
                )
                for p in params
            ],
            ui.input_action_button("save_params", "Save Changes"),
            ui.input_action_button("cancel_params", "Cancel"),
            size="xl",
            easy_close=False
        )
    )
```

### Phase 2: Initial Conditions Editor (Priority: HIGH)

#### 3.2.1 Spatial IC Editor

```python
ui.h4("Initial Conditions"),
ui.input_select("ic_set", "IC Set:", choices=["1", "2"]),
ui.input_action_button("edit_ics", "Edit Initial Conditions"),

# In modal:
- Table view of all 36 state variables
- Input fields for each variable
- Apply to all boxes or per-box editing
- Validation (non-negative, reasonable ranges)
```

### Phase 3: Model Options Controller (Priority: MEDIUM)

#### 3.3.1 Expose Model Switches

**Read/write PELAGIC_MODEL_OPTIONS.txt:**

```python
ui.h4("Model Options"),
ui.input_switch("zooplankton_option",
                "Realistic Zooplankton CNP Partitioning", True),
ui.input_switch("advanced_redox",
                "Advanced Redox Simulation (Mn, Fe, SO4, CH4)", False),
ui.input_select("light_extinction",
                "Light Extinction Method:",
                choices=["Curonian-specific empirical",
                         "Calculated from Chl-a"]),
ui.input_switch("cyano_buoyancy",
                "Cyanobacteria Buoyancy Simulation", True),
ui.input_switch("consider_allelopathy",
                "Consider Allelopathy", True),
```

### Phase 4: Simulation Configuration (Priority: MEDIUM)

#### 3.4.1 Time Controls

```python
ui.h4("Simulation Setup"),
ui.input_date("sim_start", "Start Date", value=date(1998, 1, 1)),
ui.input_date("sim_end", "End Date", value=date(1999, 1, 1)),
ui.input_numeric("time_steps_per_day", "Time Steps/Day:", 240,
                 min=24, max=1440),
ui.input_numeric("print_interval", "Output Interval (steps):", 24),
```

Automatically update INPUT.txt with proper date conversions.

### Phase 5: Advanced Features (Priority: LOW)

#### 3.5.1 Parameter Presets/Scenarios

```python
ui.h4("Scenarios"),
ui.input_select("scenario", "Load Scenario:",
                choices=["Default", "High Nutrient", "Hypoxic",
                         "Algal Bloom", "Custom"]),
ui.input_action_button("save_scenario", "Save Current as Scenario"),
```

Store scenarios as JSON:

```json
{
  "name": "High Nutrient",
  "description": "Elevated N and P loading",
  "parameters": {
    "NH4_N_IC": 0.15,
    "PO4_P_IC": 0.08,
    "KG_DIA_OPT_TEMP": 4.2
  }
}
```

#### 3.5.2 Sensitivity Analysis

```python
ui.h4("Sensitivity Analysis"),
ui.input_select("sa_parameter", "Parameter:", choices=param_list),
ui.input_slider("sa_range", "Range (%):", min=-50, max=50, value=[-20, 20]),
ui.input_numeric("sa_steps", "Number of Runs:", 5),
ui.input_action_button("run_sensitivity", "Run Analysis"),
```

Run multiple simulations with parameter perturbations, plot responses.

#### 3.5.3 Mass Balance Diagnostics

```python
ui.h4("Mass Balance"),
ui.input_action_button("check_mass_balance", "Check Mass Balance"),
output_widget("mass_balance_plot"),  # C, N, P budgets over time
```

Calculate and display:
- Total C, N, P in system
- Inputs/outputs
- Conservation checks

#### 3.5.4 Comparison with Observations

```python
ui.h4("Model Validation"),
ui.input_file("obs_data", "Upload Observations (.csv)"),
ui.input_selectize("obs_vars", "Variables to Compare:",
                   choices=state_var_list, multiple=True),
output_widget("validation_plot"),  # Observed vs. Modeled
ui.output_text("statistics"),  # R², RMSE, bias
```

### Phase 6: Time Series Editor (Priority: LOW)

#### 3.6.1 Large File Handling

```python
ui.h4("Forcing Functions"),
ui.input_select("forcing_var", "Variable:",
                choices=["Air Temperature", "Solar Radiation",
                         "Wind Speed", "Cloud Cover"]),
output_widget("forcing_plot"),  # Plot time series
ui.input_action_button("edit_forcing", "Edit Time Series"),
```

For large files (32 MB AIR_TEMP_TS.txt):
- Load/plot subset of data
- Allow interpolation
- Support data import/export

---

## 4. Implementation Roadmap

### Sprint 1 (Week 1): Parameter Editor Core
- [ ] Create `parameter_parser.py` module
- [ ] Implement WCONST_04.txt parser
- [ ] Build parameter category selector
- [ ] Create modal parameter editor
- [ ] Add save/backup functionality
- [ ] Test with all 318 parameters

### Sprint 2 (Week 2): Initial Conditions & Model Options
- [ ] Build IC file parser
- [ ] Create IC table editor
- [ ] Implement per-box IC editing
- [ ] Add MODEL_OPTIONS.txt controller
- [ ] Expose all binary switches
- [ ] Link switches to UI

### Sprint 3 (Week 3): Simulation Configuration
- [ ] Add date picker for simulation period
- [ ] Implement INPUT.txt editor
- [ ] Add time step controls
- [ ] Create output interval selector
- [ ] Add base year configuration

### Sprint 4 (Week 4): Validation & Polish
- [ ] Add parameter validation
- [ ] Implement range checking
- [ ] Create help tooltips
- [ ] Add parameter search
- [ ] Improve error handling
- [ ] Documentation

### Future Enhancements:
- [ ] Parameter presets/scenarios
- [ ] Sensitivity analysis
- [ ] Mass balance checks
- [ ] Observation comparison
- [ ] Time series editor
- [ ] Multi-scenario comparison
- [ ] Batch run capability

---

## 5. Technical Considerations

### 5.1 Data Management

**File Locking:**
- Implement file locking during edits
- Prevent concurrent modifications
- Save queue for multiple users

**Backup Strategy:**
- Timestamped backups for all changes
- Git integration for version control
- Rollback capability

### 5.2 Performance

**Large File Handling:**
- Chunk reading for 32 MB time series files
- Lazy loading of parameter categories
- Caching parsed structures

**Reactive Updates:**
- Debounce parameter changes
- Batch updates to files
- Asynchronous file I/O

### 5.3 Validation

**Parameter Constraints:**
- Physical bounds (e.g., growth rates > 0)
- Stoichiometric ratios (C:N:P relationships)
- Temperature coefficients (theta > 1)
- Cross-parameter dependencies

**User Guidance:**
- Hover tooltips with parameter descriptions
- Units display
- Typical value ranges
- Warning for extreme values

### 5.4 User Experience

**Workflow:**
1. Load existing parameter set
2. Browse by category
3. Edit parameters with visual feedback
4. Validate changes
5. Save with auto-backup
6. Run model
7. Compare results

**Undo/Redo:**
- Parameter change history
- Restore previous values
- Compare before/after

---

## 6. Documentation Needs

### 6.1 User Documentation
- Parameter descriptions (expand from comments)
- Example scenarios
- Best practices for calibration
- Troubleshooting guide

### 6.2 Developer Documentation
- File format specifications
- Parameter interdependencies
- Model physics/equations
- Code structure documentation

---

## 7. Testing Strategy

### 7.1 Unit Tests
- Parameter file parsing
- Value validation
- File I/O operations
- Backup/restore

### 7.2 Integration Tests
- Full parameter edit workflow
- IC modification and save
- Model execution after edits
- Theme persistence

### 7.3 Regression Tests
- Compare model outputs
- Ensure parameter changes propagate correctly
- Verify numerical stability

---

## 8. Conclusion

The AQUABC model is a sophisticated water quality simulation tool with:
- **318 calibratable parameters** across multiple functional groups
- **36 state variables** tracking C, N, P, O2, metals
- **Complex biogeochemical processes** (growth, mortality, mineralization, redox)
- **Modular structure** allowing easy extension

The proposed Shiny frontend enhancements will:
1. **Democratize access** to model parameters (no more raw text editing)
2. **Reduce errors** through validation and structured inputs
3. **Accelerate calibration** with organized parameter categories
4. **Enable scenario testing** with presets and comparisons
5. **Improve reproducibility** with version control and backups

**Priority Order:**
1. Parameter editor (WCONST files) - enables calibration
2. Initial conditions editor - essential for scenarios
3. Model options controller - toggles key features
4. Simulation configuration - run control
5. Advanced features - analysis and validation tools

This plan provides a roadmap from basic parameter control to advanced model management and analysis capabilities.

---

**Next Steps:**
1. Review and approve this plan
2. Set up development environment
3. Begin Sprint 1: Parameter Editor Core
4. Iterate based on user feedback
