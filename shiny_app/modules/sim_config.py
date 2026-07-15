"""Simulation Config sub-tab as a Shiny module (Phase 2 fat-tab extraction).

This is *not* a standalone tab: `sim_config_ui(id)` returns the FIRST
`ui.nav_panel("Simulation Config", …)` of `panel_model_control`'s
`navset_card_tab`. Its sibling nav_panels (Run Model, Output Config) stay inline
in `panel_model_control` until Phase 4, so they remain un-namespaced. Composing
this module nav_panel into the app-level navset namespaces its inner ids to
`sim_config-*` while the siblings keep their bare ids.

`state` IS used: `save_simulation_config` bumps `state.sim_config_version`
(Phase-0 wiring), preserved here inside the module.

Cross-namespace note: the `sim_output_dir` widget lives in the `run_control`
module's "Output Config" nav_panel. The load/save handlers read and update
it, so those two touchpoints are routed through
`session.root_scope().make_scope("run_control")` (bare id `"sim_output_dir"`)
to reach `run_control-sim_output_dir` instead of a nonexistent
`sim_config-sim_output_dir`. The literal hyphenated id
`"run_control-sim_output_dir"` is NOT used — it raises `ValueError` in
`validate_id` on read, which is exactly the read-side crash this bridge form
avoids. Everything else is a verbatim port.

Self-contained: imports simulation_config and self-computes ROOT/INPUT_TXT_PATH;
imports nothing from app.py or ui_panels.
"""
import logging
import os
from datetime import date, datetime

from shiny import module, reactive, render, ui

try:
    from shiny_app.simulation_config import (
        OUTPUT_INTERVAL_PRESETS,
        TIME_STEP_PRESETS,
        SimulationConfigFile,
    )
except ImportError:  # running as a script from inside shiny_app/
    from simulation_config import (
        OUTPUT_INTERVAL_PRESETS,
        TIME_STEP_PRESETS,
        SimulationConfigFile,
    )

logger = logging.getLogger("AQUABC")
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)), "..", ".."))
# Path to INPUT.txt (in ROOT, not INPUTS)
INPUT_TXT_PATH = os.path.join(ROOT, "INPUT.txt")


@module.ui
def sim_config_ui():
    return ui.nav_panel(
        "Simulation Config",
        ui.tooltip(
            ui.input_action_button("load_sim_config", "Load Configuration", class_="btn-secondary mb-3"),
            "Load settings from INPUT.txt file"
        ),
        ui.layout_columns(
            ui.card(
                ui.card_header("Time Period"),
                ui.tooltip(
                    ui.input_numeric("sim_base_year", "Base Year:", value=1998, min=1900, max=2100),
                    "Reference year for input forcing data (e.g., meteorology files)"
                ),
                ui.tooltip(
                    ui.input_date("sim_start_date", "Start Date:", value="2015-01-01"),
                    "Simulation start date (converted to day of year)"
                ),
                ui.tooltip(
                    ui.input_date("sim_end_date", "End Date:", value="2016-01-01"),
                    "Simulation end date (converted to day of year)"
                ),
                ui.output_text("sim_duration_info"),
                fill=False
            ),
            ui.card(
                ui.card_header("Time Stepping"),
                ui.tooltip(
                    ui.input_select(
                        "sim_timestep_preset",
                        "Time Step:",
                        choices=list(TIME_STEP_PRESETS.keys()),
                        selected="6 minutes"
                    ),
                    "Preset time step intervals. 6 minutes (240 steps/day) is recommended."
                ),
                ui.tooltip(
                    ui.input_numeric("sim_timesteps_per_day", "Steps/Day:", value=240, min=1, max=1440),
                    "Number of model time steps per day. Higher values = more precision but slower."
                ),
                ui.output_text("sim_timestep_info"),
                fill=False
            ),
            ui.card(
                ui.card_header("Output Interval"),
                ui.tooltip(
                    ui.input_select(
                        "sim_output_preset",
                        "Output Frequency:",
                        choices=list(OUTPUT_INTERVAL_PRESETS.keys()),
                        selected="Hourly"
                    ),
                    "How often to write output. More frequent = larger files."
                ),
                ui.tooltip(
                    ui.input_numeric("sim_print_interval", "Print Interval (steps):", value=24, min=1),
                    "Number of time steps between output writes. 24 steps = hourly at 240 steps/day."
                ),
                ui.output_text("sim_output_info"),
                fill=False
            ),
            col_widths=[4, 4, 4]
        ),
        ui.tags.hr(),
        ui.layout_columns(
            ui.card(
                ui.card_header("Model Options"),
                ui.tooltip(
                    ui.input_switch("sim_model_sediments", "Enable Sediment Model", value=False),
                    "Enable bottom sediment diagenesis model. Increases computation time significantly."
                ),
                ui.tooltip(
                    ui.input_select(
                        "sim_resuspension",
                        "Resuspension Option:",
                        choices={"0": "Disabled", "1": "Fully Prescribed", "2": "Semi-Prescribed"},
                        selected="2"
                    ),
                    "0=No resuspension, 1=Fully prescribed rates, 2=Semi-prescribed (recommended)"
                ),
                fill=False
            ),
            col_widths=[6, 6]
        ),
        ui.layout_columns(
            ui.tooltip(
                ui.input_action_button("save_sim_config", "Save Configuration", class_="btn-success"),
                "Save current settings to INPUT.txt file"
            ),
            ui.output_text("sim_config_save_status"),
            col_widths=[3, 9]
        )
    )


@module.server
def sim_config_server(input, output, session, state):
    # Reactive values for simulation config
    sim_config_obj = reactive.Value(None)
    sim_config_save_msg = reactive.Value("")

    # `sim_output_dir` lives in the run_control module's Output Config tab;
    # reach it through session.root_scope().make_scope("run_control") + bare id.
    rc = session.root_scope().make_scope("run_control")

    @reactive.effect
    @reactive.event(input.load_sim_config)
    def load_simulation_config_file():
        """Load INPUT.txt simulation configuration"""
        if not os.path.exists(INPUT_TXT_PATH):
            logger.error(f"INPUT.txt not found: {INPUT_TXT_PATH}")
            ui.notification_show("INPUT.txt not found", type="error")
            return

        logger.info(f"Loading simulation config: {INPUT_TXT_PATH}")
        scf = SimulationConfigFile(INPUT_TXT_PATH)
        if scf.parse():
            sim_config_obj.set(scf)
            sim_config_save_msg.set("")

            # Update UI with loaded values
            cfg = scf.config

            # Base year
            ui.update_numeric("sim_base_year", value=cfg.base_year)

            # Calculate dates from days
            try:
                start_date = cfg.get_start_date()
                end_date = cfg.get_end_date()
                ui.update_date("sim_start_date", value=start_date)
                ui.update_date("sim_end_date", value=end_date)
            except Exception as e:
                logger.warning(f"Could not calculate dates: {e}")

            # Time stepping
            ui.update_numeric("sim_timesteps_per_day", value=cfg.time_steps_per_day)

            # Find matching preset for time steps
            for preset_name, preset_value in TIME_STEP_PRESETS.items():
                if preset_value == cfg.time_steps_per_day:
                    ui.update_select("sim_timestep_preset", selected=preset_name)
                    break

            # Output interval
            ui.update_numeric("sim_print_interval", value=cfg.print_interval)

            # Find matching preset for output interval
            output_hours = cfg.get_output_interval_hours()
            for preset_name, preset_value in OUTPUT_INTERVAL_PRESETS.items():
                if abs(preset_value - output_hours) < 0.1:
                    ui.update_select("sim_output_preset", selected=preset_name)
                    break

            # Model options
            ui.update_switch("sim_model_sediments", value=bool(cfg.model_sediments))
            ui.update_select("sim_resuspension", selected=str(cfg.resuspension_option))

            # Output directory (widget lives in the run_control module's tab)
            if cfg.output_folder:
                # Remove trailing slash for matching
                output_folder = cfg.output_folder.rstrip('/')
                ui.update_select("sim_output_dir", selected=output_folder, session=rc)

            logger.info(f"Loaded simulation config: {cfg.base_year}, "
                       f"days {cfg.simulation_start}-{cfg.simulation_end}")
            ui.notification_show("Configuration loaded", type="message", duration=2)
        else:
            logger.error("Failed to parse INPUT.txt")
            ui.notification_show("Failed to parse INPUT.txt", type="error")

    @reactive.effect
    @reactive.event(input.sim_timestep_preset)
    def update_timesteps_from_preset():
        """Update time steps when preset is selected"""
        preset = input.sim_timestep_preset()
        if preset in TIME_STEP_PRESETS:
            ui.update_numeric("sim_timesteps_per_day", value=TIME_STEP_PRESETS[preset])

    @reactive.effect
    @reactive.event(input.sim_output_preset)
    def update_output_from_preset():
        """Update print interval when output preset is selected"""
        preset = input.sim_output_preset()
        steps_per_day = input.sim_timesteps_per_day()

        if preset in OUTPUT_INTERVAL_PRESETS and steps_per_day:
            hours = OUTPUT_INTERVAL_PRESETS[preset]
            steps_per_hour = steps_per_day / 24.0
            print_interval = int(round(hours * steps_per_hour))
            ui.update_numeric("sim_print_interval", value=print_interval)

    @render.text
    def sim_duration_info():
        """Display simulation duration info"""
        try:
            start = input.sim_start_date()
            end = input.sim_end_date()
            base_year = input.sim_base_year()

            if start and end and base_year:
                # Convert to date objects if needed
                if isinstance(start, str):
                    start = datetime.strptime(start, "%Y-%m-%d").date()
                if isinstance(end, str):
                    end = datetime.strptime(end, "%Y-%m-%d").date()

                duration = (end - start).days
                base_date = date(base_year, 1, 1)
                start_days = (start - base_date).days
                end_days = (end - base_date).days

                return (f"Duration: {duration} days\n"
                        f"Days from base: {start_days:.0f} - {end_days:.0f}")
        except Exception as e:
            logger.debug(f"Error calculating duration: {e}")
        return "Load configuration to see details"

    @render.text
    def sim_timestep_info():
        """Display time step info"""
        try:
            steps = input.sim_timesteps_per_day()
            if steps and steps > 0:
                dt_seconds = 86400 / steps
                dt_minutes = dt_seconds / 60
                return f"dt = {dt_minutes:.1f} min ({dt_seconds:.0f} sec)"
        except Exception as e:
            logger.debug(f"Error calculating timestep: {e}")
        return ""

    @render.text
    def sim_output_info():
        """Display output interval info"""
        try:
            steps = input.sim_timesteps_per_day()
            interval = input.sim_print_interval()
            if steps and interval and steps > 0:
                hours = interval * 24.0 / steps
                if hours >= 24:
                    return f"Output every {hours/24:.1f} days"
                else:
                    return f"Output every {hours:.1f} hours"
        except Exception as e:
            logger.debug(f"Error calculating output info: {e}")
        return ""

    @reactive.effect
    @reactive.event(input.save_sim_config)
    def save_simulation_config():
        """Save simulation configuration to INPUT.txt"""
        scf = sim_config_obj.get()

        # If not loaded yet, create new from file
        if scf is None:
            if os.path.exists(INPUT_TXT_PATH):
                scf = SimulationConfigFile(INPUT_TXT_PATH)
                if not scf.parse():
                    sim_config_save_msg.set("Error: Could not parse INPUT.txt")
                    return
            else:
                sim_config_save_msg.set("Error: INPUT.txt not found")
                return

        try:
            cfg = scf.config

            # Update base year
            base_year = input.sim_base_year()
            if base_year:
                cfg.base_year = int(base_year)

            # Update dates -> convert to days
            start_date = input.sim_start_date()
            end_date = input.sim_end_date()

            if start_date:
                if isinstance(start_date, str):
                    start_date = datetime.strptime(start_date, "%Y-%m-%d").date()
                cfg.set_start_date(start_date)

            if end_date:
                if isinstance(end_date, str):
                    end_date = datetime.strptime(end_date, "%Y-%m-%d").date()
                cfg.set_end_date(end_date)

            # Time stepping
            steps = input.sim_timesteps_per_day()
            if steps:
                cfg.time_steps_per_day = int(steps)

            # Print interval
            interval = input.sim_print_interval()
            if interval:
                cfg.print_interval = int(interval)

            # Model options
            cfg.model_sediments = 1 if input.sim_model_sediments() else 0
            resuspension = input.sim_resuspension()
            if resuspension:
                cfg.resuspension_option = int(resuspension)

            # Output directory (widget lives in the run_control module's tab)
            output_dir = rc.input.sim_output_dir()
            if output_dir:
                # Ensure trailing slash for folder path
                cfg.output_folder = output_dir if output_dir.endswith('/') else output_dir + '/'

            # Validate
            is_valid, errors = cfg.validate()
            if not is_valid:
                sim_config_save_msg.set(f"Validation errors: {'; '.join(errors)}")
                ui.notification_show(f"Validation errors: {'; '.join(errors)}", type="error")
                return

            # Save
            save_ok, save_msg = scf.save(backup=True)

            if save_ok:
                sim_config_save_msg.set(f"Saved at {datetime.now().strftime('%H:%M:%S')}")
                state.sim_config_version.set(state.sim_config_version.get() + 1)
                ui.notification_show("Configuration saved successfully", type="message", duration=3)
                logger.info(f"Saved simulation config: base={cfg.base_year}, "
                           f"start={cfg.simulation_start}, end={cfg.simulation_end}")
            else:
                sim_config_save_msg.set(f"Save failed: {save_msg}")
                ui.notification_show(f"Save failed: {save_msg}", type="error")

        except Exception as e:
            logger.error(f"Error saving simulation config: {e}", exc_info=True)
            sim_config_save_msg.set(f"Error: {e}")
            ui.notification_show(f"Error: {e}", type="error")

    @render.text
    def sim_config_save_status():
        """Display save status"""
        return sim_config_save_msg.get()
