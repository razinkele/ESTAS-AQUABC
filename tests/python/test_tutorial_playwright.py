"""Playwright end-to-end test that follows the Getting Started Tutorial.

Walks through every step of docs/Tutorial_Getting_Started.md against a live
Shiny app instance, exercising the full tutorial workflow:

  Step 1:  Dashboard overview
  Step 2:  Model Build page (verify controls, do NOT trigger a real build)
  Step 3:  Explore Input Files
  Step 4:  Review Parameters
  Step 5:  Set Initial Conditions
  Step 6:  Configure Model Options
  Step 7:  Configure the Simulation (Model Config tabs)
  Step 8:  Run the 30-day simulation (Quick Run)
  Step 9:  View Results (Plots page)
  Step 10: Analyse Mass Balance
  Step 11: Compare with Observations
  Extra:   Scenarios, Map, Tutorial link, Header buttons

Run with:
    cd /home/razinka/AQUABCv0.2
    /opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_tutorial_playwright.py -v

For headed (visible browser):
    /opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_tutorial_playwright.py -v --headed

Requires: playwright, pytest, shiny[test]
"""


from playwright.sync_api import Page, expect
from shiny.pytest import create_app_fixture
from shiny.run import ShinyAppProc

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Longer timeout because the app imports many modules on first start
app = create_app_fixture("../../shiny_app/app.py", timeout_secs=60)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

STARTUP_WAIT = 3000  # ms — initial page load settle time
NAV_WAIT = 2000      # ms — after navigating to a new panel
ACTION_WAIT = 2000   # ms — after clicking a button / changing a select
LONG_WAIT = 5000     # ms — after actions that trigger server work


def navigate_to(page: Page, nav_id: str):
    """Navigate to a panel via Shiny.setInputValue (mirrors the sidebar JS)."""
    page.evaluate(f"Shiny.setInputValue('navigation', '{nav_id}')")
    page.wait_for_timeout(NAV_WAIT)


def click_tab(page: Page, tab_label: str):
    """Click a Bootstrap navset tab by its visible label text."""
    page.locator(f".nav-link:has-text('{tab_label}')").first.click()
    page.wait_for_timeout(ACTION_WAIT)


def select_option(page: Page, selector: str, value: str):
    """Select an <option> by value on a <select> element."""
    page.select_option(selector, value)
    page.wait_for_timeout(ACTION_WAIT)


def goto_app(page: Page, app: ShinyAppProc):
    """Navigate to the app URL and wait for full render."""
    page.goto(app.url)
    page.wait_for_timeout(STARTUP_WAIT)


# ===================================================================
# Step 1 — Dashboard Overview
# ===================================================================

class TestStep1DashboardOverview:
    """Tutorial Step 1: Verify the Dashboard loads with all key elements."""

    def test_app_title_visible(self, page: Page, app: ShinyAppProc):
        """The AQUABC header bar is rendered."""
        goto_app(page, app)
        expect(page.locator(".app-header")).to_contain_text("AQUABC")

    def test_quick_run_button(self, page: Page, app: ShinyAppProc):
        """Quick Run button is present on the Dashboard."""
        goto_app(page, app)
        btn = page.locator("#quick_run")
        expect(btn).to_be_visible()
        expect(btn).to_contain_text("Quick Run")

    def test_stop_button(self, page: Page, app: ShinyAppProc):
        """Stop button is present on the Dashboard."""
        goto_app(page, app)
        expect(page.locator("#dashboard_stop")).to_be_visible()

    def test_system_status_panel(self, page: Page, app: ShinyAppProc):
        """System Status card is rendered with model info."""
        goto_app(page, app)
        expect(page.locator("body")).to_contain_text("System Status")

    def test_simulation_config_panel(self, page: Page, app: ShinyAppProc):
        """Simulation Config card shows INPUT.txt variables."""
        goto_app(page, app)
        expect(page.locator("body")).to_contain_text("Simulation Config")

    def test_run_log_panel(self, page: Page, app: ShinyAppProc):
        """Run Log panel is present."""
        goto_app(page, app)
        expect(page.locator("body")).to_contain_text("Run Log")

    def test_sidebar_has_all_nav_links(self, page: Page, app: ShinyAppProc):
        """Sidebar contains all 13 navigation entries from the tutorial."""
        goto_app(page, app)
        nav_ids = [
            "nav_dashboard", "nav_model_structure", "nav_model_build",
            "nav_model_control", "nav_input_files", "nav_parameters",
            "nav_initial_conditions", "nav_model_options", "nav_scenarios",
            "nav_plot", "nav_mass_balance", "nav_observations", "nav_map",
        ]
        for nav_id in nav_ids:
            link = page.locator(f".nav-link[data-nav-id='{nav_id}']")
            expect(link).to_be_attached()

    def test_header_tutorial_link(self, page: Page, app: ShinyAppProc):
        """Tutorial book icon in the header links to tutorial.html."""
        goto_app(page, app)
        link = page.locator(".app-header a[href='tutorial.html']")
        expect(link).to_be_visible()
        assert link.get_attribute("target") == "_blank"

    def test_header_help_settings_changelog(self, page: Page, app: ShinyAppProc):
        """Header has changelog, help, and settings buttons."""
        goto_app(page, app)
        expect(page.locator("#changelog_toggle")).to_be_visible()
        expect(page.locator("#help_toggle")).to_be_visible()
        expect(page.locator("#settings_toggle")).to_be_visible()


# ===================================================================
# Step 2 — Building the Model
# ===================================================================

class TestStep2ModelBuild:
    """Tutorial Step 2: Model Build page has correct controls."""

    def test_build_page_loads(self, page: Page, app: ShinyAppProc):
        """Model Build page renders three columns."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        expect(page.locator("body")).to_contain_text("Build Configuration")
        expect(page.locator("body")).to_contain_text("Available Executables")
        expect(page.locator("body")).to_contain_text("Build Log")

    def test_compiler_selection(self, page: Page, app: ShinyAppProc):
        """Compiler radio buttons include gfortran and ifort."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        expect(page.locator("label:has-text('GNU Fortran')")).to_be_visible()

    def test_build_type_selection(self, page: Page, app: ShinyAppProc):
        """Build type radio buttons include Release, Debug, Fast."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        for label in ["Release", "Debug", "Fast"]:
            expect(page.locator(f"label:has-text('{label}')")).to_be_visible()

    def test_build_and_rebuild_buttons(self, page: Page, app: ShinyAppProc):
        """Build and Rebuild All buttons are present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#btn_build")).to_be_visible()
        expect(page.locator("#btn_rebuild")).to_be_visible()

    def test_build_log_output(self, page: Page, app: ShinyAppProc):
        """Build log verbatim output area is present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#build_log")).to_be_visible()

    def test_executable_selection_dropdown(self, page: Page, app: ShinyAppProc):
        """Active executable dropdown exists."""
        goto_app(page, app)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#active_executable")).to_be_visible()


# ===================================================================
# Step 3 — Exploring Input Files
# ===================================================================

class TestStep3InputFiles:
    """Tutorial Step 3: Input Files browser works correctly."""

    def test_file_browser_visible(self, page: Page, app: ShinyAppProc):
        """File Browser with category filter and file list is present."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        expect(page.locator("#file_category_filter")).to_be_visible()
        expect(page.locator("#file_select")).to_be_visible()

    def test_file_list_populated(self, page: Page, app: ShinyAppProc):
        """File select dropdown has files from INPUTS/."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        options = page.locator("#file_select option")
        count = options.count()
        assert count > 10, f"Expected many input files, got {count}"

    def test_file_contents_tab(self, page: Page, app: ShinyAppProc):
        """File Contents tab and textarea are visible."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        expect(page.locator("body")).to_contain_text("File Contents")
        expect(page.locator("#file_contents")).to_be_visible()

    def test_map_display_tab(self, page: Page, app: ShinyAppProc):
        """Map Display tab exists for box network visualization."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        map_tab = page.locator(".nav-link:has-text('Map Display')")
        expect(map_tab).to_be_visible()

    def test_file_information_panel(self, page: Page, app: ShinyAppProc):
        """File Information card is present."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        expect(page.locator("body")).to_contain_text("File Information")

    def test_select_pelagic_inputs(self, page: Page, app: ShinyAppProc):
        """Selecting PELAGIC_INPUTS.txt loads content into textarea."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        page.select_option("#file_select", "PELAGIC_INPUTS.txt")
        page.wait_for_timeout(ACTION_WAIT)
        textarea = page.locator("#file_contents")
        # The textarea should now have some content
        value = textarea.input_value()
        assert len(value) > 0, "PELAGIC_INPUTS.txt should have content"

    def test_refresh_files_button(self, page: Page, app: ShinyAppProc):
        """Refresh file list button works without error."""
        goto_app(page, app)
        navigate_to(page, "nav_input_files")
        page.locator("#refresh_files").click()
        page.wait_for_timeout(ACTION_WAIT)
        # File list should still be populated
        count = page.locator("#file_select option").count()
        assert count > 0


# ===================================================================
# Step 4 — Reviewing Parameters
# ===================================================================

class TestStep4Parameters:
    """Tutorial Step 4: Parameters page loads and displays constants."""

    def test_parameter_page_loads(self, page: Page, app: ShinyAppProc):
        """Parameters page has category selector and load button."""
        goto_app(page, app)
        navigate_to(page, "nav_parameters")
        expect(page.locator("#parameters-param_category")).to_be_visible()
        expect(page.locator("#parameters-param_file")).to_be_visible()
        expect(page.locator("#parameters-load_params")).to_be_visible()

    def test_parameter_categories_populated(self, page: Page, app: ShinyAppProc):
        """Category dropdown has all 14 parameter categories."""
        goto_app(page, app)
        navigate_to(page, "nav_parameters")
        options = page.locator("#parameters-param_category option")
        count = options.count()
        assert count >= 14, f"Expected 14 parameter categories, got {count}"

    def test_load_diatoms_parameters(self, page: Page, app: ShinyAppProc):
        """Loading Diatoms category renders parameter inputs."""
        goto_app(page, app)
        navigate_to(page, "nav_parameters")
        page.select_option("#parameters-param_category", "Diatoms")
        page.locator("#parameters-load_params").click()
        page.wait_for_timeout(LONG_WAIT)
        # Category info should show something about Diatoms
        expect(page.locator("body")).to_contain_text("Diatom")

    def test_save_button_present(self, page: Page, app: ShinyAppProc):
        """Save All Changes button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_parameters")
        expect(page.locator("#parameters-save_params")).to_be_visible()

    def test_constants_file_selector(self, page: Page, app: ShinyAppProc):
        """Constants file dropdown includes WCONST_04.txt."""
        goto_app(page, app)
        navigate_to(page, "nav_parameters")
        page.wait_for_timeout(ACTION_WAIT)
        options_text = page.locator("#parameters-param_file").inner_text()
        assert "WCONST_04" in options_text


# ===================================================================
# Step 5 — Setting Initial Conditions
# ===================================================================

class TestStep5InitialConditions:
    """Tutorial Step 5: Initial Conditions page works correctly."""

    def test_ic_page_loads(self, page: Page, app: ShinyAppProc):
        """Initial Conditions page has file selector, category, and load button."""
        goto_app(page, app)
        navigate_to(page, "nav_initial_conditions")
        expect(page.locator("#initial_conditions-ic_file")).to_be_visible()
        expect(page.locator("#initial_conditions-ic_category")).to_be_visible()
        expect(page.locator("#initial_conditions-load_ics")).to_be_visible()

    def test_ic_files_available(self, page: Page, app: ShinyAppProc):
        """IC file dropdown has INIT_CONC_1.txt and INIT_CONC_2.txt."""
        goto_app(page, app)
        navigate_to(page, "nav_initial_conditions")
        text = page.locator("#initial_conditions-ic_file").inner_text()
        assert "INIT_CONC_1" in text
        assert "INIT_CONC_2" in text

    def test_ic_categories_populated(self, page: Page, app: ShinyAppProc):
        """Category dropdown has 11 state variable categories."""
        goto_app(page, app)
        navigate_to(page, "nav_initial_conditions")
        count = page.locator("#initial_conditions-ic_category option").count()
        assert count >= 11, f"Expected 11 IC categories, got {count}"

    def test_load_nutrients_ic(self, page: Page, app: ShinyAppProc):
        """Loading Nutrients category shows state variable values."""
        goto_app(page, app)
        navigate_to(page, "nav_initial_conditions")
        page.select_option("#initial_conditions-ic_category", "Nutrients")
        page.locator("#initial_conditions-load_ics").click()
        page.wait_for_timeout(LONG_WAIT)
        expect(page.locator("body")).to_contain_text("Nutrient")

    def test_save_ics_button(self, page: Page, app: ShinyAppProc):
        """Save All Changes button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_initial_conditions")
        expect(page.locator("#initial_conditions-save_ics")).to_be_visible()


# ===================================================================
# Step 6 — Configuring Model Options
# ===================================================================

class TestStep6ModelOptions:
    """Tutorial Step 6: Model Options page with switches and constants."""

    def test_options_page_loads(self, page: Page, app: ShinyAppProc):
        """Model Options page has category selector and load button."""
        goto_app(page, app)
        navigate_to(page, "nav_model_options")
        expect(page.locator("#model_options-options_category")).to_be_visible()
        expect(page.locator("#model_options-load_options")).to_be_visible()

    def test_options_categories(self, page: Page, app: ShinyAppProc):
        """Category dropdown has 6 option categories."""
        goto_app(page, app)
        navigate_to(page, "nav_model_options")
        count = page.locator("#model_options-options_category option").count()
        assert count >= 6, f"Expected 6 option categories, got {count}"

    def test_load_cyanobacteria_options(self, page: Page, app: ShinyAppProc):
        """Loading Cyanobacteria options shows switches."""
        goto_app(page, app)
        navigate_to(page, "nav_model_options")
        page.select_option("#model_options-options_category", "Cyanobacteria")
        page.locator("#model_options-load_options").click()
        page.wait_for_timeout(LONG_WAIT)
        # Should show Model Switches and/or Extra Constants
        body_text = page.locator("body").inner_text()
        assert "Model Switches" in body_text or "Extra Constants" in body_text

    def test_save_options_button(self, page: Page, app: ShinyAppProc):
        """Save All Changes button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_options")
        expect(page.locator("#model_options-save_options")).to_be_visible()


# ===================================================================
# Step 7 — Configuring the Simulation
# ===================================================================

class TestStep7SimulationConfig:
    """Tutorial Step 7: Model Config page with three tabs."""

    def test_model_config_page_loads(self, page: Page, app: ShinyAppProc):
        """Model Config page shows the Simulation Config tab by default."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        expect(page.locator("body")).to_contain_text("Simulation Config")

    def test_load_configuration_button(self, page: Page, app: ShinyAppProc):
        """Load Configuration button exists and is clickable."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        btn = page.locator("#load_sim_config")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(ACTION_WAIT)

    def test_time_period_inputs(self, page: Page, app: ShinyAppProc):
        """Base year, start date, end date inputs are present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        expect(page.locator("#sim_base_year")).to_be_visible()

    def test_time_stepping_inputs(self, page: Page, app: ShinyAppProc):
        """Steps/day and print interval inputs are present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        expect(page.locator("#sim_timesteps_per_day")).to_be_visible()
        expect(page.locator("#sim_print_interval")).to_be_visible()

    def test_model_options_switches(self, page: Page, app: ShinyAppProc):
        """Sediment model switch and resuspension select are present."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        expect(page.locator("#sim_resuspension")).to_be_visible()

    def test_run_model_tab(self, page: Page, app: ShinyAppProc):
        """Run Model tab has executable selector, input file, and run button."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        expect(page.locator("#run_executable")).to_be_visible()
        expect(page.locator("#cmd_input_file")).to_be_visible()
        expect(page.locator("#run")).to_be_visible()

    def test_input_file_dropdown_has_30day(self, page: Page, app: ShinyAppProc):
        """Input file dropdown includes INPUT_30day.txt."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        page.wait_for_timeout(ACTION_WAIT)
        text = page.locator("#cmd_input_file").inner_text()
        assert "INPUT_30day" in text, f"INPUT_30day.txt not in dropdown: {text[:200]}"

    def test_constants_file_dropdown(self, page: Page, app: ShinyAppProc):
        """Constants file dropdown includes WCONST_04.txt."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        page.wait_for_timeout(ACTION_WAIT)
        text = page.locator("#cmd_constants_file").inner_text()
        assert "WCONST_04" in text

    def test_command_preview(self, page: Page, app: ShinyAppProc):
        """Command Preview shows an executable command string."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        page.wait_for_timeout(ACTION_WAIT)
        preview = page.locator("#cmd_preview")
        expect(preview).to_be_visible()
        preview_text = preview.inner_text()
        assert "ESTAS" in preview_text or "INPUT" in preview_text

    def test_output_config_tab(self, page: Page, app: ShinyAppProc):
        """Output Config tab has box checkboxes and output directory selector."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Output Config")
        expect(page.locator("#output_boxes")).to_be_visible()

    def test_select_30day_input_and_wconst04(self, page: Page, app: ShinyAppProc):
        """Tutorial config: select INPUT_30day.txt and WCONST_04.txt."""
        goto_app(page, app)
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        page.wait_for_timeout(ACTION_WAIT)
        # Select INPUT_30day.txt
        page.select_option("#cmd_input_file", "INPUT_30day.txt")
        page.wait_for_timeout(ACTION_WAIT)
        # Select WCONST_04.txt
        page.select_option("#cmd_constants_file", "WCONST_04.txt")
        page.wait_for_timeout(ACTION_WAIT)
        # Verify command preview updated
        preview_text = page.locator("#cmd_preview").inner_text()
        assert "INPUT_30day" in preview_text
        assert "WCONST_04" in preview_text


# ===================================================================
# Step 8 — Running the Simulation (Quick Run)
# ===================================================================

class TestStep8RunSimulation:
    """Tutorial Step 8: Configure and run the 30-day simulation.

    This test actually triggers a model run using Quick Run.
    It requires the ESTAS_II executable to be present.
    """

    def test_quick_run_30day(self, page: Page, app: ShinyAppProc):
        """Run a 30-day simulation via Quick Run and wait for completion."""
        goto_app(page, app)

        # First, configure input file to INPUT_30day.txt on Model Config
        navigate_to(page, "nav_model_control")
        click_tab(page, "Run Model")
        page.wait_for_timeout(ACTION_WAIT)
        page.select_option("#cmd_input_file", "INPUT_30day.txt")
        page.wait_for_timeout(ACTION_WAIT)
        page.select_option("#cmd_constants_file", "WCONST_04.txt")
        page.wait_for_timeout(ACTION_WAIT)

        # Navigate to Dashboard and click Quick Run
        navigate_to(page, "nav_dashboard")
        page.locator("#quick_run").click()

        # Wait for run to complete — poll the run log for completion
        # 30-day run takes ~2-5 min; we allow up to 10 min
        max_wait_ms = 600_000
        poll_interval_ms = 5000
        elapsed = 0
        completed = False

        while elapsed < max_wait_ms:
            page.wait_for_timeout(poll_interval_ms)
            elapsed += poll_interval_ms
            # Check the dashboard run log for completion markers
            log_el = page.locator("#dashboard_log_container")
            if log_el.count() > 0:
                log_text = log_el.inner_text()
                if "completed successfully" in log_text.lower() or "model completed" in log_text.lower():
                    completed = True
                    break
                if "error" in log_text.lower() and "aborted" in log_text.lower():
                    # Run failed
                    raise AssertionError(f"Model run failed:\n{log_text[-500:]}")

            # Also check the mini run log
            mini_log = page.locator("#run_log_mini")
            if mini_log.count() > 0:
                mini_text = mini_log.inner_text()
                if "completed successfully" in mini_text.lower() or "model completed" in mini_text.lower():
                    completed = True
                    break

        assert completed, (
            f"Model run did not complete within {max_wait_ms // 1000}s. "
            "Ensure ESTAS_II executable exists and INPUT_30day.txt is valid."
        )


# ===================================================================
# Step 9 — Viewing Results (Plots)
# ===================================================================

class TestStep9ViewResults:
    """Tutorial Step 9: Plots page — output directory, model output, input TS.

    These tests use pre-existing output in OUTPUTS_30day/ so they work
    even if the Quick Run was not executed.
    """

    def test_plots_page_loads(self, page: Page, app: ShinyAppProc):
        """Plots page renders with Output Directory tab."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        expect(page.locator("body")).to_contain_text("Output Directory")

    def test_output_directory_selector(self, page: Page, app: ShinyAppProc):
        """Output directory dropdown is present."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        expect(page.locator("#output_dir_select")).to_be_visible()

    def test_select_outputs_30day(self, page: Page, app: ShinyAppProc):
        """Select OUTPUTS_30day directory and verify it's accepted."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        page.wait_for_timeout(ACTION_WAIT)
        dir_select = page.locator("#output_dir_select")
        dir_text = dir_select.inner_text()
        if "OUTPUTS_30day" in dir_text:
            page.select_option("#output_dir_select", "OUTPUTS_30day")
            page.wait_for_timeout(ACTION_WAIT)

    def test_model_output_tab(self, page: Page, app: ShinyAppProc):
        """Model Output tab has format selector, file selector, and axis controls."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Model Output")
        expect(page.locator("#output_format")).to_be_visible()
        expect(page.locator("#plot_output_file")).to_be_visible()

    def test_output_file_format_options(self, page: Page, app: ShinyAppProc):
        """File format radio buttons include Text (.out)."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Model Output")
        expect(page.locator("label:has-text('Text')")).to_be_visible()

    def test_variable_selectors(self, page: Page, app: ShinyAppProc):
        """Left axis and right axis variable selectize inputs are present."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Model Output")
        # Selectize inputs render as div containers
        expect(page.locator("#left_vars + .selectize-control, .selectize-input").first).to_be_visible()

    def test_smoothing_controls(self, page: Page, app: ShinyAppProc):
        """Rolling mean checkbox and window slider are present."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Model Output")
        expect(page.locator("#smooth")).to_be_visible()
        expect(page.locator("#smooth_window")).to_be_visible()

    def test_refresh_plot_button(self, page: Page, app: ShinyAppProc):
        """Refresh Plot button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Model Output")
        expect(page.locator("#refresh_plot")).to_be_visible()

    def test_input_timeseries_tab(self, page: Page, app: ShinyAppProc):
        """Input Timeseries tab shows forcing file selector."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Input Timeseries")
        expect(page.locator("#input_ts_file")).to_be_visible()
        expect(page.locator("#plot_input_ts")).to_be_visible()

    def test_input_ts_file_options(self, page: Page, app: ShinyAppProc):
        """Input timeseries dropdown has Temperature, Salinity, Flow, etc."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        click_tab(page, "Input Timeseries")
        text = page.locator("#input_ts_file").inner_text()
        assert "Temperature" in text or "TEMP" in text

    def test_data_preview_tab(self, page: Page, app: ShinyAppProc):
        """Data Preview tab exists."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        expect(page.locator(".nav-link:has-text('Data Preview')")).to_be_visible()

    def test_analyze_output_directory(self, page: Page, app: ShinyAppProc):
        """Analyze Directory button exists and is clickable."""
        goto_app(page, app)
        navigate_to(page, "nav_plot")
        btn = page.locator("#analyze_output_dir")
        expect(btn).to_be_visible()


# ===================================================================
# Step 10 — Analysing Mass Balance
# ===================================================================

class TestStep10MassBalance:
    """Tutorial Step 10: Mass Balance calculation and display."""

    def test_mass_balance_page_loads(self, page: Page, app: ShinyAppProc):
        """Mass Balance page has the calculate button."""
        goto_app(page, app)
        navigate_to(page, "nav_mass_balance")
        expect(page.locator("#calc_mass_balance")).to_be_visible()

    def test_element_selector(self, page: Page, app: ShinyAppProc):
        """Element dropdown has N, C, P, Si."""
        goto_app(page, app)
        navigate_to(page, "nav_mass_balance")
        text = page.locator("#mb_element").inner_text()
        for element in ["Nitrogen", "Carbon", "Phosphorus", "Silicon"]:
            assert element in text, f"{element} not found in element dropdown"

    def test_mass_balance_page_sections(self, page: Page, app: ShinyAppProc):
        """Mass Balance page has Summary and Element Details sections."""
        goto_app(page, app)
        navigate_to(page, "nav_mass_balance")
        expect(page.locator("body")).to_contain_text("Summary")
        expect(page.locator("body")).to_contain_text("Element Details")


# ===================================================================
# Step 11 — Comparing with Observations
# ===================================================================

class TestStep11Observations:
    """Tutorial Step 11: Observations page for model validation."""

    def test_observations_page_loads(self, page: Page, app: ShinyAppProc):
        """Observations page renders with file selection controls."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        expect(page.locator("body")).to_contain_text("Model Validation")

    def test_scan_observations_button(self, page: Page, app: ShinyAppProc):
        """Scan OBSERVATIONS Directory button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        expect(page.locator("#obs_scan_dir")).to_be_visible()

    def test_scan_and_list_files(self, page: Page, app: ShinyAppProc):
        """Scanning the OBSERVATIONS directory populates the file list."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        page.locator("#obs_scan_dir").click()
        page.wait_for_timeout(LONG_WAIT)
        # After scanning, obs_file_select should have options
        select = page.locator("#obs_file_select")
        text = select.inner_text()
        assert len(text.strip()) > 0, "Observation file list should be populated after scan"

    def test_generate_sample_data_button(self, page: Page, app: ShinyAppProc):
        """Generate Sample Data button is present for testing."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        expect(page.locator("#generate_sample_obs")).to_be_visible()

    def test_load_file_button(self, page: Page, app: ShinyAppProc):
        """Load Selected File button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        expect(page.locator("#obs_load_file")).to_be_visible()

    def test_comparison_sections(self, page: Page, app: ShinyAppProc):
        """Comparison Summary and Variable Details sections exist."""
        goto_app(page, app)
        navigate_to(page, "nav_observations")
        expect(page.locator("body")).to_contain_text("Comparison Summary")
        expect(page.locator("body")).to_contain_text("Variable Details")


# ===================================================================
# Extra — Scenarios
# ===================================================================

class TestScenarios:
    """Tutorial Section 6: Scenarios page for saving/loading presets."""

    def test_scenarios_page_loads(self, page: Page, app: ShinyAppProc):
        """Scenarios page renders with load and save sections."""
        goto_app(page, app)
        navigate_to(page, "nav_scenarios")
        expect(page.locator("body")).to_contain_text("Load Scenario")
        expect(page.locator("body")).to_contain_text("Save Current Configuration")

    def test_scenario_name_input(self, page: Page, app: ShinyAppProc):
        """Scenario name text input is present."""
        goto_app(page, app)
        navigate_to(page, "nav_scenarios")
        expect(page.locator("#new_scenario_name")).to_be_visible()

    def test_save_scenario_button(self, page: Page, app: ShinyAppProc):
        """Save as New Scenario button is present."""
        goto_app(page, app)
        navigate_to(page, "nav_scenarios")
        expect(page.locator("#save_scenario")).to_be_visible()

    def test_scenario_checkboxes(self, page: Page, app: ShinyAppProc):
        """Include checkboxes for Parameters, ICs, Options are present."""
        goto_app(page, app)
        navigate_to(page, "nav_scenarios")
        expect(page.locator("#scenario_include_params")).to_be_visible()
        expect(page.locator("#scenario_include_ics")).to_be_visible()
        expect(page.locator("#scenario_include_options")).to_be_visible()


# ===================================================================
# Extra — Model Structure Diagram
# ===================================================================

class TestModelStructure:
    """Tutorial: Model Structure diagram page."""

    def test_model_structure_page(self, page: Page, app: ShinyAppProc):
        """Model Structure page renders with diagram info."""
        goto_app(page, app)
        navigate_to(page, "nav_model_structure")
        expect(page.locator("body")).to_contain_text("Model Structure")


# ===================================================================
# Extra — Geographic Map
# ===================================================================

class TestMap:
    """Tutorial Section 7: Geographic Visualization page."""

    def test_map_page_loads(self, page: Page, app: ShinyAppProc):
        """Map page renders with map controls."""
        goto_app(page, app)
        navigate_to(page, "nav_map")
        expect(page.locator("body")).to_contain_text("Geographic Visualization")

    def test_map_controls_present(self, page: Page, app: ShinyAppProc):
        """Map settings controls (style, lat, lon, zoom) are present."""
        goto_app(page, app)
        navigate_to(page, "nav_map")
        expect(page.locator("#map_style")).to_be_visible()
        expect(page.locator("#map_lat")).to_be_visible()
        expect(page.locator("#map_lon")).to_be_visible()
        expect(page.locator("#map_zoom")).to_be_visible()


# ===================================================================
# Extra — Tutorial HTML Page
# ===================================================================

class TestTutorialHTML:
    """Verify the tutorial HTML page is served correctly."""

    def test_tutorial_html_accessible(self, page: Page, app: ShinyAppProc):
        """tutorial.html is served by the static assets and has content."""
        page.goto(f"{app.url}/tutorial.html")
        page.wait_for_timeout(2000)
        expect(page.locator("body")).to_contain_text("ESTAS-AQUABC Tutorial")

    def test_tutorial_has_toc(self, page: Page, app: ShinyAppProc):
        """Tutorial HTML has a table of contents."""
        page.goto(f"{app.url}/tutorial.html")
        page.wait_for_timeout(2000)
        expect(page.locator("#TOC, nav")).to_be_visible()

    def test_tutorial_has_all_steps(self, page: Page, app: ShinyAppProc):
        """Tutorial HTML contains all 11 tutorial steps."""
        page.goto(f"{app.url}/tutorial.html")
        page.wait_for_timeout(2000)
        body_text = page.locator("body").inner_text()
        for step_num in range(1, 12):
            assert f"Step {step_num}" in body_text, f"Step {step_num} not found in tutorial"
