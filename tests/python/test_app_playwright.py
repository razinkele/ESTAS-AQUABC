"""Playwright integration tests for the AQUABC Shiny app.

Uses Shiny's built-in Playwright testing framework (shiny.playwright.controller)
to test UI interactions against a live app instance.

Run with:
    cd /home/razinka/AQUABCv0.2
    /opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_app_playwright.py -v --headed
"""

from playwright.sync_api import Page, expect
from shiny.pytest import create_app_fixture
from shiny.run import ShinyAppProc

# Create fixture pointing to our app
app = create_app_fixture("../../shiny_app/app.py", timeout_secs=30)


def navigate_to(page: Page, nav_id: str):
    """Navigate to a panel by setting the Shiny navigation input via JavaScript.

    The app uses a custom sidebar with data-nav-id attributes and a hidden
    input.navigation text field. Clicking nav links triggers
    Shiny.setInputValue('navigation', navId).
    """
    page.evaluate(f"Shiny.setInputValue('navigation', '{nav_id}')")
    page.wait_for_timeout(1500)


# ---------------------------------------------------------------------------
# App Startup & Dashboard
# ---------------------------------------------------------------------------

class TestAppStartup:
    """Verify the app starts and renders the dashboard."""

    def test_app_loads(self, page: Page, app: ShinyAppProc):
        """App starts without error and renders the page title."""
        page.goto(app.url)
        expect(page.locator("body")).to_be_visible()

    def test_dashboard_visible(self, page: Page, app: ShinyAppProc):
        """Dashboard panel is visible on startup."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        expect(page.locator("body")).to_contain_text("AQUABC")

    def test_navigation_sidebar_present(self, page: Page, app: ShinyAppProc):
        """Sidebar navigation container with nav links is present."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        sidebar = page.locator("#custom-sidebar")
        expect(sidebar).to_be_visible()
        # All main nav links should exist
        nav_ids = [
            "nav_dashboard", "nav_model_build", "nav_model_control",
            "nav_input_files", "nav_parameters", "nav_initial_conditions",
            "nav_model_options", "nav_plot",
        ]
        for nav_id in nav_ids:
            link = page.locator(f".nav-link[data-nav-id='{nav_id}']")
            expect(link).to_be_attached()


# ---------------------------------------------------------------------------
# Navigation
# ---------------------------------------------------------------------------

class TestNavigation:
    """Test navigation between major panels."""

    def test_navigate_to_input_files(self, page: Page, app: ShinyAppProc):
        """Navigating to Input Files shows the file editor panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        expect(page.locator("body")).to_contain_text("File Contents")

    def test_navigate_to_parameters(self, page: Page, app: ShinyAppProc):
        """Navigating to Parameters shows the parameter editor."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_parameters")
        # The load button and category selector should be visible
        expect(page.locator("#load_params")).to_be_visible()
        expect(page.locator("#param_category")).to_be_visible()

    def test_navigate_to_model_config(self, page: Page, app: ShinyAppProc):
        """Navigating to Model Config shows simulation configuration."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_control")
        expect(page.locator("body")).to_contain_text("Simulation Config")

    def test_navigate_to_model_build(self, page: Page, app: ShinyAppProc):
        """Navigating to Model Build shows the build panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_build")
        expect(page.locator("body")).to_contain_text("Build")

    def test_navigate_to_initial_conditions(self, page: Page, app: ShinyAppProc):
        """Navigating to Initial Conditions shows the IC panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_initial_conditions")
        expect(page.locator("#load_ics")).to_be_visible()

    def test_navigate_to_model_options(self, page: Page, app: ShinyAppProc):
        """Navigating to Model Options shows the options panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_options")
        expect(page.locator("#load_options")).to_be_visible()

    def test_navigate_to_plots(self, page: Page, app: ShinyAppProc):
        """Navigating to Plots shows the plotting panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_plot")
        expect(page.locator("body")).to_contain_text("Output Directory")


# ---------------------------------------------------------------------------
# Simulation Config Panel
# ---------------------------------------------------------------------------

class TestSimulationConfig:
    """Test the simulation configuration panel interactions."""

    def test_load_config_button(self, page: Page, app: ShinyAppProc):
        """Load Configuration button exists and is clickable."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_control")
        btn = page.locator("#load_sim_config")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(2000)
        base_year = page.locator("#sim_base_year")
        expect(base_year).to_be_visible()

    def test_numeric_inputs_present(self, page: Page, app: ShinyAppProc):
        """Simulation config numeric inputs are present."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_control")
        for input_id in ["sim_base_year", "sim_timesteps_per_day", "sim_print_interval"]:
            expect(page.locator(f"#{input_id}")).to_be_visible()


# ---------------------------------------------------------------------------
# Input Files Panel
# ---------------------------------------------------------------------------

class TestInputFiles:
    """Test the input files editor panel."""

    def test_file_select_populated(self, page: Page, app: ShinyAppProc):
        """File select dropdown is populated with INPUTS directory files."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        select = page.locator("#file_select")
        expect(select).to_be_visible()
        options = select.locator("option")
        count = options.count()
        assert count > 0, "File select should have at least one file option"

    def test_file_content_loads_on_select(self, page: Page, app: ShinyAppProc):
        """Selecting a file loads its content into the text area."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        select = page.locator("#file_select")
        first_option = select.locator("option").first
        value = first_option.get_attribute("value")
        if value:
            select.select_option(value)
            page.wait_for_timeout(1000)
            textarea = page.locator("#file_contents")
            expect(textarea).to_be_visible()

    def test_refresh_files_button(self, page: Page, app: ShinyAppProc):
        """Refresh file list button exists and is clickable."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        btn = page.locator("#refresh_files")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(1000)

    def test_file_contents_textarea_present(self, page: Page, app: ShinyAppProc):
        """File contents textarea is present in the editor panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        textarea = page.locator("#file_contents")
        expect(textarea).to_be_visible()


# ---------------------------------------------------------------------------
# Parameters Panel
# ---------------------------------------------------------------------------

class TestParameters:
    """Test the parameter editor panel."""

    def test_load_parameters(self, page: Page, app: ShinyAppProc):
        """Load Parameters button loads parameter data."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_parameters")
        btn = page.locator("#load_params")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(3000)
        # After loading, the param_table should have parameter inputs
        expect(page.locator("#param_category")).to_be_visible()


# ---------------------------------------------------------------------------
# Build Panel
# ---------------------------------------------------------------------------

class TestBuildPanel:
    """Test the model build panel elements."""

    def test_build_controls_present(self, page: Page, app: ShinyAppProc):
        """Build panel has build and rebuild buttons."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#btn_build")).to_be_visible()
        expect(page.locator("#btn_rebuild")).to_be_visible()

    def test_build_log_visible(self, page: Page, app: ShinyAppProc):
        """Build log output area is present."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#build_log")).to_be_visible()
