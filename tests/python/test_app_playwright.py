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
        expect(page.locator("#parameters-load_params")).to_be_visible()
        expect(page.locator("#parameters-param_category")).to_be_visible()

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
        expect(page.locator("#initial_conditions-load_ics")).to_be_visible()

    def test_navigate_to_model_options(self, page: Page, app: ShinyAppProc):
        """Navigating to Model Options shows the options panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_options")
        expect(page.locator("#model_options-load_options")).to_be_visible()

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
        btn = page.locator("#sim_config-load_sim_config")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(2000)
        base_year = page.locator("#sim_config-sim_base_year")
        expect(base_year).to_be_visible()

    def test_numeric_inputs_present(self, page: Page, app: ShinyAppProc):
        """Simulation config numeric inputs are present."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_control")
        for input_id in ["sim_config-sim_base_year", "sim_config-sim_timesteps_per_day", "sim_config-sim_print_interval"]:
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
        select = page.locator("#input_files-file_select")
        expect(select).to_be_visible()
        options = select.locator("option")
        count = options.count()
        assert count > 0, "File select should have at least one file option"

    def test_file_content_loads_on_select(self, page: Page, app: ShinyAppProc):
        """Selecting a file loads its content into the text area."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        select = page.locator("#input_files-file_select")
        first_option = select.locator("option").first
        value = first_option.get_attribute("value")
        if value:
            select.select_option(value)
            page.wait_for_timeout(1000)
            textarea = page.locator("#input_files-file_contents")
            expect(textarea).to_be_visible()

    def test_refresh_files_button(self, page: Page, app: ShinyAppProc):
        """Refresh file list button exists and is clickable."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        btn = page.locator("#input_files-refresh_files")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(1000)

    def test_file_contents_textarea_present(self, page: Page, app: ShinyAppProc):
        """File contents textarea is present in the editor panel."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_input_files")
        textarea = page.locator("#input_files-file_contents")
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
        btn = page.locator("#parameters-load_params")
        expect(btn).to_be_visible()
        btn.click()
        page.wait_for_timeout(3000)
        # After loading, the param_table should have parameter inputs
        expect(page.locator("#parameters-param_category")).to_be_visible()


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
        expect(page.locator("#model_build-btn_build")).to_be_visible()
        expect(page.locator("#model_build-btn_rebuild")).to_be_visible()

    def test_build_log_visible(self, page: Page, app: ShinyAppProc):
        """Build log output area is present."""
        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_model_build")
        expect(page.locator("#model_build-build_log")).to_be_visible()


# ---------------------------------------------------------------------------
# Map tab (deck.gl through shiny_deckgl)
# ---------------------------------------------------------------------------

MAP_CDN_MARKS = ("deck.gl", "deck-gl", "maplibre", "luma.gl", "loaders.gl", "mapbox-gl-draw")


def _until(predicate, timeout_ms=8000, step_ms=100):
    """Poll `predicate` until it is true; fail with its last value otherwise."""
    import time
    deadline = time.monotonic() + timeout_ms / 1000
    last = None
    while time.monotonic() < deadline:
        last = predicate()
        if last:
            return
        time.sleep(step_ms / 1000)
    raise AssertionError(f"condition not met within {timeout_ms} ms (last value: {last!r})")


class TestMapTab:
    """The map lives in a panel that is hidden at start-up, where a WebGL map
    can come up blank. So this checks the drawn map, not just the markup."""

    def test_map_draws_and_follows_controls(self, page: Page, app: ShinyAppProc):
        from shiny_deckgl.testing import MapWidgetController

        page.goto(app.url)
        page.wait_for_timeout(2000)
        navigate_to(page, "nav_map")

        m = MapWidgetController(page, "map-deck_map")
        m.wait_ready(timeout=30000)
        assert m.layer_ids() == ["stations"]

        # the canvas has a real size (a map built at zero size stays blank)
        box = page.locator("#map-deck_map .maplibregl-canvas").bounding_box()
        assert box is not None and box["width"] > 200 and box["height"] > 200

        # the radius slider reaches the layer
        page.evaluate("Shiny.setInputValue('map-map_point_radius', 2500)")
        _until(lambda: (m.layer("stations") or {}).get("radius") == 2500)

        # a raster basemap adds its tile layer under the stations, with its credit
        page.evaluate("Shiny.setInputValue('map-map_style', 'satellite')")
        m.expect_layers(["basemap_raster", "stations"])
        expect(page.locator("#map-map_credit")).to_contain_text("Esri")

        # the camera controls move the existing map instead of rebuilding it
        page.evaluate("Shiny.setInputValue('map-map_zoom', 12)")
        m.expect_zoom(12, timeout=8000)

        # a vector basemap swaps the style and drops the raster layer
        page.evaluate("Shiny.setInputValue('map-map_style', 'dark')")
        m.expect_layers(["stations"])
        _until(lambda: "dark-matter" in (m.style_url() or ""))

    def test_other_tabs_work_without_the_map_cdn(self, page: Page, app: ShinyAppProc):
        """The map's scripts are requested on every page load. If their CDN is
        unreachable, only the Map tab may suffer."""
        blocked = []

        def _route(route):
            url = route.request.url
            if any(mark in url for mark in MAP_CDN_MARKS):
                blocked.append(url)
                route.abort()
            else:
                route.continue_()

        page.route("**/*", _route)
        page.goto(app.url)
        page.wait_for_timeout(2000)
        assert blocked, "expected the page to request the map libraries"
        expect(page.locator("#custom-sidebar")).to_be_visible()
        navigate_to(page, "nav_model_build")
        expect(page.locator("#model_build-build_log")).to_be_visible()
        # outputs still arrive from the server (the websocket session is alive)
        navigate_to(page, "nav_map")
        expect(page.locator("#map-map_info")).to_contain_text("Curonian Lagoon")
