"""Selenium integration tests for the AQUABC Shiny app.

Uses Selenium WebDriver with headless Chrome to test the app
via HTTP against a live Shiny process.

Run with:
    cd /home/razinka/AQUABCv0.2
    /opt/micromamba/envs/shiny/bin/python -m pytest tests/python/test_app_selenium.py -v
"""

import subprocess
import time

import pytest
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait


def navigate_to(driver, nav_id):
    """Navigate to a panel by setting the Shiny navigation input via JavaScript.

    The app uses a custom sidebar with data-nav-id attributes and a hidden
    input.navigation text field.
    """
    driver.execute_script(f"Shiny.setInputValue('navigation', '{nav_id}')")
    time.sleep(2)


@pytest.fixture(scope="module")
def shiny_app():
    """Start the Shiny app as a subprocess and yield the URL."""
    proc = subprocess.Popen(
        ["/opt/micromamba/envs/shiny/bin/python", "-m", "shiny", "run",
         "--port", "18765", "--host", "127.0.0.1", "shiny_app/app.py"],
        cwd="/home/razinka/AQUABCv0.2",
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    # Wait for the app to be ready
    url = "http://127.0.0.1:18765"
    ready = False
    for _ in range(30):
        time.sleep(1)
        try:
            import urllib.request
            with urllib.request.urlopen(url, timeout=2) as resp:
                if resp.status == 200:
                    ready = True
                    break
        except Exception:
            continue

    if not ready:
        proc.kill()
        pytest.skip("Shiny app did not start within 30 seconds")

    yield url

    proc.terminate()
    try:
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        proc.kill()


@pytest.fixture(scope="module")
def driver():
    """Create a headless Chrome WebDriver."""
    options = Options()
    options.add_argument("--headless=new")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--disable-gpu")
    options.add_argument("--window-size=1280,1024")

    try:
        drv = webdriver.Chrome(options=options)
    except Exception as e:
        pytest.skip(f"Chrome WebDriver not available: {e}")

    yield drv
    drv.quit()


class TestAppSmoke:
    """Basic smoke tests: app loads, no JS errors, key elements present."""

    def test_page_loads(self, driver, shiny_app):
        """Page loads and returns HTML with expected content."""
        driver.get(shiny_app)
        wait = WebDriverWait(driver, 15)
        wait.until(EC.presence_of_element_located((By.TAG_NAME, "body")))
        assert "AQUABC" in driver.page_source or "shiny" in driver.page_source.lower()

    def test_no_critical_js_errors(self, driver, shiny_app):
        """No critical JavaScript errors on page load."""
        driver.get(shiny_app)
        time.sleep(3)
        logs = driver.get_log("browser")
        severe_errors = [log for log in logs if log["level"] == "SEVERE"]
        # Filter out known non-critical errors (e.g., favicon 404)
        critical = [e for e in severe_errors if "favicon" not in e.get("message", "").lower()]
        assert len(critical) == 0, f"Critical JS errors: {critical}"

    def test_sidebar_nav_present(self, driver, shiny_app):
        """Sidebar navigation container is present with nav links."""
        driver.get(shiny_app)
        wait = WebDriverWait(driver, 10)
        sidebar = wait.until(EC.presence_of_element_located((By.ID, "custom-sidebar")))
        assert sidebar.is_displayed()
        # Check for some key nav links
        for nav_id in ["nav_dashboard", "nav_input_files", "nav_parameters"]:
            link = driver.find_element(By.CSS_SELECTOR, f".nav-link[data-nav-id='{nav_id}']")
            assert link is not None


class TestNavigationSelenium:
    """Test navigation between panels via Selenium."""

    def test_navigate_to_input_files(self, driver, shiny_app):
        """Navigate to Input Files panel."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_input_files")
        assert "File Contents" in driver.page_source or "file_select" in driver.page_source

    def test_navigate_to_parameters(self, driver, shiny_app):
        """Navigate to Parameters panel."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_parameters")
        wait = WebDriverWait(driver, 10)
        wait.until(EC.presence_of_element_located((By.ID, "load_params")))
        assert driver.find_element(By.ID, "param_category") is not None

    def test_navigate_to_plots(self, driver, shiny_app):
        """Navigate to Plots panel."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_plot")
        assert "Output Directory" in driver.page_source or "output_dir" in driver.page_source


class TestFileEditorSelenium:
    """Test the input file editor with Selenium."""

    def test_file_select_has_options(self, driver, shiny_app):
        """File select dropdown contains files from INPUTS directory."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_input_files")
        wait = WebDriverWait(driver, 10)
        select = wait.until(EC.presence_of_element_located((By.ID, "file_select")))
        options = select.find_elements(By.TAG_NAME, "option")
        real_options = [o for o in options if o.get_attribute("value")]
        assert len(real_options) > 5, f"Expected many input files, got {len(real_options)}"

    def test_file_contents_textarea_present(self, driver, shiny_app):
        """File contents textarea is present in the editor panel."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_input_files")
        wait = WebDriverWait(driver, 10)
        textarea = wait.until(EC.presence_of_element_located((By.ID, "file_contents")))
        assert textarea.is_displayed()


class TestSimConfigSelenium:
    """Test simulation config interactions with Selenium."""

    def test_load_config_populates_fields(self, driver, shiny_app):
        """Loading config fills in the numeric fields."""
        driver.get(shiny_app)
        time.sleep(3)
        navigate_to(driver, "nav_model_control")
        wait = WebDriverWait(driver, 10)
        load_btn = wait.until(EC.element_to_be_clickable((By.ID, "load_sim_config")))
        load_btn.click()
        time.sleep(3)
        base_year = driver.find_element(By.ID, "sim_base_year")
        value = base_year.get_attribute("value")
        assert value and int(value) >= 1900, f"Base year should be set, got: {value}"
