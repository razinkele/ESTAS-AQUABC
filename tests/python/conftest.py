"""Pytest configuration for the Python test suite.

Skip the browser-based integration tests (Playwright / Selenium) when their
optional dependencies are not installed -- e.g. the lint/unit-test CI job, which
does not install playwright or selenium. The dedicated integration-tests job
installs playwright and runs test_app_playwright.py explicitly.
"""
import importlib.util

collect_ignore = []
if importlib.util.find_spec("playwright") is None:
    collect_ignore += ["test_app_playwright.py", "test_tutorial_playwright.py"]
if importlib.util.find_spec("selenium") is None:
    collect_ignore += ["test_app_selenium.py"]
