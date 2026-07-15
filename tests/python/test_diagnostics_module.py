try:
    from shiny_app.diagnostics import diagnostics_ui
except ImportError:
    from diagnostics import diagnostics_ui


def nid(module_id: str, input_id: str) -> str:
    """DOM id of a namespaced Shiny-module input/output (Shiny joins with '-')."""
    return f"{module_id}-{input_id}"


def test_diagnostics_ui_namespaces_ids():
    # diagnostics_ui returns a ui.card wrapping a navset_card_tab, which does
    # NOT render via str() (returns a Python repr). Use .tagify() to get real HTML.
    html = str(diagnostics_ui("diagnostics").tagify())
    for raw in (
        "diag_output_dir",
        "diag_run_btn",
        "diag_run_status",
        "diag_severity_cards",
        "diag_findings_table",
        "diag_filter_box",
        "diag_gen_results_pdf",
    ):
        assert nid("diagnostics", raw) in html, f"missing namespaced id for {raw}"
    # a stable literal survives
    assert "Process Rate Diagnostics" in html
    # the module UI must NOT carry the app-level nav wrapper (that stays in create_ui)
    assert "input.navigation" not in html
