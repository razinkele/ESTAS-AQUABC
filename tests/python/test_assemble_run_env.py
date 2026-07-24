from shiny_app.build_commands import assemble_run_env


def test_sets_solver_and_composes_with_hold_volume():
    base = {"ESTAS_HOLD_VOLUME": "1"}
    env = assemble_run_env(base, "2")
    assert env["ESTAS_PELAGIC_SOLVER"] == "2"
    assert env["ESTAS_HOLD_VOLUME"] == "1"


def test_default_and_no_mutation():
    base = {}
    env = assemble_run_env(base, "1")
    assert env["ESTAS_PELAGIC_SOLVER"] == "1"
    assert base == {}          # input dict not mutated
