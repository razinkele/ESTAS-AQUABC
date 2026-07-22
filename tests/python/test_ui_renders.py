def test_create_ui_renders():
    import shiny_app.app as app

    app.create_ui().tagify()   # the suite doesn't import app.py otherwise; guards a broken render
