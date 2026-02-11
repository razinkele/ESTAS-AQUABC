import os

from starlette.applications import Starlette
from starlette.responses import PlainTextResponse
from starlette.routing import Route

# Import the Shiny App object from the app module
from shiny_app.app import app as pyshiny_app

async def _health(request):
    return PlainTextResponse("ok")

async def _info(request):
    # diagnostic info about the mounted app
    from starlette.responses import JSONResponse
    info = {
        "pyshiny_app_type": str(type(pyshiny_app)),
        "pyshiny_app_callable": callable(pyshiny_app),
    }
    try:
        star_app = getattr(pyshiny_app, "starlette_app", None)
        if star_app is not None:
            route_info = []
            for r in star_app.router.routes:
                try:
                    app_obj = getattr(r, 'app', None)
                    route_info.append({
                        'route_type': type(r).__name__,
                        'path': getattr(r, 'path', getattr(r, 'name', None)),
                        'app_type': str(type(app_obj)),
                        'app_callable': callable(app_obj)
                    })
                except Exception as e:
                    route_info.append({'route': str(r), 'error': str(e)})
            info['pyshiny_starlette_routes'] = route_info
    except Exception as e:
        info["error_inspecting_starlette_app"] = str(e)
    return JSONResponse(info)

starlette_app = Starlette(debug=False, routes=[
    Route("/_health", _health),
    Route("/_info", _info)
])

# mount the shiny app at root
starlette_app.mount("/", pyshiny_app)

# Wrap the Starlette app with an ASGI exception-logging proxy to capture details when errors occur
class ExceptionLoggingASGI:
    def __init__(self, inner_app):
        self.inner_app = inner_app

    async def __call__(self, scope, receive, send):
        try:
            return await self.inner_app(scope, receive, send)
        except Exception as e:
            # log debug info to a temporary file for post-mortem
            import traceback, json, time
            info = {
                'time': time.time(),
                'scope': {k: v for k, v in scope.items() if k != 'app'},
                'exception': repr(e),
            }
            try:
                # inspect pyshiny_app routes
                star_app = getattr(pyshiny_app, 'starlette_app', None)
                if star_app is not None:
                    route_info = []
                    for r in star_app.router.routes:
                        app_obj = getattr(r, 'app', None)
                        route_info.append({
                            'route_type': type(r).__name__,
                            'path': getattr(r, 'path', getattr(r, 'name', None)),
                            'app_type': str(type(app_obj)),
                            'app_callable': callable(app_obj)
                        })
                    info['pyshiny_routes'] = route_info
            except Exception as e2:
                info['route_inspect_error'] = repr(e2)
            log_dir = os.path.join(os.path.dirname(__file__), 'logs')
            os.makedirs(log_dir, exist_ok=True)
            log_file = os.path.join(log_dir, 'shiny_asgi_errors.log')
            with open(log_file, 'a') as fh:
                fh.write(json.dumps(info) + '\n')
            raise

app = ExceptionLoggingASGI(starlette_app)
