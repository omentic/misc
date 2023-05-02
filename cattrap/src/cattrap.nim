import std/[os, strutils]
import prologue, regex

const roll = readFile("src/rickroll.html")

# note: prologue appears to deal with directory traversal for us, nice
proc serve(ctx: Context, regex: Regex) {.async.} =
  if regex in $ctx.request.headers["User-Agent"]:
    resp roll
  else:
    try:
      if ($ctx.request.path == "/"):
        resp readFile(getCurrentDir() & "/index.html")
      else:
        resp readFile(getCurrentDir() & $ctx.request.path)
    except IOError:
      resp "<html><body><h1>404 Not Found</h1></body></html>"

try:
  let port = Port(paramStr(1).parseInt())
  let regex = re(paramStr(2))
  let settings = newSettings(port = port)
  let app = newApp(settings)

  # gcsafe for passing a global to a callback
  proc thunk(ctx: Context) {.async, gcsafe.} =
    yield serve(ctx, regex)

  # match all requests
  app.get("*$", thunk)
  app.run()

except ValueError, IndexDefect:
  echo "usage: cattrap <port> <regex>"
except OSError:
  echo "cattrap: insufficient permissions, try a different port"
