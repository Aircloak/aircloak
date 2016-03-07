# create namespace for result exception related functions
window.ResultException = {}


ResultException.parse_to_error = (exception) ->
  error = {count: exception.count}
  # look for sandbox error messages
  sandboxErrorMatch = exception.error.match /^{sandbox_error,"(.*)"}$/
  if sandboxErrorMatch
    sandboxError = sandboxErrorMatch[1]
    # look for Lua code error messages
    lineErrorMatch = sandboxError.match /^\[string \\"([^\\]*)\\"\]:(\d*): (.*)/
    if lineErrorMatch # if we have a match
      error.module = lineErrorMatch[1]
      error.line = Number(lineErrorMatch[2])
      error.message = lineErrorMatch[3]
    else
      error.message = sandboxError # we don't know the format of this type of sandbox error
  else
    error.message = exception.error # we don't know the format of this type of exception
  error


ResultException.format_error = (error) ->
  reason= if error.module
        "[@#{error.module}:#{error.line}] #{error.message}"
      else
        error.message
  "#{error.count} occurances of \"#{reason}\""
