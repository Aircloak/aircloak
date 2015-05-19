# Common handlebars helpers

# Increments a value by a given amount (by default 1)
# Examples:
#   {{inc foo}}
#   {{inc foo by=10}}
Handlebars.registerHelper("inc", (value, options) -> parseInt(value) + parseInt(options.hash.by || 1))