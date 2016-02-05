require './lib/redcarpet_header_fix'

set :css_dir, '/apidocs/stylesheets'
set :js_dir, '/apidocs/javascripts'
set :images_dir, '/apidocs/images'
set :fonts_dir, '/apidocs/fonts'

set :markdown_engine, :redcarpet
set :markdown, :fenced_code_blocks => true, :smartypants => true, :disable_indented_code_blocks => true, :prettify => true, :tables => true, :with_toc_data => true, :no_intra_emphasis => true

# Activate the syntax highlighter
activate :syntax

# This is needed for Github pages, since they're hosted on a subdomain
set :relative_links, true

# Build-specific configuration
configure :build do
  # For example, change the Compass output style for deployment
  activate :minify_css

  # Minify Javascript on build
  activate :minify_javascript

  # Enable cache buster
  activate :asset_hash

  # Or use a different image path
  # set :http_prefix, "/Content/images/"
end

