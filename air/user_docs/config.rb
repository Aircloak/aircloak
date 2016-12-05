require './lib/redcarpet_header_fix'

set :css_dir, 'stylesheets'
set :js_dir, 'javascripts'
set :images_dir, 'images'
set :fonts_dir, 'fonts'

set :markdown_engine, :redcarpet
set :markdown, :fenced_code_blocks => true, :smartypants => true, :disable_indented_code_blocks => true, :prettify => true, :tables => true, :with_toc_data => true, :no_intra_emphasis => true

# Activate the syntax highlighter
activate :syntax

# This is needed for Github pages, since they're hosted on a subdomain
set :relative_links, true

configure :build do
  # Minify assets in production
  activate :minify_css
  activate :minify_javascript

  # This forces clients to load new
  # assets should they change.
  activate :asset_hash

  # Or use a different image path
  set :http_prefix, "/docs"
end

