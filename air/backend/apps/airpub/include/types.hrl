-record(article, {
  path :: string(),
  content_type = "undefined" :: string(),
  content_encoding = "undefined" :: string(),
  content :: binary(),
  published_at = os:timestamp() :: erlang:timestamp()
}).
