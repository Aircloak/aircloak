-record(subscriber, {
  id :: term(),
  module :: module(),
  path :: string()
}).

-record(article, {
  path :: string(),
  content_type = "undefined" :: string(),
  content :: binary(),
  published_at = os:timestamp() :: erlang:timestamp()
}).
