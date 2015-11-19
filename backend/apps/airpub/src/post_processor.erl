%% @doc Module for post-processing published articles.
%%      For each article, the post-processing rules, defined in the sys.config file,
%%      are checked and, in case of a match, the article is unpacked, the corresponding
%%      JavaScript function is invoked to handle the article and the new content is
%%      re-packed and sent to subscribers.
-module(post_processor).

-export([
  edit/1
]).

-include("types.hrl").


%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------

%% @doc The single entry point into the module. It will check and apply the post-processing rules.
-spec edit(#article{}) -> #article{}.
edit(#article{path = Path} = Article) ->
  {ok, PostProcessingRules} = application:get_env(airpub, post_processing_rules),
  PostProcessingRule = first(fun ({PathPrefix, _JSFunction}) ->
        lists:prefix(PathPrefix, Path)
      end, PostProcessingRules),
  case PostProcessingRule of
    {_PathPrefix, JSFunction} -> edit(Article, JSFunction);
    undefined -> Article
  end.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

% Helper function for returning the first item in a list for which the specified predicate is true.
-spec first(fun(), [term()]) -> term().
first(_Predicate, []) ->
  undefined;
first(Predicate, [Item | Rest]) ->
  case Predicate(Item) of
    true -> Item;
    false -> first(Rest, Predicate)
  end.

% The post-processing rule handler. It will unpack the content of the article, apply
% the given JavaScript function over it and repack the result.
-spec edit(#article{}, string()) -> #article{}.
edit(Article = #article{content_encoding = ContentEncoding}, JSFunction) ->
  try
    StartTime = os:timestamp(),
    Content = unpack_content(ContentEncoding, Article#article.content),
    case js_vm_sup:call(list_to_binary(JSFunction), [Content]) of
      {ok, null} ->
        Article; % post_processing function ignored this article
      {ok, NewContent} ->
        NewContentPacked = pack_content(ContentEncoding, NewContent),
        Duration = timer:now_diff(os:timestamp(), StartTime) / 1000.0,
        lager:info("Article published at ~s was post-processed in ~p ms", [Article#article.path, Duration]),
        Article#article{content = NewContentPacked};
      {error, Error} ->
        lager:error("An error occurred during article post-processing: ~p", [Error]),
        Article
    end
  catch
    ErrorType:ErrorReason ->
      lager:error("An exception occurred during article post-processing: ~p:~p", [ErrorType, ErrorReason]),
      lager:info("Stacktrace: ~p", [erlang:get_stacktrace()]),
      Article
  end.

-spec unpack_content(string(), binary()) -> binary().
unpack_content("undefined", Content) ->
  Content;
unpack_content("identity", Content) ->
  Content;
unpack_content("gzip", Content) ->
  zlib:gunzip(Content);
unpack_content("deflate", Content) ->
  zlib:uncompress(Content).

-spec pack_content(string(), binary()) -> binary().
pack_content("undefined", Content) ->
  Content;
pack_content("identity", Content) ->
  Content;
pack_content("gzip", Content) ->
  zlib:gzip(Content);
pack_content("deflate", Content) ->
  zlib:compress(Content).
