%% @doc Testing the {@link anonymizer}.
%% @end
-module(anonymizer_test).
-proper(simple).

-include_lib("proper/include/proper.hrl").
-include("cloak.hrl").

-define(MAX_LENGTH, 150).
-define(MIN_LENGTH_FOR_AVARAGE_NOISE, 10).


%% ---------------------------------------------------------------------
%% PropEr Test
%% ---------------------------------------------------------------------

prop_anonymizer() ->
  random:seed(),
  ?FORALL(InputList0, g_bucket_reports(),
    ?TRAPEXIT(
      begin
        InputList = strip_doubles(InputList0),
        OutputList = anonymizer:anonymize(InputList),
        Errors = check_output(InputList, OutputList),
        ?WHENFAIL(
            io:format("Input list: ~p\nOutput list: ~p\nErrors: ~p\n",
              [InputList, OutputList, Errors]),
            measure("Input length", length(InputList),
              measure("Output length", length(OutputList),
                Errors == [])))
      end
    )
  ).


%% ---------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------

check_output(InputList, OutputList) ->
  Functions = [
    fun() -> check_length(InputList, OutputList) end,
    fun() -> noisy_results_positive(OutputList) end,
    fun() -> check_correct_rounding(OutputList) end,
    fun() -> check_lower_limit_k1(OutputList) end,
    fun() -> check_no_additional_bucket(InputList, OutputList) end,
    fun() -> check_average_noise_greater_minimum(OutputList) end
  ],
  Results = [lists:usort(Function()) || Function <- Functions],
  lists:usort(lists:concat(Results)).

check_length(InputList, OutputList) ->
  case length(InputList) >= length(OutputList) of
    true -> [];
    false -> ["output list is longer than input list"]
  end.

noisy_results_positive(OutputList) ->
  ["non-positive noisy bucket count" ||
    #bucket_report{noisy_count=NoisyCount} <- OutputList,
    NoisyCount =< 0].

check_correct_rounding(OutputList) ->
  ["erroneous rounding" ||
    #bucket_report{noisy_count=NoisyCount, noise_sd=Sigma} <- OutputList,
    not check_correct_rounding(NoisyCount, Sigma)].

check_correct_rounding(NoisyCount, Sigma) when Sigma < 5 -> NoisyCount rem 5 == 0;
check_correct_rounding(NoisyCount, _Sigma) -> NoisyCount rem 10 == 0.

check_lower_limit_k1(OutputList) ->
  #anonymizer_params{absolute_lower_bound=LowerBound} = anonymizer:default_params(),
  ["bucket included with count not above #anonymizer_params.absolute_lower_bound" ||
    #bucket_report{count=Count} <- OutputList,
    Count =< LowerBound].

check_no_additional_bucket(InputList, OutputList) ->
  InputDict = lists:foldl(fun(#bucket_report{label=Label}, D) ->
    dict:store(Label, true, D)
  end, dict:new(), InputList),
  ["unknown bucket created by anonymization function" ||
    #bucket_report{label=Label} <- OutputList,
    not dict:is_key(Label, InputDict)].

check_average_noise_greater_minimum(OutputList) ->
  #anonymizer_params{min_sigma = MinSigma} = anonymizer:default_params(),
  SD = math:sqrt(lists:foldl(fun(#bucket_report{count=Count, noisy_count=NoisyCount}, Acc) ->
    Noise = Count - NoisyCount,
    Noise * Noise + Acc
  end, 0, OutputList)),
  ["average noise is too small" ||
    _ <- [SD],
    length(OutputList) > ?MIN_LENGTH_FOR_AVARAGE_NOISE,
    SD < MinSigma].


%% ---------------------------------------------------------------------
%% Generators
%% ---------------------------------------------------------------------

g_bucket_reports() ->
  ?LET(Length, integer(0, ?MAX_LENGTH), g_bucket_reports(Length, [])).

g_bucket_reports(0, Acc) ->
  return(Acc);
g_bucket_reports(N, Acc) ->
  ?LET(BucketReport, g_bucket_report(), g_bucket_reports(N - 1, [BucketReport|Acc])).

g_bucket_report() ->
  ?LET(Label, g_bucket_label(),
    ?LET(Count, g_bucket_count(),
      ?LET(UsersHash, binary(16),
        return(#bucket_report{
          label = Label,
          count = Count,
          noisy_count = Count,
          users_hash = UsersHash
        })))).

g_bucket_label() ->
  ?LET(Label, string(),
    ?LET(Value, string(),
      return(#bucket_label{
        label = Label,
        value = Value
      }))).

g_bucket_count() ->
  ?LET(Base, integer(1, 10), return(integer(1, Base*Base*Base*Base))).


%% ---------------------------------------------------------------------
%% Internal Functions
%% ---------------------------------------------------------------------

strip_doubles(List) ->
  Dict = lists:foldl(fun(#bucket_report{label=Label}=BucketReport, D) ->
    dict:store(Label, BucketReport, D)
  end, dict:new(), List),
  [BucketReport || {_, BucketReport} <- dict:to_list(Dict)].
