%% @doc Anonymizer for buckets.
%%      The anonymizer applies standard anonymization steps on
%%      individual buckets. It ensures an answer is large enough
%%      to be used, filters for high-touch
%%      users, and applies noise and rounding.
%%
%%      The documentation of the anonymization procedure can be found in
%%      docs/Anonymization.md.
-module(anonymizer).

%% API
-export([
  anonymize/1,
  anonymize/2,
  default_params/0
]).

%% API exported for property based tests
-export([
  noise_sigma_for_count/2
]).

-include("cloak.hrl").


%% -------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------

%% @doc Works just like {@link anonymize/2} but doesn't include LCF tail in the result
-spec anonymize([#bucket{}]) -> [#bucket{}].
anonymize(AggregatedBuckets) ->
  anonymize(AggregatedBuckets, undefined).

%% @doc Anonymizes a list of buckets, filtering out buckets that don't qualify
%%      for reporting, and applying noisy to the rest.
%%
%%      If the `LcfUsers` argument is provided (i.e. not `undefined`), it will be used to generate the
%%      additional anonymized LCF tail property containing the count of LCF-ed users.
%%
%%      We use the following anonymization parameters:
%%
%%        - `#bucket.count':  this is the raw count of the users in the bucket without any noise added.
%%
%%        - `#bucket.noisy_count':  this is the noisy count we will report to the air (it will get
%%          constructed in many steps by `anonymize/1', thus the record value will contain temporary values
%%          while running `anonymize/1' and has to be initialized to #bucket.count).
%%
%%        - `#anonymizer_params.absolute_lower_bound':  this is the absolute lower bound for the number of
%%          users that have to be in a bucket for it not to get filtered out (an integer value).
%%
%%        - `#anonymizer_params.soft_lower_bound':  this is a soft lower bound for the number of users that
%%          have to be in a bucket for it not to get filtered out (an integer value).  A bucket gets filtered
%%          out if the noisy count given by adding a static noise with a sigma of `#anonymizer_params.sigma_soft_lower_bound'
%%          to the raw count, is less than or equal to the `#anonymizer_params.soft_lower_bound'.
%%
%%        - `#anonymizer_params.sigma_soft_lower_bound':  the standard deviation used when calculating a static noise
%%          value to add to the raw count to produce a value to compare against `#anonymizer_params.soft_lower_bound'.
%%
%%        - `#anonymizer_params.min_sigma':  a floating point value representing the minimum standard deviation of
%%          noise added to create `#bucket.noisy_count'.
%%
%%        - `#anonymizer_params.max_sigma': a floating point value representing the maximum standard deviation of
%%          noise added to create `#bucket.noisy_count'. This value is a soft bound. Exceeding it does not
%%          damage to privacy, but might hurt utility.
%%
%%      The following properties are fulfilled by the resulting buckets:
%%
%%        - `#bucket.count > #anonymizer_params.absolute_lower_bound'
%%
%%        - #bucket.count + noise with sigma `#anonymizer_params.sigma_soft_lower_bound' > `#anonymizer_params.soft_lower_bound'.
%%          Remark: The noise used in the soft lower bound comparison is computed independently of the noise used to generate the
%%          noisy counts that are reported. Thus the final reported `#bucket.noisy_count' could be less than
%%          `#anonymizer_params.soft_lower_bound'. If the noisy count is rounded down to 0 it will be filtered out though.
%%
%%        - Noise is proportional to the `#bucket.count', but we guarantee that at least noise with a
%%          standard deviation of `#anonymizer_params.min_sigma' is added.
%%
%%        - `#bucket.noisy_count > 0'.
%%
%%        - Each bucket returned by `anonymize/1' existed in the bucket list given to `anonymize/1'.
%%
%%        - `#bucket.noisy_count mod K ≡ 0' with `K' equals 5 or 10 depending on the noise added.
-spec anonymize([#bucket{}], undefined | lcf_users:lcf_users()) -> [#bucket{}].
anonymize(AggregatedBuckets, LcfUsers) ->
  BucketsWithAnonState = [append_anonymization_state(Bucket) || Bucket <- AggregatedBuckets],
  Params = default_params(),
  {PassedLcf, FilteredProperties} = filter_lcf(BucketsWithAnonState, Params),
  LcfTailReports = lcf_tail_reports(FilteredProperties, Params, LcfUsers),
  {FinalResults, _} = oportunistically_filter_reports(LcfTailReports ++ PassedLcf, Params, [
    fun apply_constant_noise/2,
    fun apply_proportional_random_noise/2,
    fun calculate_total_noise/2,
    fun cap_noise_to_4sd/2,
    fun round_count/2,
    fun remove_non_positive_buckets/2
  ]),
  [strip_anonymization_state(Result) || Result <- FinalResults].

%% @doc Return the default anonymizer parameters.
%%      The parameters are read from the configuration file.
-spec default_params() -> #anonymizer_params{}.
default_params() ->
  #anonymizer_params{
    absolute_lower_bound = cloak_conf:get_val(noise, absolute_lower_bound),
    sigma_soft_lower_bound = cloak_conf:get_val(noise, sigma_soft_lower_bound),
    soft_lower_bound = cloak_conf:get_val(noise, soft_lower_bound),
    target_error = cloak_conf:get_val(noise, target_error),
    max_sigma = cloak_conf:get_val(noise, max_sigma),
    min_sigma = cloak_conf:get_val(noise, min_sigma),
    constant_noise_sd = cloak_conf:get_val(noise, constant_noise_sd)
  }.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

-record(anonymization_state, {
  noise_sds=[],
  userids_hash
}).

filter_lcf(BucketsWithAnonState, Params) ->
  oportunistically_filter_reports(BucketsWithAnonState, Params, [
    fun absolute_low_count_filter/2,
    fun soft_low_count_filter/2
  ]).

lcf_tail_reports(_LcfFilteredProperties, _Params, undefined) -> [];
lcf_tail_reports(LcfFilteredProperties, Params, LcfUsers) ->
  case lcf_users:lcf_tail_report(LcfUsers, LcfFilteredProperties) of
    undefined -> [];
    LcfTailBucket ->
      {Passed, _Filtered} = filter_lcf([append_anonymization_state(LcfTailBucket)], Params),
      Passed
  end.

%% @doc Wraps the bucket with an anonymization state
%%      that is used internally while anonymizing.
append_anonymization_state(#bucket{users_hash = UsersHash} = Bucket) ->
  <<A:32, B:32, C:64>> = UsersHash,
  AnonymizationState = #anonymization_state{userids_hash = {A,B,C}},
  {Bucket, AnonymizationState}.

strip_anonymization_state({Bucket, _AnonymizationState}) -> Bucket.

remember_noise(Sd, #anonymization_state{noise_sds = ExistingNoise} = State) ->
  State#anonymization_state{noise_sds = [Sd | ExistingNoise]}.

%% @doc This function will try as hard as it can to reject buckets :)
%%      If a bucket is rejected, then at least it doesn't leak private
%%      information out of the system, that is the working motto.
%%      It will iteratively apply a test at a time on the buckets,
%%      for each test possibly discarding some of the buckets.
%%
%%      It is important that we run one test at a time over all reports,
%%      rather than applying all tests in a row to a single report,
%%      as some of the tests have to re-seed random and this would
%%      put a significant amount of strain on the clock if we constantly
%%      had to reseed it.
oportunistically_filter_reports(Reports, AnonymizationParameters, Tests) ->
  oportunistically_filter_reports(Reports, [], AnonymizationParameters, Tests).

oportunistically_filter_reports(Reports, FailedProperties, _AnonymizationParameters, []) -> {Reports, FailedProperties};
oportunistically_filter_reports(Reports, FailedProperties, AnonymizationParameters, [NextTest | PendingTests]) ->
  {ReportsThatPassedTheTest, NewFailedProperties} = lists:foldl(
    fun(Report, {PassedReports, NewFailedPropertiesAcc}) ->
      case NextTest(Report, AnonymizationParameters) of
        failed ->
          {Bucket, _} = Report,
          {PassedReports, [Bucket#bucket.property | NewFailedPropertiesAcc]};
        UpdatedReport ->
          {[UpdatedReport | PassedReports], NewFailedPropertiesAcc}
      end
    end,
    {[], []},
    Reports
  ),
  oportunistically_filter_reports(
    ReportsThatPassedTheTest,
    NewFailedProperties ++ FailedProperties,
    AnonymizationParameters,
    PendingTests
  ).


%% -------------------------------------------------------------------
%% Anonymization steps
%% -------------------------------------------------------------------

%% @doc We have an absolute low count filter.
%%      It is in place to make sure that an analyst can never
%%      learn something about properties that only pertain to very few (0 or 1)
%%      users.
absolute_low_count_filter({#bucket{count = Count}, _State},
    #anonymizer_params{absolute_lower_bound = LowerBound}) when Count =< LowerBound ->
  failed;
absolute_low_count_filter(Bucket, _) -> Bucket.

%% @doc The soft low-count filter prevents very small buckets from
%%      leaving the system. It is soft rather than fixed, to ensure
%%      that a malicious analyst cannot do attacks where the fact that
%%      a value is reported or not, tells him if his victim is in
%%      the result set or not.
soft_low_count_filter({#bucket{count = Count}, #anonymization_state{userids_hash = UserIdHashSeed}} = FullBucket,
    #anonymizer_params{sigma_soft_lower_bound = SigmaSoftLowerBound, soft_lower_bound = LowerBound}) ->
  NoisedCount = cloak_distributions:gauss_s(SigmaSoftLowerBound, Count, UserIdHashSeed),
  case NoisedCount =< LowerBound of
    true -> failed;
    false -> FullBucket
  end.

%% @doc We apply a layer of noise that is constant (i.e. it doesn't grow
%%      with the size of the answer), and which is tied to the particular
%%      users represented in the answer. It prevents an attacker from
%%      repeating a bucket over and over in hope of averaging out the
%%      noise and getting to the exact real answer.
apply_constant_noise({#bucket{noisy_count = Count} = Bucket,
    #anonymization_state{userids_hash = UserIdHashSeed} = AnonState},
    #anonymizer_params{constant_noise_sd = ConstantNoiseStandardDeviation}) ->
  NoisedCount = cloak_distributions:gauss_s(ConstantNoiseStandardDeviation, Count, UserIdHashSeed),
  UpdatedBucket = Bucket#bucket{noisy_count = NoisedCount},
  {UpdatedBucket, remember_noise(ConstantNoiseStandardDeviation, AnonState)}.

%% @doc We apply random noise to the answer, proportional to the size of the
%%      answer itself. This allows us to provide good accuracy while
%%      at the same time allowing individuals to be hidden in the crowd.
%%
%%      The noise generated has the maximum standard deviation possible that will
%%      keep the result in the target error range.
%%
%%      The total noise added (including the proportional noise) is capped to `#anonymizer_params.max_sigma'
apply_proportional_random_noise({#bucket{noisy_count = Count} = Bucket, AnonState}, AnonParams) ->
  Sigma = noise_sigma_for_count(Count, AnonParams),
  NoisyCount = cloak_distributions:gauss(Sigma, Count),
  {Bucket#bucket{noisy_count = NoisyCount}, remember_noise(Sigma, AnonState)}.

noise_sigma_for_count(Count, #anonymizer_params{min_sigma = MinSigma, max_sigma = MaxDesiredSigma,
    target_error = TargetError, constant_noise_sd = ConstantSigma}) ->
  %% we want the noisy count to have at most TargetError relative difference from the original count
  %% 99% of the values of a normal distribution are between +- 3 SD (68–95–99.7 rule)
  %% the SD is square root of the variance which is the sum of the squared errors => SD is target absolute error.
  MaxErrorSigma = Count * TargetError / 3,
  %% To keep the utility of the system high, we use the smaller one of the proportional SD and the largest
  %% standard deviation we want to use. Effectively this sets the MaxDesiredSigma as the upper bound of the
  %% standard deviation used for generating noise.
  MaxSigma = min(MaxErrorSigma, MaxDesiredSigma),
  %% compute final SD by taking the maximum of (the minimum set SD and and the minimum of (the maximum standard deviation
  %% allowed by the target error, and the maximum standard deviation allowing for good utility))
  math:sqrt(max(MinSigma * MinSigma, MaxSigma * MaxSigma - ConstantSigma * ConstantSigma)).

%% @doc We calculate the total noise resulting from applying multiple layers of noise
calculate_total_noise({Bucket, #anonymization_state{noise_sds = NoiseValues} = AnonState}, _AnonymizationParameters) ->
  TotalNoise = math:sqrt(lists:foldl(fun(N, Acc) -> N * N + Acc end, 0, NoiseValues)),
  {Bucket#bucket{noise_sd = TotalNoise}, AnonState}.

%% @doc We cap the noise added such that the noisy count stays within 4SD of the original count.
%%      This occurring is a very low probability event (~0.006%), and has very little measurable effect on
%%      the outcome of the anonymsation. What it does yield are strong bounds that help us test
%%      our implementation better.
cap_noise_to_4sd({#bucket{noise_sd = TotalNoiseSD, count = RawCount, noisy_count = NoisyCount} = Bucket,
    AnonState}, _AnonymizationParameters) ->
  AdjustedBucket = case abs(RawCount - NoisyCount) > 4 * TotalNoiseSD of
    true ->
      % We have added more noise than 4SD, cap it
      Sign = case RawCount < NoisyCount of
        true -> 1;
        false -> -1
      end,
      Bucket#bucket{noisy_count = RawCount + (Sign * TotalNoiseSD * 4)};
    false ->
      Bucket
  end,
  {AdjustedBucket, AnonState}.

%% @doc We round the noisy value to the nearest 5 or nearest 10 depending on the answer size.
%%      This provides marginal better anonymity properties as well as making it clear that
%%      there is some uncertainty present in the values reported.
round_count({#bucket{noisy_count = Count, noise_sd = Sigma} = Bucket, AnonState}, _AnonymizationParameters) ->
  {Bucket#bucket{noisy_count = round_noisy(Count, Sigma)}, AnonState}.

%% @doc Remove buckets with a non-positive noisy count.
remove_non_positive_buckets({#bucket{noisy_count = Count} = Bucket, AnonState}, _AnonymizationParameters) ->
  if
    Count =< 0 ->
      failed;
    Count > 0 ->
      {Bucket, AnonState}
  end.


%% -------------------------------------------------------------------
%% Noise function
%% -------------------------------------------------------------------

-spec round_noisy(integer(), integer()) -> integer().
round_noisy(Value, 0) -> Value;
round_noisy(Value, Sigma) when Sigma < 5 -> discretize(Value, 5);
round_noisy(Value, _Sigma) -> discretize(Value, 10).

-spec discretize(integer(), 5 | 10) -> integer().
discretize(Value, Step) -> round(Value / Step) * Step.


%% -------------------------------------------------------------------
%% Tests
%% -------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SIGMA_SOFT_LOWER_BOUND, 1).
-define(MAX_SIGMA, 20.0).
-define(MIN_SIGMA, 1.0).

anonymizer_params() ->
  #anonymizer_params{
    absolute_lower_bound = 1,
    sigma_soft_lower_bound = ?SIGMA_SOFT_LOWER_BOUND,
    soft_lower_bound = 3,
    target_error = 0.05,
    max_sigma = ?MAX_SIGMA,
    min_sigma = ?MIN_SIGMA,
    constant_noise_sd = 1
  }.

users() -> [<<"user1">>, <<"user2">>].

bucket(Count) ->
  #bucket{
    property = ["X", "Y"],
    count = Count,
    noisy_count = Count,
    users_hash = crypto:hash(md4, term_to_binary(lists:sort(users()))),
    noise_sd = 0
  }.

anon_params() ->
  anon_params([]).

anon_params(SDs) ->
  #anonymization_state{
    userids_hash={1,2,3},
    noise_sds=SDs
  }.

-define(bucket(Count), {bucket(Count), anon_params()}).
-define(
  bucket(Count, NoisyCount),
  {
    (bucket(Count))#bucket{noisy_count = NoisyCount},
    anon_params()
  }
).
-define(
  bucket(Count, NoisyCount, NoiseSD),
  {
    ((bucket(Count))#bucket{noisy_count = NoisyCount})#bucket{noise_sd = NoiseSD},
    anon_params()
  }
).
-define(
  bucket(Count, NoisyCount, NoiseSD, UsedSDs),
  {
    ((bucket(Count))#bucket{noisy_count = NoisyCount})#bucket{noise_sd = NoiseSD},
    anon_params(UsedSDs)
  }
).
-define(strip(Exp), strip_anonymization_state(Exp)).

-define(run(FunctionName, Input), FunctionName(Input, anonymizer_params())).

apply_constant_noise_test() ->
  F = fun(_) -> ?assertEqual(?bucket(4, 4, 0, [1]), ?run(apply_constant_noise, ?bucket(4))) end,
  lists:foreach(F, lists:seq(1,100)).

absolute_low_count_filter_test_() ->
  [
    ?_assertEqual(failed, ?run(absolute_low_count_filter, ?bucket(0))),
    ?_assertEqual(failed, ?run(absolute_low_count_filter, ?bucket(1))),
    ?_assertEqual(?bucket(2), ?run(absolute_low_count_filter,  ?bucket(2)))
  ].

soft_low_count_filter_test_() ->
  {setup,
   fun() ->
      meck:expect(cloak_distributions, gauss_s, fun(_SD, Count, _Seed) -> Count - 1 end)
   end,
   fun(_) -> meck:unload() end,
  [
    ?_assertEqual(failed, ?run(soft_low_count_filter, ?bucket(0))),
    ?_assertEqual(failed, ?run(soft_low_count_filter, ?bucket(4))),
    ?_assertEqual(?bucket(5), ?run(soft_low_count_filter,  ?bucket(5)))
  ]}.

noise_test_() ->
  {setup,
    fun() ->
      meck:new(cloak_distributions),
      meck:expect(cloak_distributions, gauss, fun(Sigma, N) -> round(N + Sigma) end),
      meck:expect(cloak_distributions, gauss_s, fun(Sigma, N, _Seed) -> round(N + Sigma) end)
    end,
    fun(_) ->
      ?assert(meck:validate(cloak_distributions)),
      meck:unload()
    end,
    [
      {"Constant noise", [
        ?_assertEqual(?bucket(4, 5, 0, [1]), ?run(apply_constant_noise, ?bucket(4))),
        ?_assertEqual(?bucket(5, 6, 0, [1]), ?run(apply_constant_noise, ?bucket(5)))
      ]},
      {"Variable noise", [
        {"Sigma = sqrt(3) for 120 users and target error 5%",
           ?_assertEqual(?bucket(120, 122, 0, [math:sqrt(3)]), ?run(apply_proportional_random_noise, ?bucket(120)))
        },
        {"Sigma = sqrt(15) for 240 users and target error 5%",
           ?_assertEqual(?bucket(240, 244, 0, [math:sqrt(15)]), ?run(apply_proportional_random_noise, ?bucket(240)))
        },
        {"Sigma ~= 20 (sqrt(400 - 1) because of static noise = 1) for 999999 users, because it is capped",
           ?_assertEqual(?bucket(999999, 1000019, 0, [
              math:sqrt(?MAX_SIGMA * ?MAX_SIGMA - ?SIGMA_SOFT_LOWER_BOUND * ?SIGMA_SOFT_LOWER_BOUND)]),
              ?run(apply_proportional_random_noise, ?bucket(999999)))
        },
        {"Fall back to min sigma = 1 for 30 users and target error 5%",
           ?_assertEqual(?bucket(30, 31, 0, [?MIN_SIGMA]), ?run(apply_proportional_random_noise, ?bucket(30)))
        }
      ]}
    ]
  }.

calculate_noise_total_test_() ->
  [
    ?_assertEqual(?bucket(0, 0, 4.0, [2,2,2,2]), ?run(calculate_total_noise, ?bucket(0, 0, 0, [2,2,2,2]))),
    ?_assertEqual(?bucket(0, 0, 6.0, [4,4,2]), ?run(calculate_total_noise, ?bucket(0, 0, 0, [4,4,2])))
  ].

cap_noise_to_4sd_test_() ->
  [
    ?_assertEqual(?bucket(100, 120, 5), ?run(cap_noise_to_4sd, ?bucket(100, 130, 5))),
    ?_assertEqual(?bucket(100, 80, 5), ?run(cap_noise_to_4sd, ?bucket(100, 5, 5))),
    ?_assertEqual(?bucket(100, 110, 5), ?run(cap_noise_to_4sd, ?bucket(100, 110, 5))),
    ?_assertEqual(?bucket(100, 90, 5), ?run(cap_noise_to_4sd, ?bucket(100, 90, 5)))
  ].

round_count_test_() ->
  [
    ?_assertEqual(?bucket(0, 15, 4), ?run(round_count, ?bucket(0, 15, 4))),
    ?_assertEqual(?bucket(0, 10, 4), ?run(round_count, ?bucket(0, 12, 4))),
    ?_assertEqual(?bucket(0, 20, 5), ?run(round_count, ?bucket(0, 15, 5))),
    ?_assertEqual(?bucket(0, 10, 5), ?run(round_count, ?bucket(0, 14, 5)))
  ].

remove_non_positive_buckets_test_() ->
  [
    ?_assertEqual(failed, ?run(remove_non_positive_buckets, ?bucket(0, 0))),
    ?_assertEqual(?bucket(0, 1), ?run(remove_non_positive_buckets, ?bucket(0, 1))),
    ?_assertEqual(failed, ?run(remove_non_positive_buckets, ?bucket(0, -1))),
    ?_assertEqual(?bucket(0, 2), ?run(remove_non_positive_buckets, ?bucket(0, 2)))
  ].

end_to_end_test_() ->
  {setup,
    fun() ->
      meck:new(cloak_distributions),
      meck:expect(cloak_distributions, gauss, fun(Sigma, N) -> round(N + Sigma) end),
      meck:expect(cloak_distributions, gauss_s, fun(Sigma, N, _Seed) -> round(N + Sigma) end)
    end,
    fun(_) ->
      ?assert(meck:validate(cloak_distributions)),
      meck:unload()
    end,
    [
      {"Too small counts get filtered out", [
        ?_assertEqual([], anonymize([?strip(?bucket(0)), ?strip(?bucket(1)), ?strip(?bucket(2))]))
      ]},

      {"Values that fall below the soft threshold after noise should be removed", [
        %% Since we are doing some meck trickery here, the constant + variable noise = 3.5 (1.5 + 2)
        %% The soft threshold that should be exceeded in order to get a bucketed value is 7 (5 + 2)
        ?_assertEqual([], anonymize([?strip(?bucket(2)), ?strip(?bucket(3))]))
      ]},

      {"Noise should be applied and answers rounded", [
        ?_assertEqual([?strip(?bucket(100, 105, 2.5))], anonymize([?strip(?bucket(100))])),
        ?_assertEqual([?strip(?bucket(120, 125, 2.5))], anonymize([?strip(?bucket(120))])),
        ?_assertEqual([?strip(?bucket(400, 405, 2.5))], anonymize([?strip(?bucket(400))])),
        ?_assertEqual([?strip(?bucket(1800, 1810, 6.006666666666668))], anonymize([?strip(?bucket(1800))]))
      ]}
    ]
  }.

target_error_test() ->
  % For the default settings (target_error=1%,min_sigma=2,constant_noise=1.5)
  % the minimum count for which we can respect the target error is 750.
  Buckets = [bucket(750), bucket(997), bucket(1567), bucket(2345), bucket(8345)],
  AnonBuckets = lists:flatten([anonymizer:anonymize(Buckets) || _ <- lists:seq(1,100)]),
  AnonParams = default_params(),
  TargetError = AnonParams#anonymizer_params.target_error,
  IsUnderTargetError = fun(#bucket{count = Count, noisy_count = NoisyCount}) ->
      % Rounding to multiples of 10 will add at most 5 to the noise.
      abs(NoisyCount - Count) - 5 =< Count * TargetError
    end,
  % we need 99% of the results to be under the target error (68–95–99.7 rule)
  true = length(lists:filter(IsUnderTargetError, AnonBuckets)) > length(Buckets) * 99.

lcf_test_() ->
  {
    setup,
    fun() ->
      meck:new(cloak_distributions),
      meck:expect(cloak_distributions, gauss, fun(_Sigma, N) -> round(N) end),
      meck:expect(cloak_distributions, gauss_s, fun(_Sigma, N, _Seed) -> round(N) end)
    end,
    fun(_) ->
      meck:unload()
    end,
    [
      {"lcf tail is reported", ?_assertEqual(8, (lcf_tail([{p1, 1, 4}, {p2, 5, 8}]))#bucket.count)},
      {"lcf tail count has noise",
        % Although we have 8 users, the noisy count should be 10, due to discretization
        ?_assertEqual(10, (lcf_tail([{p3, 11, 14}, {p4, 15, 18}]))#bucket.noisy_count)},
      % Too few entries in the lcf tail to be reported
      {"lcf tail is lcf-ed", ?_assertEqual(undefined, lcf_tail([{p5, 21, 24}]))},
      % User 34 is in two buckets, so we should have 7 users in the lcf tail
      {"users are deduplicated", ?_assertEqual(7, (lcf_tail([{p6, 31, 34}, {p7, 34, 37}]))#bucket.count)},
      % A large bucket including all users shouldn't affect the lcf tail
      {"only lcf-ed properties are included in lcf tail",
        ?_assertEqual(8, (lcf_tail([{p8, 41, 44}, {p9, 45, 48}, {p10, 41, 100}]))#bucket.count)}
    ]
  }.

lcf_tail(Properties) ->
  LcfUsers = lcf_users:new(),
  Aggregator = aggregator:new(LcfUsers),
  try
    add_buckets(Aggregator, Properties),
    case [Bucket ||
      #bucket{
        property = [?AIRCLOAK_LABEL, ?LCF_TAIL_VALUE]
      } = Bucket <- anonymizer:anonymize(aggregator:buckets(Aggregator), LcfUsers)
    ] of
      [] -> undefined;
      [Count] -> Count
    end
  after
    aggregator:delete(Aggregator),
    lcf_users:delete(LcfUsers)
  end.

add_buckets(Aggregator, Properties) ->
  [
    aggregator:add_property(
      [atom_to_binary(Property, latin1)],
      iolist_to_binary(io_lib:format("u~p", [UserIndex])),
      Aggregator
    ) || {Property, From, To} <- Properties, UserIndex <- lists:seq(From, To)
  ].

-endif.
