-include("cloak_logging.hrl").
-include("debug_helpers.hrl").

-type user_id() :: binary().
-type query_id() :: binary().
-type property() :: 'Elixir.Cloak.DataSource':row().

-record(bucket, {
  property :: property(),
  count :: non_neg_integer(),
  noisy_count :: number(),
  users_hash :: binary(),
  %% The size of the standard deviation used, to give the analyst a sense of the noise in the result
  noise_sd :: number()
}).

%% This record contains the parameters for the anonymizer.
-record(anonymizer_params, {
  %% The minimum number of users that must be in a bucket to get reported.
  absolute_lower_bound :: pos_integer(),

  %% After adding noise by with sigma sigma_soft_lower_bound to the number of users in a bucket this noisy count has
  %% to be greater than soft_lower_bound to get reported.
  soft_lower_bound :: pos_integer(),
  sigma_soft_lower_bound :: float(),

  %% target_error: The target error for which the anonymization engine will add noise to the results.
  target_error :: float(),

  %% Bounds for the noise added to anonymized results.
  %% min_sigma is the abosolute minimum standard deviation used, whereas max_sigma
  %% is more of a soft bound. Exceeding max_sigma will hurt usability, but not reduce the privacy
  %% protection provided by the system.
  min_sigma :: float(),
  max_sigma :: float(),

  %% The anonymized results contain a layer of noise that is constant and unique to
  %% the bucket. The noise is normal with a certain standard deviation.
  constant_noise_sd :: float(),

  %% Keeps state about which users have been low count filtered, and how many times.
  %% Allows us to later generate an anonymized property indicating how many user
  %% properties have been removed due to anonymization
  lcf_data :: 'Elixir.Cloak.LCFData':t()
}).
