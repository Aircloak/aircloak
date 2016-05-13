defmodule Air.TaskPostProcessor do
  @moduledoc """
  Post-processing of tasks result.

  Post-processing is performed in a JavaScript sandbox which is powered by the
  `JsSandbox.Pool`. The post-processing code resides in the
  `priv/task_post_processor` folder.
  """

  alias Air.JsSandbox
  require Logger

  @doc "Starts the pool of post-processing JavaScript sandbox workers."
  @spec start_link() :: GenServer.on_start
  def start_link() do
    JsSandbox.Pool.start_link(
      Application.app_dir(:air, "priv/task_post_processor"),
      __MODULE__
    )
  end

  @doc "Processes a raw result sent by the cloak."
  @spec process(%{String.t => any}) :: %{String.t => any}
  def process(original_result) do
    case JsSandbox.Pool.call(__MODULE__, "process_result", [original_result]) do
      {:ok, post_processed_result} ->
        post_processed_result
      {:error, reason} ->
        # log error and add it to the exceptions list
        Logger.error("Error during result post-processing: #{reason}")
        exception = %{"error" => "post-processing failed: #{reason}", "count" => 1}
        %{original_result | "exceptions" => [exception | original_result["exceptions"]]}
    end
  end
end
