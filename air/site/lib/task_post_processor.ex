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
        Logger.error("Error during article post-processing: #{inspect reason}")
        original_result
    end
  end
end
