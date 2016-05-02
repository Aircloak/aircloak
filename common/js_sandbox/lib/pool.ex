defmodule Aircloak.JsSandbox.Pool do
  def start_link(js_folder, name \\ nil, pool_args \\ default_pool_args()) do
    :poolboy.start_link(pool_args_with_name(name, pool_args), js_modules(js_folder))
  end

  def call(pool, function, arguments) do
    worker = :poolboy.checkout(pool, true, :infinity)
    try do
      :js_vm_worker.call(worker, function, arguments)
    after
      :poolboy.checkin(pool, worker)
    end
  end

  defp js_modules(js_folder) do
    Path.wildcard("#{js_folder}/*.js")
  end

  defp pool_args_with_name(nil, pool_args), do: pool_args
  defp pool_args_with_name(name, pool_args), do: [name: {:local, name}] ++ pool_args

  defp default_pool_args() do
    [worker_module: :js_vm_worker, size: default_pool_size(), max_overflow: 0]
  end

  defp default_pool_size() do
    case :erlang.system_info(:logical_processors_available) do
      :unknown -> 2 # if we can't determine CPU count, assume 2
      count -> count
    end
  end
end
