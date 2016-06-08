defmodule Cloak.Processor.Noise do
  def passes_filter?(count, users) do
    count > absolute_lower_bound() && noisy_count(count, users) > soft_lower_bound()
  end

  defp noisy_count(count, users) do
    :cloak_distributions.gauss_s(sigma_soft_lower_bound(), count, random_seed(users))
  end

  defp random_seed(users) do
    users
    |> Enum.sort()
    |> :erlang.term_to_binary()
    |> compute_hash()
    |> binary_to_seed()
  end

  defp compute_hash(binary), do: :crypto.hash(:md4, binary)

  defp binary_to_seed(binary) do
    <<a::32, b::32, c::64>> = binary
    {a, b, c}
  end

  defp absolute_lower_bound, do: noise_config(:absolute_lower_bound)

  defp soft_lower_bound, do: noise_config(:soft_lower_bound)

  defp sigma_soft_lower_bound, do: noise_config(:sigma_soft_lower_bound)

  defp noise_config(name), do: Application.get_env(:cloak, :noise) |> Keyword.get(name)
end
