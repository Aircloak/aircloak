defmodule DataGenerator do
  def generate(size) do
    users = Enum.map(1..size, fn _ -> generate_user() end)
    purchases = Enum.flat_map(users, &generate_purchases/1)

    {users, purchases}
  end

  def write(data, filename) do
    with {:ok, file} <- File.open(filename, [:write]) do
      IO.puts(file, data |> Stream.map(&JSON.encode!/1) |> Enum.join("\n"))
    end
  end

  def generate_user() do
    %{
      id: "id-#{:erlang.unique_integer([:positive])}",
      name: "user-#{:rand.uniform(1_000_000_000)}"
    }
  end

  def generate_purchases(user) do
    for _ <- 1..10 do
      %{
        user_id: user.id,
        date: %{"$date" => "#{NaiveDateTime.utc_now() |> NaiveDateTime.to_iso8601()}Z"}
      }
    end
  end
end

{users, purchases} = DataGenerator.generate(10)

DataGenerator.write(users, "/tmp/users.json")
DataGenerator.write(purchases, "/tmp/purchases.json")
