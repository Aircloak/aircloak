defmodule DataGenerator do
  def generate(size) do
    users = Enum.map(1..size, fn _ -> generate_user() end)
    purchases = Enum.flat_map(users, &generate_purchases/1)
    items = Enum.flat_map(purchases, &generate_items/1)

    {users, purchases, items}
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
    for _ <- 0..:rand.uniform(20) do
      %{
        id: "id-#{:erlang.unique_integer([:positive])}",
        user_id: user.id,
        date: %{"$date" => "#{NaiveDateTime.utc_now() |> NaiveDateTime.to_iso8601()}Z"}
      }
    end
  end

  def generate_items(purchase) do
    for _ <- 0..:rand.uniform(20) do
      %{
        purchase_id: purchase.id,
        itemname: "widget-#{:rand.uniform(1000)}",
        quantity: :rand.uniform(100) + 1,
        price: :rand.uniform()
      }
    end
  end
end

size = (System.get_env("SIZE") || "10") |> String.to_integer()
{users, purchases, items} = DataGenerator.generate(size)

DataGenerator.write(users, "/tmp/users.json")
DataGenerator.write(purchases, "/tmp/purchases.json")
DataGenerator.write(items, "/tmp/items.json")
