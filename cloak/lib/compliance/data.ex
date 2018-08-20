defmodule Compliance.Data do
  @moduledoc """
  Generates a database independent dataset that can be imported into
  backends for compliance testing.

  The generated data follows the following outline:
  - We generate a list of users
  - Each user may have a number of addresses, each consisting of a work and home address
  - Each user may have a list of notes
  - Each note may have a list of changes made to the notes

  If you change the schema you will need to update `Compliance.TableDefinitions`.
  """

  @external_resource "lib/compliance/words.txt"
  @external_resource "lib/compliance/names.txt"
  @external_resource "lib/compliance/cities.txt"

  @min_addresses 0
  @max_addresses 2
  @min_postal_code 10_000
  @max_postal_code 11_000
  @min_notes 0
  @max_notes 2
  @min_note_changes 0
  @max_note_changes 2
  @encryption_key "1234567890ABCDEF"
  @zero_iv String.duplicate(<<0>>, 16)

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Returns a lazy random enumerable of users that can be imported into a database for compliance testing.
  For more information on the structure and expected usage, see the module doc.
  """
  @spec users(non_neg_integer) :: Enumerable.t()
  def users(num_users) do
    samples = samples()

    1..num_users
    |> Task.async_stream(&generate_user(&1, samples), timeout: :timer.minutes(10))
    |> Stream.map(fn {:ok, user_data} -> user_data end)
  end

  @doc "Flattens the data down into structures that can be inserted into a relational database."
  @spec flatten(Map.t()) :: Map.t()
  def flatten(users) do
    %{
      users: flatten_users(users),
      addresses: flatten_addresses(users),
      notes: flatten_notes(users),
      notes_changes: flatten_notes_changes(users)
    }
  end

  @doc "Regroups a dataset into set of collections. No flattening occurs. Useful for document stores."
  @spec to_collections(Map.t()) :: Map.t()
  def to_collections(users) do
    %{
      users: users_to_collection(users),
      addresses: addresses_to_collection(users),
      notes: notes_to_collection(users)
    }
  end

  @doc "Returns the encryption key used when encrypting the data"
  @spec encryption_key() :: String.t()
  def encryption_key(), do: @encryption_key

  # -------------------------------------------------------------------
  # Internal functions - data generation
  # -------------------------------------------------------------------

  defp generate_user(user_num, samples) do
    :rand.seed(:exsplus, {0, 0, user_num})

    user = %{
      id: user_num,
      user_id: :erlang.unique_integer([:positive]),
      name: generate_name(samples.names),
      age: sample_one(samples.ages),
      height: sample_one(samples.heights),
      active: :rand.uniform() < 0.80,
      addresses: generate_addresses(samples),
      notes: generate_notes(samples),
      nullable: samples.floats |> sample_one() |> nullable(),
      birthday: samples.dates |> sample_one() |> NaiveDateTime.to_date()
    }

    {user, encode_user(user)}
  end

  defp nullable(item), do: if(:rand.uniform() < 0.80, do: item, else: nil)

  defp generate_addresses(samples) do
    for _ <- rand_range(@min_addresses, @max_addresses) do
      %{
        home: %{
          city: sample_one(samples.cities),
          postal_code: sample_one(samples.postcodes)
        },
        work: %{
          city: sample_one(samples.cities),
          postal_code: sample_one(samples.postcodes)
        }
      }
    end
  end

  defp generate_notes(samples) do
    for _ <- rand_range(@min_notes, @max_notes) do
      note_id = :erlang.unique_integer([:positive, :monotonic])

      %{
        id: note_id,
        note_id: note_id,
        title: sample_randomly(samples.words, 2, 10),
        content: sample_randomly(samples.words, 0, 10),
        changes: generate_note_changes(note_id, samples)
      }
    end
  end

  defp generate_note_changes(note_id, samples) do
    for _ <- rand_range(@min_note_changes, @max_note_changes) do
      %{
        note_id: note_id,
        date: sample_one(samples.dates),
        change: sample_randomly(samples.words, 0, 10)
      }
    end
  end

  defp rand_range(min, max) when max >= min, do: min..(:rand.uniform(max - min + 1) + min - 1)

  defp generate_name(names), do: sample_one(names)

  defp sample_randomly(samples, min, max),
    do:
      rand_range(min, max)
      |> Enum.map(fn _ -> sample_one(samples) end)
      |> Enum.join(" ")

  defp sample_one(options) do
    Map.fetch!(options, :rand.uniform(Map.size(options) - 1))
  end

  # -------------------------------------------------------------------
  # Internal functions - data samples
  # -------------------------------------------------------------------

  defp samples() do
    %{
      words: words(),
      names: names(),
      cities: cities(),
      ages: ages(),
      heights: heights(),
      postcodes: postcodes(),
      dates: dates(),
      floats: floats()
    }
  end

  lines_from_file = fn file ->
    File.read!(file)
    |> String.split("\n")
    |> Enum.reverse()
    # Get rid of empty entry due to empty line at the end of file
    |> tl()
    |> Stream.with_index()
    |> Stream.map(fn {word, index} -> {index, word} end)
    |> Map.new()
    |> Macro.escape()
  end

  defp words(), do: unquote(lines_from_file.("lib/compliance/words.txt"))

  defp names(), do: unquote(lines_from_file.("lib/compliance/names.txt"))

  defp cities(), do: unquote(lines_from_file.("lib/compliance/cities.txt"))

  defp ages(), do: sample(fn -> :rand.uniform(70) + 10 end)

  defp heights(), do: sample(fn -> 170 + :rand.uniform(30) + random_float(2) end)

  defp floats(), do: sample(fn -> random_sign() * random_float(6) end)

  defp random_float(num_digits) do
    # Generates random float with desired number of digits. No digit will have the value of 0, 5, or 9, to reduce the
    # chance of rounding inconsistencies, which can happen due to the way floats are stored in the database.
    1..num_digits
    |> Enum.reduce(0, fn _, acc -> 10 * acc + Enum.random([1, 2, 3, 4, 6, 7, 8]) end)
    |> Kernel./(:math.pow(10, num_digits))
  end

  defp random_sign(), do: Enum.random([-1, 1])

  defp postcodes(), do: sample(fn -> :rand.uniform(@max_postal_code - @min_postal_code) + @min_postal_code end)

  defp dates() do
    sample(fn -> (1_500_000_000 + :rand.uniform(100_026_704)) |> DateTime.from_unix!() |> DateTime.to_naive() end)
  end

  defp sample(fun), do: 0..9 |> Enum.map(&{&1, fun.()}) |> Map.new()

  # -------------------------------------------------------------------
  # Internal functions - flattening to tables
  # -------------------------------------------------------------------

  defp users_to_collection(users), do: flatten_users(users)

  defp addresses_to_collection(users),
    do:
      Enum.flat_map(users, fn user ->
        for address <- user.addresses, do: Map.put(address, :user_fk, user.id)
      end)

  defp notes_to_collection(users),
    do:
      Enum.flat_map(users, fn user ->
        for note <- user.notes, do: Map.put(note, :user_fk, user.id)
      end)

  defp flatten_users(users),
    do: Enum.map(users, &Map.take(&1, [:id, :user_id, :name, :age, :height, :active, :nullable, :birthday]))

  defp flatten_addresses(users) do
    Enum.flat_map(users, fn user ->
      for address <- user.addresses do
        %{
          user_fk: user.id,
          "home.city": address.home[:city],
          "home.postal_code": address.home[:postal_code],
          "work.city": address.work[:city],
          "work.postal_code": address.work[:postal_code]
        }
      end
    end)
  end

  defp flatten_notes(users) do
    Enum.flat_map(users, fn user ->
      for note <- user.notes do
        note
        |> Map.take([:id, :title, :content])
        |> Map.merge(%{user_fk: user.id})
      end
    end)
  end

  defp flatten_notes_changes(users) do
    Enum.flat_map(users, fn user ->
      Enum.flat_map(user.notes, fn note ->
        for change <- note.changes,
            do: %{
              id: note.id,
              note_id: change.note_id,
              title: note.title,
              content: note.content,
              "changes.change": change.change,
              "changes.date": change.date
            }
      end)
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions - data encoding
  # -------------------------------------------------------------------

  defp encrypt_keys(map, keys),
    do:
      Enum.reduce(
        fixup_keys(keys),
        map,
        &update_in(&2, &1, fn v -> v |> encrypt() |> base64() end)
      )

  defp stringify_keys(map, keys),
    do:
      Enum.reduce(
        fixup_keys(keys),
        map,
        &update_in(&2, &1, fn
          nil -> nil
          other -> to_string(other)
        end)
      )

  defp fixup_keys([[_ | _] | _] = keys), do: keys
  defp fixup_keys(keys), do: Enum.map(keys, &[&1])

  defp encrypt(value), do: :crypto.block_encrypt(:aes_cbc128, @encryption_key, @zero_iv, pad(value))

  @block_size 16
  def pad(value) do
    pad_value = @block_size - rem(byte_size(value), @block_size)
    value <> String.duplicate(<<pad_value::8>>, pad_value)
  end

  defp base64(value), do: Base.encode64(value)

  defp encode_user(user) do
    user
    |> encrypt_keys([:name])
    |> stringify_keys([:age, :height, :active, :nullable])
    |> Map.put(:addresses, encode_addresses(user[:addresses]))
    |> Map.put(:notes, encode_notes(user[:notes]))
  end

  defp encode_addresses(addresses) do
    for address <- addresses do
      address
      |> encrypt_keys([[:home, :city], [:work, :city]])
      |> stringify_keys([[:home, :postal_code], [:work, :postal_code]])
    end
  end

  defp encode_notes(notes) do
    for note <- notes do
      note
      |> encrypt_keys([:title, :content])
      |> Map.put(:changes, encode_notes_changes(note[:changes]))
    end
  end

  defp encode_notes_changes(changes) do
    for change <- changes do
      change
      |> encrypt_keys([:change])
      |> stringify_keys([:date])
    end
  end
end
