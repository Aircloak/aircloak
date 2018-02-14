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

  @min_addresses 0
  @max_addresses 2
  @min_postal_code 10_000
  @max_postal_code 11_000
  @min_notes 0
  @max_notes 3
  @min_note_changes 0
  @max_note_changes 3
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
    words = words()
    names = names()
    cities = cities()

    1..num_users
    |> Task.async_stream(&generate_user(&1, words, names, cities), timeout: :timer.minutes(10))
    |> Stream.map(fn({:ok, user_data}) -> user_data end)
  end

  @doc "Flattens the data down into structures that can be inserted into a relational database."
  @spec flatten(Map.t) :: Map.t
  def flatten(users) do
    %{
      users: flatten_users(users),
      addresses: flatten_addresses(users),
      notes: flatten_notes(users),
      notes_changes: flatten_notes_changes(users),
    }
  end

  @doc "Regroups a dataset into set of collections. No flattening occurs. Useful for document stores."
  @spec to_collections(Map.t) :: Map.t
  def to_collections(users) do
    %{
      users: users_to_collection(users),
      addresses: addresses_to_collection(users),
      notes: notes_to_collection(users),
    }
  end

  @doc "Returns the encryption key used when encrypting the data"
  @spec encryption_key() :: String.t
  def encryption_key(), do: @encryption_key


  # -------------------------------------------------------------------
  # Internal functions - data generation
  # -------------------------------------------------------------------

  defp generate_user(user_num, words, names, cities) do
    :rand.seed(:exsplus, {0, 0, user_num})

    user =
      %{
        id: user_num,
        user_id: :erlang.unique_integer([:positive]),
        name: generate_name(names),
        age: :rand.uniform(70) + 10,
        height: :rand.uniform() * 30 + 170,
        active: :rand.uniform() < 0.80,
        addresses: generate_addresses(cities),
        notes: generate_notes(words),
        nullable: nullable(:rand.uniform() * 30),
      }
    {user, encode_user(user)}
  end

  defp nullable(item), do:
    if :rand.uniform() < 0.80, do: item, else: nil

  defp generate_addresses(cities) do
    for _ <- rand_range(@min_addresses, @max_addresses) do
      %{
        home: %{
          city: sample_one(cities),
          postal_code: random_postcode(),
        },
        work: %{
          city: sample_one(cities),
          postal_code: random_postcode(),
        }
      }
    end
  end

  defp generate_notes(words) do
    for _ <- rand_range(@min_notes, @max_notes) do
      note_id = :erlang.unique_integer([:positive, :monotonic])
      %{
        id: note_id,
        note_id: note_id,
        title: sample_randomly(words, 2, 10),
        content: sample_randomly(words, 100, 200),
        changes: generate_note_changes(note_id, words),
      }
    end
  end

  defp generate_note_changes(note_id, words) do
    for _ <- rand_range(@min_note_changes, @max_note_changes) do
      %{
        note_id: note_id,
        date: random_date(),
        change: sample_randomly(words, 5, 50),
      }
    end
  end

  defp random_date(), do:
    1_500_000_000 + :rand.uniform(100_026_704) |> DateTime.from_unix!() |> DateTime.to_naive()

  defp rand_range(min, max) when min > max, do:
    raise "Max must be greater or equal to min"
  defp rand_range(min, max), do:
    min..(:rand.uniform(max - min + 1) + min)

  defp cities(), do:
    lines_from_file("lib/compliance/cities.txt")

  defp random_postcode(), do:
    :rand.uniform(@max_postal_code - @min_postal_code) + @min_postal_code

  defp generate_name(names), do:
    sample_randomly(names, 2, 3)

  defp sample_randomly(samples, min, max), do:
    rand_range(min, max)
    |> Enum.map(fn(_) -> sample_one(samples) end)
    |> Enum.join(" ")

  defp sample_one(options) do
    Map.fetch!(options, :rand.uniform(Map.size(options) - 1))
  end

  defp words(), do:
    lines_from_file("lib/compliance/words.txt")

  defp names(), do:
    lines_from_file("lib/compliance/names.txt")

  defp lines_from_file(file), do:
    File.read!(file)
    |> String.split("\n")
    |> Enum.reverse()
    # Get rid of empty entry due to empty line at the end of file
    |> tl()
    |> Stream.with_index()
    |> Stream.map(fn({word, index}) -> {index, word} end)
    |> Map.new()


  # -------------------------------------------------------------------
  # Internal functions - flattening to tables
  # -------------------------------------------------------------------

  defp users_to_collection(users), do:
    flatten_users(users)

  defp addresses_to_collection(users), do:
    Enum.flat_map(users, fn(user) ->
      for address <- user.addresses, do:
        Map.put(address, :user_fk, user.id)
    end)

  defp notes_to_collection(users), do:
    Enum.flat_map(users, fn(user) ->
      for note <- user.notes, do:
        Map.put(note, :user_fk, user.id)
    end)

  defp flatten_users(users), do:
    Enum.map(users, & Map.take(&1, [:id, :user_id, :name, :age, :height, :active, :nullable]))

  defp flatten_addresses(users) do
    Enum.flat_map(users, fn(user) ->
      for address <- user.addresses do
        %{
          user_fk: user.id,
          'home.city': address.home[:city],
          'home.postal_code': address.home[:postal_code],
          'work.city': address.work[:city],
          'work.postal_code': address.work[:postal_code],
        }
      end
    end)
  end

  defp flatten_notes(users) do
    Enum.flat_map(users, fn(user) ->
      for note <- user.notes do
        note
        |> Map.take([:id, :title, :content])
        |> Map.merge(%{user_fk: user.id})
      end
    end)
  end

  defp flatten_notes_changes(users) do
    Enum.flat_map(users, fn(user) ->
      Enum.flat_map(user.notes, fn(note) ->
        for change <- note.changes, do:
          %{
            id: note.id,
            note_id: change.note_id,
            user_fk: user.id,
            title: note.title,
            content: note.content,
            'changes.change': change.change,
            'changes.date': change.date,
          }
      end)
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions - data encoding
  # -------------------------------------------------------------------

  defp encrypt_keys(map, keys), do:
    Enum.reduce(fixup_keys(keys), map, &update_in(&2, &1, fn(v) -> v |> encrypt() |> base64() end))

  defp stringify_keys(map, keys), do:
    Enum.reduce(fixup_keys(keys), map, &update_in(&2, &1, fn
      nil -> nil
      other -> to_string(other)
    end))

  defp fixup_keys([[_|_] | _] = keys), do: keys
  defp fixup_keys(keys), do:
    Enum.map(keys, & [&1])

  defp encrypt(value), do:
    :crypto.block_encrypt(:aes_cbc128, @encryption_key, @zero_iv, pad(value))

  @block_size 16
  def pad(value) do
    pad_value = @block_size - rem(byte_size(value), @block_size)
    value <> String.duplicate(<<pad_value::8>>, pad_value)
  end

  defp base64(value), do:
    Base.encode64(value)

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
