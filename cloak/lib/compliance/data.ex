defmodule Compliance.Data do
  @moduledoc """
  Generates a database independent dataset that can be imported into
  backends for compliance testing.

  The generated data follows the following outline:
  - We generate a list of users
  - Each user may have a number of addresses, each consisting of a work and home address
  - Each user may have a list of notes
  - Each note may have a list of changes made to the notes

  If you update the schema, please make sure the reference schema on queries_test.exs is still
  correct and up to date!
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
  Generates a random dataset that can be imported into a database for compliance testing.
  For more information on the structure and expected usage, see the module doc.
  """
  @spec generate(non_neg_integer) :: {Map.t, Map.t}
  def generate(num_users) do
    normal = generate_users(num_users)
    encoded = encode(normal)
    {normal, encoded}
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

  @doc "Returns the encryption key used when encrypting the data"
  @spec encryption_key() :: String.t
  def encryption_key(), do: @encryption_key


  # -------------------------------------------------------------------
  # Internal functions - data generation
  # -------------------------------------------------------------------

  defp generate_users(num_users) do
    words = words()
    names = names()
    cities = cities()

    output_progress(0, num_users)

    Enum.to_list(1..num_users)
    |> Task.async_stream(fn(user_num) ->
      :rand.seed(:exsplus, {0, 0, user_num})
      %{
        id: user_num,
        user_id: :erlang.unique_integer([:positive]),
        name: generate_name(names),
        age: :rand.uniform(70) + 10,
        height: :rand.uniform() * 30 + 170,
        active: :rand.uniform() < 0.80,
        addresses: generate_addresses(cities),
        notes: generate_notes(words),
      }
    end)
    |> Stream.map(fn({:ok, user}) ->
      output_progress(user.id, num_users)
      user
    end)
    |> Enum.to_list()
  end

  defp output_progress(num, total) do
    percent = round((num/total) * 100)
    :io.format(to_charlist("Generating users #{percent}% complete.\r"))
  end

  defp generate_addresses(cities) do
    for _ <- rand_range_list(@min_addresses, @max_addresses) do
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
    for _ <- rand_range_list(@min_notes, @max_notes) do
      note_id = :erlang.unique_integer([:positive, :monotonic])
      %{
        id: note_id,
        title: sample_randomly(words, 2, 10),
        content: sample_randomly(words, 100, 200),
        changes: generate_note_changes(note_id, words),
      }
    end
  end

  defp generate_note_changes(note_id, words) do
    for _ <- rand_range_list(@min_note_changes, @max_note_changes) do
      %{
        note_id: note_id,
        date: random_date(),
        change: sample_randomly(words, 5, 50),
      }
    end
  end

  defp random_date(), do:
    1_500_000_000 + :rand.uniform(100_026_704) |> DateTime.from_unix!() |> DateTime.to_naive()

  defp rand_range_list(val, val), do: [val]
  defp rand_range_list(min, max), do:
    (min..:rand.uniform(max - min) + min)
    |> Enum.to_list()

  defp cities(), do:
    lines_from_file("lib/compliance/cities.txt")

  defp random_postcode(), do:
    :rand.uniform(@max_postal_code - @min_postal_code) + @min_postal_code

  defp generate_name(names), do:
    sample_randomly(names, 2, 3)

  defp sample_randomly(samples, min, max), do:
    rand_range_list(min, max)
    |> Enum.map(fn(_) -> sample_one(samples) end)
    |> Enum.join(" ")

  defp sample_one(options), do:
    Enum.random(options)

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


  # -------------------------------------------------------------------
  # Internal functions - flattening to tables
  # -------------------------------------------------------------------

  defp flatten_users(users), do:
    Enum.map(users, & Map.take(&1, [:id, :user_id, :name, :age, :height, :active]))

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
    Enum.reduce(fixup_keys(keys), map, &update_in(&2, &1, fn(v) -> to_string(v) end))

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

  defp encode(users) do
    for user <- users do
      user
      |> encrypt_keys([:name])
      |> stringify_keys([:age, :height, :active])
      |> Map.put(:addresses, encode_addresses(user[:addresses]))
      |> Map.put(:notes, encode_notes(user[:notes]))
    end
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
