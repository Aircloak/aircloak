defmodule Air.PsqlServer.Protocol do
  @moduledoc """
  PostgreSQL server protocol implemented as a state machine.

  This module handles the workflow of the
  [PostgreSQL server protocol v3.0](https://www.postgresql.org/docs/9.6/static/protocol.html).

  The module is implemented as a pure passive state machine, and doesn't deal with
  temporal logic, TCP communication, interaction with the system, or other side-effects.
  These are left to the driver of the module (e.g. TCP server, or a test). For an example
  usage see `Air.PsqlServer.RanchServer`.

  The general usage flow starts by creating the initial protocol state with `new/0`.
  Then, the driver must feed input bytes to the state with `process/2`. As the result,
  the protocol will change its internal state and produce output actions which the driver must
  obtain using `actions/1` and interpret them.
  """

  import Air.PsqlServer.Protocol.Messages

  @opaque t :: %{
    name: :atom,
    buffer: binary,
    expecting: non_neg_integer,
    actions: [action]
  }

  @type action ::
    {:send, iodata()} |
    {:close, reason :: any} |
    {:login_params, map} |
    {:authenticate, password :: binary}

  @type authentication_method :: :cleartext

  @doc "Creates the initial protocol state."
  @spec new() :: t
  def new() do
    %{
      name: :initial,
      buffer: "",
      expecting: 8,
      actions: []
    }
  end


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Returns the list of actions which the driver must perform.

  This function returns following actions:

  - `{:send, iodata()}` - The driver must send iodata to the client.
  - `{:close, reason :: any}` - The driver must close the connection
  - `{:login_params, map}` - The driver must store login params, and select the
    authentication method with `authentication_method/2`.
  - `{:authenticate, password :: binary}` - The driver must authenticate the user
    identified by `:login_params` and the given password. If the authentication
    succeeded, the driver must invoke `authenticated/1`.
  """
  @spec actions(t) :: {[action], t}
  def actions(state), do:
    {Enum.reverse(state.actions), %{state | actions: []}}

  @doc "Should be invoked by the driver after the connection is upgraded to ssl."
  @spec ssl_negotiated(t) :: t
  def ssl_negotiated(state), do:
    transition(state, :ssl_negotiated)

  @doc "Should be invoked by the driver to feed input bytes to the protocol state machine."
  @spec process(t, binary) :: t
  def process(state, input), do:
    process_buffer(%{state | buffer: state.buffer <> input})

  @doc "Should be invoked by the driver to choose the authentication method."
  @spec authentication_method(t, authentication_method) :: t
  def authentication_method(state, authentication_method), do:
    transition(state, {:authentication_method, authentication_method})

  @doc "Should be invoked by the driver if the user has been authenticated."
  @spec authenticated(t, boolean) :: t
  def authenticated(state, success), do:
    transition(state, {:authenticated, success})


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp process_buffer(%{expecting: expecting, buffer: buffer} = state)
      when expecting > 0 and byte_size(buffer) >= expecting do
    <<message::binary-size(expecting)>> <> rest_buffer = buffer

    %{state | expecting: 0, buffer: rest_buffer}
    |> transition({:message, message})
    |> process_buffer()
  end
  defp process_buffer(state), do: state

  defp request_send(state, action), do: add_action(state, {:send, action})

  defp add_action(state, action), do: %{state | actions: [action | state.actions]}

  defp next_state(state, next_state_name, expecting \\ 0), do:
    %{state | name: next_state_name, expecting: expecting}

  defp close(state, reason), do:
    state
    |> add_action({:close, reason})
    |> next_state(:closed)


  #-----------------------------------------------------------------------------------------------------------
  # State transitions
  #-----------------------------------------------------------------------------------------------------------

  defmacrop state(name), do:
    quote(do: %{name: unquote(name)} = var!(state))

  # :closed -> ignore all actions
  defp transition(state(:closed), _), do:
    state
  # :initial -> awaiting SSLRequest or StartupMessage
  defp transition(state(:initial), {:message, message}) do
    if ssl_message?(message) do
      state
      |> request_send(require_ssl())
      |> add_action(:upgrade_to_ssl)
      |> next_state(:ssl)
    else
      state
      |> request_send(fatal_error("28000", "Only SSL connections are allowed!"))
      |> close(:required_ssl)
    end
  end
  # :ssl -> waiting for the connection to be upgraded to SSL
  defp transition(state(:ssl), :ssl_negotiated), do:
    next_state(state, :startup_message, 8)
  # :startup_message -> expecting startup message from the client
  defp transition(state(:startup_message), {:message, message}) do
    startup_message = startup_message(message)
    if startup_message.version.major != 3 do
      close(state, :unsupported_protocol_version)
    else
      next_state(state, :login_params, startup_message.length)
    end
  end
  # :login_params -> expecting login params from the client
  defp transition(state(:login_params), {:message, raw_login_params}), do:
    state
    |> add_action({:login_params, login_params(raw_login_params)})
    |> next_state(:authentication_method)
  # :authentication_method -> expecting the driver to choose the authentication method
  defp transition(state(:authentication_method), {:authentication_method, authentication_method}), do:
    state
    |> request_send(authentication_method(authentication_method))
    |> next_state(:password_message, 5)
  # :password_message -> expecting password message from the client
  defp transition(state(:password_message), {:message, password_message}), do:
    next_state(state, :password, password_length(password_message))
  # :password -> expecting password from the client
  defp transition(state(:password), {:message, null_terminated_password}), do:
    state
    |> add_action({:authenticate, null_terminated_to_string(null_terminated_password)})
    |> next_state(:authenticating)
  # :authenticating -> expecting authentication result from the driver
  defp transition(state(:authenticating), {:authenticated, true}) do
    state
    |> request_send(authentication_ok())
    |> request_send(parameter_status("application_name", "aircloak"))
    |> request_send(parameter_status("server_version", "1.0.0"))
    |> request_send(ready_for_query())
    |> next_state(:connected)
  end
  defp transition(state(:authenticating), {:authenticated, false}), do:
    state
    # We're sending AuthenticationOK to indicate to the client that the auth procedure went fine. Then
    # we'll send a fatal error with a custom error message. It is unclear from the official docs that it
    # should be done this way. However, this approach produces a nicer error message, and it's the same
    # in PostgreSQL server (determined by wireshark).
    |> request_send(authentication_ok())
    |> request_send(fatal_error("28000", "Authentication failed!"))
    |> close(:not_authenticated)
end
