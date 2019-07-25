defmodule IntegrationTest.AcceptanceHelper do
  import ExUnit.Assertions
  use Hound.Helpers

  def visit(path) do
    air_address = if System.get_env("CI") == "true", do: "localhost", else: to_string(System.get_env("AIR_IP"))
    navigate_to("http://#{air_address}:8081#{path}")
  end

  def in_another_session(fun) do
    current_session = current_session_name()

    try do
      change_session_to(make_ref())
      set_window_size(current_window_handle(), 1024, 1536)
      fun.()
    after
      change_session_to(current_session)
    end
  end

  defmacro assert_has(parent \\ nil, strategy, selector, opts \\ []) do
    quote bind_quoted: [parent: parent, strategy: strategy, selector: selector, opts: opts] do
      fun = fn fun, attempts ->
        unless exists_visible?(parent, strategy, selector) do
          if attempts <= 1 do
            take_screenshot("./browser_test/screenshots/error_#{:erlang.unique_integer([:monotonic, :positive])}.png")
            flunk("Element #{inspect({strategy, selector})} is not present")
          end

          Process.sleep(Keyword.get(opts, :retry_delay, 100))
          fun.(fun, attempts - 1)
        end
      end

      fun.(fun, Keyword.get(opts, :attempts, 10))
    end
  end

  defmacro refute_has(parent \\ nil, strategy, selector, opts \\ []) do
    quote bind_quoted: [parent: parent, strategy: strategy, selector: selector, opts: opts] do
      fun = fn fun, attempts ->
        if exists_visible?(parent, strategy, selector) do
          if attempts <= 1 do
            take_screenshot("./browser_test/screenshots/error_#{:erlang.unique_integer([:monotonic, :positive])}.png")
            flunk("Element #{inspect({strategy, selector})} is present")
          end

          Process.sleep(Keyword.get(opts, :retry_delay, 100))
          fun.(fun, attempts - 1)
        end
      end

      fun.(fun, Keyword.get(opts, :attempts, 1))
    end
  end

  def exists_visible?(parent \\ nil, strategy, selector) do
    fun = if is_nil(parent), do: &search_element/3, else: &search_within_element(parent, &1, &2, &3)

    case fun.(strategy, selector, 1) do
      {:ok, element} -> element_displayed?(element)
      {:error, _} -> false
    end
  rescue
    e in RuntimeError ->
      # Due to dynamic and asynchronous UI flow, it's sometimes possible that an element disappears while this function
      # is running. In such case, we'll get a "stale element reference" exception, which we're converting into a plain
      # `false` return value, to indicate that the desired element doesn't exist.
      if String.contains?(
           Exception.message(e),
           "stale element reference: element is not attached to the page document"
         ),
         do: false,
         else: reraise(e, __STACKTRACE__)
  end

  def new_group_name(), do: "group_#{:erlang.unique_integer([:positive, :monotonic])}"

  def visit_admin_page() do
    click({:css, "#navbar_dropdown"})
    click({:xpath, "//header/nav//a[text()='Admin']"})
  end

  def visit_admin_page(tab_caption) do
    visit_admin_page()
    click({:xpath, "//main//ul/li/a[text()='#{tab_caption}']"})
  end

  def visit_profile_page() do
    click({:css, "#navbar_dropdown"})
    click({:xpath, "//header/nav//a[text()='Settings']"})
  end

  def add_group(name) do
    visit_admin_page("Groups")
    click({:xpath, "//a[text()='Add a group']"})
    fill_field({:css, "#group_name"}, name)
    click({:css, "button[type='submit']"})
  end

  def login_as_admin(opts \\ []), do: login("admin@aircloak.com", "password1234", opts)

  def login(login, password, opts \\ []) do
    visit("/")
    fill_field({:css, "[name='login']"}, login)
    fill_field({:css, "[name='password']"}, password)
    if opts[:remember_me?], do: click({:css, "[name='remember']"})
    click({:css, "form button"})
  end

  def create_user() do
    name = random_string()
    login = random_string()
    password = random_string()

    user = Air.Service.User.create!(%{name: name, login: login})
    password_token = Air.Service.User.reset_password_token(user, ldap: :any)
    {:ok, _} = Air.Service.User.reset_password(password_token, %{password: password, password_confirmation: password})

    %{name: name, login: login, password: password}
  end

  def visit_data_source(name) do
    click({:xpath, "//nav//a[text()='Data sources']"})
    click({:xpath, "//a[text()='#{name}']"})
  end

  def random_string(), do: :crypto.strong_rand_bytes(10) |> Base.encode64(padding: false)

  def cookie_value(name) do
    cookies()
    |> Enum.find(&(&1["name"] == name))
    |> Access.get("value")
  end

  def start_query(text) do
    set_query_text(text)
    click({:xpath, "//button[text()='Run']"})
  end

  def set_query_text(text) do
    clear_code_mirror({:css, "#sql-editor"})
    send_text(text)
  end

  def clear_code_mirror(top_element) do
    click(top_element)
    with_keys(control_key(), do: send_text("a"))
    send_keys([:backspace])
  end

  defp control_key() do
    if :os.type() == {:unix, :darwin} and System.get_env("AIR_IP") in ~w/localhost 127.0.0.1/,
      do: :command,
      else: :control
  end

  def hover(element), do: move_to(element, 1, 1)
end
