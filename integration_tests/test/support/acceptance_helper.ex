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
      set_window_size(current_window_handle(), 1024, 768)
      fun.()
    after
      change_session_to(current_session)
    end
  end

  defmacro assert_has(parent \\ nil, strategy, selector) do
    quote bind_quoted: [parent: parent, strategy: strategy, selector: selector] do
      unless eventually_exists?(parent, strategy, selector) do
        take_screenshot("./browser_test/screenshots/error_#{:erlang.unique_integer([:monotonic, :positive])}.png")
        flunk("Element #{inspect({strategy, selector})} is not present")
      end
    end
  end

  defmacro refute_has(parent \\ nil, strategy, selector) do
    quote bind_quoted: [parent: parent, strategy: strategy, selector: selector] do
      if eventually_exists?(parent, strategy, selector, retries: 1) do
        take_screenshot("./browser_test/screenshots/error_#{:erlang.unique_integer([:monotonic, :positive])}.png")
        flunk("Element #{inspect({strategy, selector})} is present")
      end
    end
  end

  def eventually_exists?(parent \\ nil, strategy, selector, opts \\ []) do
    fun = if is_nil(parent), do: &search_element/3, else: &search_within_element(parent, &1, &2, &3)

    case fun.(strategy, selector, Keyword.get(opts, :retries, 10)) do
      {:ok, element} -> element_displayed?(element)
      {:error, _} -> false
    end
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
    login = random_string()
    name = random_string()
    password = random_string()

    password_reset_url =
      create_user(login, name, fn ->
        click({:xpath, "//a[text()='Reset password']"})
        visible_text({:xpath, "//*[@id='reset-link']"})
      end)

    in_another_session(fn ->
      navigate_to(password_reset_url)
      fill_field({:xpath, "//input[@id='user_password']"}, password)
      fill_field({:xpath, "//input[@id='user_password_confirmation']"}, password)
      click({:xpath, "//button[text()='Save']"})
    end)

    %{name: name, login: login, password: password}
  end

  def create_user(login, name, fun \\ fn -> :ok end) do
    in_another_session(fn ->
      login_as_admin()
      visit_admin_page("Users")
      click({:xpath, "//a[text()='Add a user']"})
      fill_field({:xpath, "//input[@id='user_login']"}, login)
      fill_field({:xpath, "//input[@id='user_name']"}, name)
      click({:xpath, "//button[text()='Save']"})
      fun.()
    end)
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
end
