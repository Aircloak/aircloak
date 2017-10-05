defmodule Air.Phoenix.MarkdownEngine do
  @moduledoc """
  Phoenix template compiler for markdown files.

  This module ensures that `.md` files in the `templates` folder are compiled to
  html and included in corresponding views. To use it, you need to configure
  Phoenix:

  ```elixir
  config :phoenix, :template_engines, md: Air.Phoenix.MarkdownEngine
  ```
  """
  @behaviour Phoenix.Template.Engine

  # -------------------------------------------------------------------
  # Phoenix.Template.Engine callbacks
  # -------------------------------------------------------------------


  @impl Phoenix.Template.Engine
  def compile(template_path, _template_name) do
    Macro.escape({:safe,
      template_path
      |> File.read!()
      |> Earmark.as_html!()
    })
  end
end
