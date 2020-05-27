defmodule AirWeb.LayoutView do
  @moduledoc false
  use Air.Web, :view

  defp data_sources_widget(%{request_path: request_path} = conn) do
    cond do
      show_data_source_dropdown?(conn) ->
        render("_data_sources.html",
          conn: conn,
          data_source: conn.assigns.data_source,
          data_sources: conn.assigns.data_sources
        )

      permitted?(conn, AirWeb.DataSourceController, :index) ->
        path = data_source_path(conn, :index)
        class = if String.starts_with?(request_path, path), do: "active nav-link", else: "nav-link"
        content_tag(:div, link("Data sources", to: path, class: class), class: "nav navbar-nav ml-3")
    end
  end

  defp show_data_source_dropdown?(conn) do
    permitted?(conn, AirWeb.DataSourceController, :index) && Map.has_key?(conn.assigns, :data_source) &&
      Map.has_key?(conn.assigns, :data_sources)
  end

  defp data_source_badge(data_source) do
    case Air.Service.DataSource.status(data_source) do
      :broken ->
        content_tag(:i, "",
          class: "fas fa-exclamation-triangle text-warning",
          title: "Broken",
          "data-toggle": "tooltip",
          "data-placement": "right"
        )

      :analyzing ->
        content_tag(:span, "#{length(Air.Schemas.DataSource.tables(data_source))} tables*",
          class: "badge badge-success",
          title: "Some features unavailable pending analysis",
          "data-toggle": "tooltip",
          "data-placement": "right"
        )

      :online ->
        content_tag(:span, "#{length(Air.Schemas.DataSource.tables(data_source))} tables",
          class: "badge badge-success",
          title: "Online",
          "data-toggle": "tooltip",
          "data-placement": "right"
        )

      :offline ->
        content_tag(:i, "",
          class: "fas fa-exclamation-triangle text-danger",
          title: "Offline",
          "data-toggle": "tooltip",
          "data-placement": "right"
        )
    end
  end

  @background_images [
    %{
      photographer: "Kalen Emsley",
      credit_url:
        "https://unsplash.com/photos/_LuLiJc1cdo?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Kinney Lake, Canada"
    },
    %{
      photographer: "Victoria Chen",
      credit_url:
        "https://unsplash.com/photos/N6nnIx4C-Fo?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Omaha, New Zealand"
    },
    %{
      photographer: "Riccardo Chiarini",
      credit_url:
        "https://unsplash.com/photos/2VDa8bnLM8c?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Lake of Carezza, Carezza, Nova Levante, Italy"
    },
    %{
      photographer: "Ivana Cajina",
      credit_url:
        "https://unsplash.com/photos/Tp6L7SrCgxU?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Road"
    },
    %{
      photographer: "Aaron Burden",
      credit_url:
        "https://unsplash.com/photos/Glh5hc9KDaw?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Winter"
    },
    %{
      photographer: "Ishan @seefromthesky",
      credit_url:
        "https://unsplash.com/photos/BMJWpck6eQA?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText",
      motive: "Vaavu Atoll, Maldives"
    }
  ]

  defp background_image_num(), do: rem(NaiveDateTime.utc_now().day, 6)

  defp background_img_info(), do: Enum.at(@background_images, background_image_num())

  defp background_img_class(), do: "img#{background_image_num() + 1}"
end
