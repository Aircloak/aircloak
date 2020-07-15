defmodule AirWeb.LayoutView do
  @moduledoc false
  use Air.Web, :view

  defp data_sources_widget(%{request_path: request_path} = conn) do
    if permitted?(conn, AirWeb.DataSourceController, :index) do
      if show_data_source_dropdown?(conn) do
        render("_data_sources.html",
          conn: conn,
          data_source: conn.assigns.data_source,
          data_sources: conn.assigns.data_sources
        )
      else
        path = data_source_path(conn, :index)
        class = if String.starts_with?(request_path, path), do: "active nav-link", else: "nav-link"
        content_tag(:div, link("Data sources", to: path, class: class), class: "nav navbar-nav ml-3")
      end
    end
  end

  defp show_data_source_dropdown?(conn) do
    Map.has_key?(conn.assigns, :data_source) && Map.has_key?(conn.assigns, :data_sources) &&
      length(conn.assigns.data_sources) > 1
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
      photographer: "LExie Blessing",
      credit_url: "https://www.pexels.com/photo/photo-of-pink-dogwood-plant-4273058/",
      motive: "Pink Dogwood, Washington State"
    },
    %{
      photographer: "Manfred Richter",
      credit_url: "https://pixabay.com/photos/dogwood-ornamental-shrub-bush-4142742/",
      motive: "Dogwood"
    },
    %{
      photographer: "Melissa Wilt",
      credit_url: "https://pixabay.com/photos/flower-dogwood-plant-flora-bud-2419960/",
      motive: "Dogwood"
    },
    %{
      photographer: "Susanne Jutzeler",
      credit_url: "https://pixabay.com/photos/dogwood-spring-bloom-nature-flora-4113098/",
      motive: "Dogwood"
    },
    %{
      photographer: "Kendall",
      credit_url: "https://unsplash.com/photos/RLlwhlh0KpM",
      motive: "Dogwood, North Carolina, Charlotte, United States"
    },
    %{
      photographer: "Shell Ghostcage",
      credit_url: "https://pixabay.com/photos/landscape-natural-flowers-arboretum-1345588/",
      motive: "Dogwood"
    }
  ]

  defp background_image_num(), do: rem(NaiveDateTime.utc_now().day, 6)

  defp background_img_info(), do: Enum.at(@background_images, background_image_num())

  defp background_img_class(), do: "img#{background_image_num() + 1}"
end
