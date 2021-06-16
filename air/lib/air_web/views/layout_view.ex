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
      photographer: "Marc St",
      credit_url: "https://unsplash.com/photos/0NGQmnuDnJ0",
      motive: "Magic places tucked away in the Italian mountains - Karersee, Italy"
    },
    %{
      photographer: "CHUTTERSNAP",
      credit_url: "https://unsplash.com/photos/90fArTCswjQ",
      motive: "Evergreen"
    },
    %{
      photographer: "Noah Silliman",
      credit_url: "https://unsplash.com/photos/01Qqkfz-ck8",
      motive: "Evergreen treetops from above"
    },
    %{
      photographer: "Geran de Klerk",
      credit_url: "https://pixabay.com/photos/dogwood-spring-bloom-nature-flora-4113098/",
      motive: "Remote forest path"
    },
    %{
      photographer: "Sebastian Unrau",
      credit_url: "https://unsplash.com/photos/sp-p7uuT0tw",
      motive: "Bad Pyrmont, Deutschland"
    },
    %{
      photographer: "Luca Bravo",
      credit_url: "https://unsplash.com/photos/ESkw2ayO2As",
      motive: "Lago di Braies"
    },
    %{
      photographer: "Samuel Ferrara",
      credit_url: "https://unsplash.com/photos/iecJiKe_RNg",
      motive: "Autumn woods across the lake"
    },
    %{
      photographer: "Scott Taylor",
      credit_url: "https://unsplash.com/photos/02a4DSekRVg",
      motive: "Blue ocean by forest road - Thailand"
    },
  ]

  defp background_image_num(), do: rem(NaiveDateTime.utc_now().day, 8)

  defp background_img_info(), do: Enum.at(@background_images, background_image_num())

  defp background_img_class(), do: "img#{background_image_num() + 1}"
end
