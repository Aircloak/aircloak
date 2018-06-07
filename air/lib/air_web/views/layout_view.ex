defmodule AirWeb.LayoutView do
  @moduledoc false
  use Air.Web, :view

  defp container_class(assigns) do
    if assigns[:full_width] do
      "container-fluid"
    else
      "container"
    end
  end

  defp help_links(conn) do
    case AirWeb.Plug.HelpPages.help_pages(conn) do
      [] ->
        ""

      pages ->
        content_tag(:div, class: "container-fluid text-right help-links") do
          [
            content_tag(:span, "Consider reading", class: "label label-info"),
            " ",
            help_links(conn, pages)
          ]
        end
    end
  end

  defp help_links(conn, pages) do
    Stream.map(pages, fn page ->
      link(
        Air.HelpGuideView.page_title(page),
        to: help_guide_path(conn, :article, page),
        class: "suggested-reading",
        rel: "help"
      )
    end)
    |> Enum.reduce([], fn
      link, [] -> link
      link, acc -> [acc, ", ", link]
    end)
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
