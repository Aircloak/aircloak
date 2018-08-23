defmodule Aircloak.HTMLScrubber do
  @moduledoc """
  Based on the MarkdownHTML scrubber from HtmlSanitizeEx:
  https://github.com/rrrene/html_sanitize_ex/blob/4da586be93a09f8c4d7950ba93ea3c674fd1bd14/lib/html_sanitize_ex/scrubber/markdown_html.ex

  We provide a more restrictive parser that only allow a subset of
  html tags that we deem safe.
  """

  @doc "Scrubs html removing all but headers, emphasis and tables"
  @spec scrub(String.t()) :: String.t()
  def scrub(html), do: html |> HtmlSanitizeEx.Scrubber.scrub(Aircloak.HTMLScrubber.MarkdownHTML)

  defmodule MarkdownHTML do
    @moduledoc false

    require HtmlSanitizeEx.Scrubber.Meta
    alias HtmlSanitizeEx.Scrubber.Meta

    # Removes any CDATA tags before the traverser/scrubber runs.
    Meta.remove_cdata_sections_before_scrub()

    Meta.strip_comments()

    Meta.allow_tag_with_these_attributes("b", [])
    Meta.allow_tag_with_these_attributes("br", [])
    Meta.allow_tag_with_these_attributes("em", [])
    Meta.allow_tag_with_these_attributes("h1", [])
    Meta.allow_tag_with_these_attributes("h2", [])
    Meta.allow_tag_with_these_attributes("h3", [])
    Meta.allow_tag_with_these_attributes("h4", [])
    Meta.allow_tag_with_these_attributes("h5", [])
    Meta.allow_tag_with_these_attributes("hr", [])
    Meta.allow_tag_with_these_attributes("i", [])

    Meta.allow_tag_with_these_attributes("li", [])
    Meta.allow_tag_with_these_attributes("ol", [])
    Meta.allow_tag_with_these_attributes("p", [])
    Meta.allow_tag_with_these_attributes("span", [])
    Meta.allow_tag_with_these_attributes("strong", [])
    Meta.allow_tag_with_these_attributes("table", [])
    Meta.allow_tag_with_these_attributes("tbody", [])
    Meta.allow_tag_with_these_attributes("td", [])
    Meta.allow_tag_with_these_attributes("th", [])
    Meta.allow_tag_with_these_attributes("thead", [])
    Meta.allow_tag_with_these_attributes("tr", [])
    Meta.allow_tag_with_these_attributes("u", [])
    Meta.allow_tag_with_these_attributes("ul", [])

    Meta.strip_everything_not_covered()
  end
end
