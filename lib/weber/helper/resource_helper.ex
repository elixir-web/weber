defmodule Weber.Helper.ResourceHelper do

  @moduledoc """
    Include static resources with Weber.Helper.ResourceHelper.
  """

  import Weber.Helper.Html

  @doc """
    Generates <script>...<script> resource.

    Example:

      #
      # Generates: <script type="text/javascript" src="/public/test.js"></script>
      # If no value is passed for src it defaults to "/public/application.js"
      #
      script("/public/test.js")
      script()

  """
  def script(src // "/public/application.js") do
    tag(:script, "", [type: "text/javascript", src: src])
  end

  @doc """
    Generates <link ... > resource for style elements.

    Example:

      #
      # Generates: <link href="/public/test.css" rel="stylesheet" media="screen">
      # If no value is passed for href it defaults to "/public/application.css"
      style("/public/test.css")
      style()
  """
  def style(href // "/public/application.css", media // "screen") do
    tag(:link, [href: href, rel: "stylesheet", media: media])
  end

  @doc """
    Include favicon to your html template.

    Example:

      #
      # Generates: <link href="/public/img/favicon.ico" rel="shortcut icon" type="image/png">
      # If no value is passed for href it defaults to "/public/img/favicon.ico"
      #
      favicon("/public/img/favicon.ico")
      favicon()
  """
  def favicon(href // "/public/img/favicon.ico", rel // "shortcut icon", type // "image/x-icon") do
    tag(:link, [href: href, rel: rel, type: type])
  end

  @doc """
    Generates <img ... > resource.

    Example:

      #
      # Generates: <img src="/public/img/example.jpg" alt="Image" class="some-class" height="100" width="100">"
      image("/public/img/example.jpg", [alt: "Image", class: "some-class", height: 100, width: 100])
  """

  def image(src, html_options // []) do
    dict = html_options |> ListDict.new
    options = ListDict.put(dict, :src, src) |> ListDict.to_list
    tag(:img, options)
  end

end
