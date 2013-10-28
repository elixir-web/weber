defmodule Weber.Helper.ResourceHelper do

  @moduledoc """
    Include static resources with Weber.Helper.ResourceHelper.
  """

  import Weber.Helper.Html

  @doc """
    Generates script resource.

      ## Example:

      #
      # Generates: <script type="text/javascript" src="/public/test.js"></script>
      # If no value is passed for src it defaults to "/public/js/application.js"
      #
      script("/public/test.js")
      script()

  """
  def script(src // "/public/js/application.js") do
    tag(:script, "", [type: "text/javascript", src: src])
  end

  @doc """
    Generates <link ... > resource for style elements.

      ## Example:

      #
      # Generates: <link href="/public/test.css" rel="stylesheet" media="screen">
      # If no value is passed for href it defaults to "/public/css/application.css"
      style("/public/test.css")
      style()
  """
  def style(href // "/public/css/application.css", media // "screen") do
    tag(:link, [href: href, rel: "stylesheet", media: media])
  end

  @doc """
    Include favicon to your html template.

      ## Example:

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

      ## Example:

      #
      # Generates: <img src="/public/img/example.jpg" alt="Image" class="some-class" height="100" width="100">"
      image("/public/img/example.jpg", [alt: "Image", class: "some-class", height: 100, width: 100])
  """
  def image(src, html_options // []) do
    dict = html_options |> ListDict.new
    options = ListDict.put(dict, :src, src) |> ListDict.to_list
    tag(:img, options)
  end

  def video(src) do
    case is_list(src) do
      true  -> video(src, [])
      false -> tag(:video, [src: src])
    end
    
  end

  @doc """
    Generates <video><source src="/videos/trailer.ogg" /><source src="/videos/trailer.flv" /></video>
    video(["/videos/trailer.ogg", "/videos/trailer.flv"]])
  """
  def video(src, html_options) when is_list(src) do
    tag(:video, html_options) do
      Enum.reduce(src, "", fn(video, tags) -> tag(:source, [src: video]) <> tags end)
    end
  end

  @doc """
    Generates <video ...> resource.

    ## Example:

    #
    # Generates: <video src="/public/videos/trailer">
    # video("/public/videos/trailer", [])
  """
  def video(src, html_options) do
    tag(:video, :lists.append([src: src], html_options))
  end

  def audio(src) do
    case is_list(src) do
      true  -> audio(src, [])
      false -> tag(:audio, [src: src])
    end
    
  end

  @doc """
    Generates <audio><source src="/audio/sound1.ogg" /><source src="/audio/sound2.mp3" /></audio>

    audio(["/audio/sound1.ogg", /audio/sound2.ogg])
  """
  def audio(src, html_options) when is_list(src) do
    tag(:audio, html_options) do
      Enum.reduce(src, "", fn(audio, tags) -> tag(:source, [src: audio]) <> tags end)
    end
  end

  @doc """
    Generates <audio ...> resource.

    ## Example:

    #
    # Generates: <audio src="/public/audio/sound">
    # audio("/public/audio/sound")
  """
  def audio(src, html_options) do
    tag(:audio, :lists.append([src: src], html_options))
  end

end
