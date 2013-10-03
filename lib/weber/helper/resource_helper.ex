defmodule Weber.Helper.ResourceHelper do

  @moduledoc """
    Include static resrouces with Weber.Helper.ResourceHelper.
  """

  import Weber.Helper.Html

  @doc """
    Generates <script>...<script> resource.

    Example:
            
      #
      # Generates: <script type="text/javascript" src="/static/test.js"></script>
      #
      script("/static/test.js")
          
  """
  def script(src) do
    tag(:script, "", [type: "text/javascript", src: src])
  end
    
  @doc """
    Genrates <link ... > resource.

    Example:

      #
      # Generates: <link href="/static/test.css" rel="stylesheet" media="screen">
      #
      link("/static/test.css")
  """
  def link(href, media // "screen") do
    tag(:link, [href: href, rel: "stylesheet", media: media])
  end

  @doc """
    Include favicon to your html template.

    Example:

      #
      # Generates: <link href="/static/img/favicon.ico" rel="shortcut icon" type="image/png">
      #
      favicon("/static/img/favicon.ico")
  """
  def favicon(href, rel // "shortcut icon", type // "image/png") do
    tag(:link, [href: href, rel: rel, type: type])
  end

end