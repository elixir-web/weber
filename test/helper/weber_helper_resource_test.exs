defmodule WeberHelperResourceTest do
  use ExUnit.Case

  import Weber.Helper.ResourceHelper

  test "Create script tag" do
    script = script("/public/test.js")
    assert(script == "<script type=\"text/javascript\" src=\"/public/test.js\"></script>")
  end

  test "Create script tag with default value" do
    script = script()
    assert(script == "<script type=\"text/javascript\" src=\"/public/js/application.js\"></script>")
  end

  test "Create style tag" do
    style = style("/public/test.css")
    assert(style == "<link href=\"/public/test.css\" rel=\"stylesheet\" media=\"screen\">")
  end

  test "Create style tag with default value" do
    style = style()
    assert(style == "<link href=\"/public/css/application.css\" rel=\"stylesheet\" media=\"screen\">")
  end

  test "Create favicon tag" do
    fav = favicon("/public/img/favicon.ico")
    assert(fav == "<link href=\"/public/img/favicon.ico\" rel=\"shortcut icon\" type=\"image/x-icon\">")
  end

  test "Create favicon tag with default values" do
    fav = favicon()
    assert(fav == "<link href=\"/public/img/favicon.ico\" rel=\"shortcut icon\" type=\"image/x-icon\">")
  end

  test "Create image tag" do
    image = image("/public/img/example.jpg", [alt: "Image", class: "some-class", height: 100, width: 100])
    assert(image == "<img src=\"/public/img/example.jpg\" alt=\"Image\" class=\"some-class\" height=\"100\" width=\"100\">")
  end

  test "Create image tag with no html_options" do
    image = image("/public/img/example.jpg")
    assert(image == "<img src=\"/public/img/example.jpg\">")
  end

  test "Create simple video tag" do
    video = video("/public/videos/video1")
    assert(video == "<video src=\"/public/videos/video1\"></video>")

    video2 = video("/public/videos/video1.ogg")
    assert(video2 == "<video src=\"/public/videos/video1.ogg\"></video>")

    video3 = video("/public/videos/video1.ogg", [controls: "controls"])
    assert(video3 == "<video src=\"/public/videos/video1.ogg\" controls=\"controls\"></video>")
  end

  test "Create video/source tag" do
    video = video(["/public/videos/video1", "/public/videos/video1"])
    assert(video == "<video><source src=\"/public/videos/video1\"><source src=\"/public/videos/video1\"></video>")
  end

end
