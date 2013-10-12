defmodule WeberHelperResourceTest do
  use ExUnit.Case

  import Weber.Helper.ResourceHelper

  test "Create script tag" do
    script = script("/public/test.js")
    assert(script == "<script type=\"text/javascript\" src=\"/public/test.js\"></script>")
  end

  test "Create script tag with default value" do
    script = script()
    assert(script == "<script type=\"text/javascript\" src=\"/public/application.js\"></script>")
  end

  test "Create style tag" do
    style = style("/public/test.css")
    assert(style == "<link href=\"/public/test.css\" rel=\"stylesheet\" media=\"screen\">")
  end

  test "Create style tag with default value" do
    style = style()
    assert(style == "<link href=\"/public/application.css\" rel=\"stylesheet\" media=\"screen\">")
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

end
