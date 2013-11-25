import Weber.Helper.Html
import Weber.Helper.ResourceHelper

defmodule WeberHtmlHelperTest do
  
  use ExUnit.Case
  
  test "Tests for Weber.Helper.Html" do
    assert tag(:p, "test") == "<p>test</p>"
    assert tag(:p, "test", [class: "class_test"]) == "<p class=\"class_test\">test</p>"
    assert tag(:img, [src: "public/img/img.png"]) == "<img src=\"public/img/img.png\">"
  end

  test "Test for Weber.Helper.Html | tag with bloks" do
    t = tag(:div, [id: "test"]) do
      tag(:p, "test")
    end

    assert t == "<div id=\"test\"><p>test</p></div>"
  end

end

defmodule WeberHtmlResourceTest do
  
  use ExUnit.Case
  
  test "WeberHtmlResourceTest `style` test" do
    assert script == "<script type=\"text/javascript\" src=\"/public/js/application.js\"></script>"
    assert script("/static/test.js") == "<script type=\"text/javascript\" src=\"/static/test.js\"></script>"
  end

  test "WeberHtmlResourceTest `script` test" do
    assert style == "<link href=\"/public/css/application.css\" rel=\"stylesheet\" media=\"screen\">"
    assert style("/static/test.css") == "<link href=\"/static/test.css\" rel=\"stylesheet\" media=\"screen\">"
  end

  test "WeberHtmlResourceTest `favicon` test" do
    assert favicon == "<link href=\"/public/img/favicon.ico\" rel=\"shortcut icon\" type=\"image/x-icon\">"
    assert favicon("/static/test.ico") == "<link href=\"/static/test.ico\" rel=\"shortcut icon\" type=\"image/x-icon\">"
  end

  test "WeberHtmlResourceTest `audio` test" do
    assert audio("/public/audio/sound") == "<audio src=\"/public/audio/sound\"></audio>"
    assert audio(["/public/audio/sound1", "/public/audio/sound2"], [autoplay: true]) == "<audio autoplay><source src=\"/public/audio/sound2\"><source src=\"/public/audio/sound1\"></audio>"
  end

  test "WeberHtmlResourceTest `video` test" do
    assert video("public/videos/trailer") == "<video src=\"public/videos/trailer\"></video>"
    assert video(["/public/videos/video1", "/public/videos/video2"], [height: 48, width: 48]) == "<video height=\"48\" width=\"48\"><source src=\"/public/videos/video2\"><source src=\"/public/videos/video1\"></video>"
  end

end