defmodule WeberHelperResourceTest do
  use ExUnit.Case

  import Weber.Helper.ResourceHelper

  test "Create script tag" do
    script = js("/static/test.js")
    assert(script == "<script type=\"text/javascript\" src=\"/static/test.js\"></script>")
  end

  test "Create link tag" do
    link = stylesheet("/static/test.css")
    assert(link == "<link href=\"/static/test.css\" rel=\"stylesheet\" media=\"screen\">")
  end

  test "Create favicon tag" do
    fav = favicon("/static/img/favicon.ico")
    assert(fav == "<link href=\"/static/img/favicon.ico\" rel=\"shortcut icon\" type=\"image/png\">")
  end

end