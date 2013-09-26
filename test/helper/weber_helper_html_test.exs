defmodule WeberHelperHtmlTest do
  use ExUnit.Case

  import Weber.Helper.Html

  test "Create a tag without attributes within text" do
    html = tag(:p, "test")  
    assert(html == "<p>test</p>")
  end

  test "Create tag with attributes" do
    html = tag(:p, "test", [class: "test_class", id: "test_id"])
    assert(html == "<p class=\"test_class\" id=\"test_id\">test</p>")
  end

  test "Create tag with attributes boolean" do
    html = tag(:p, "test", [class: "test_class", id: "test_id", required: true])
    assert(html == "<p class=\"test_class\" id=\"test_id\" required>test</p>")
  end

  test "Create tag with another tags inside" do
    html = tag(:div) do
      tag(:p, "test", [class: "test"])
    end
    assert(html == "<div><p class=\"test\">test</p></div>")
  end

  test "Create tag with attributes and another tags inside" do
    html = tag(:div, [class: "div_test", id: "id_test"]) do
      tag(:span, [class: "span_class"]) do
        tag(:p, "test", [class: "test"])
      end
    end

    assert(html == "<div class=\"div_test\" id=\"id_test\"><span class=\"span_class\"><p class=\"test\">test</p></span></div>")
  end

  test "Create tags without end" do
    html = tag(:img, [src: "path/to/file.png"])
    assert(html == "<img src=\"path/to/file.png\">")
  end
end
