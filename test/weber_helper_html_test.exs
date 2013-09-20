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

  test "Create tag with attributes" do
    html = tag(:p, "test", [class: "test_class", id: "test_id", required: true])
    assert(html == "<p class=\"test_class\" id=\"test_id\" required>test</p>")
  end
end
