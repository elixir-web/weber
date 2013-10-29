defmodule WeberHelperContentForTest do
  use ExUnit.Case

  setup_all do
    root = __DIR__ <> "/../weber_fake"
    app_name = Mix.Project.get.project[:app]
    Weber.run_weber(app_name, Example.Route.get_route, :binary.bin_to_list(root), Config.config)
    :ok
  end

  require Weber.Helper.ContentFor

  test "content for layout" do
    file_content = File.read!(app_views("test.html"))
    Weber.Helper.ContentFor.content_for(:layout, "main.html")
    assert file_content =~ %r/Hello/
  end

  test "content for layout bindings" do
    file_content = File.read!(app_views("test_bind.html"))
    Weber.Helper.ContentFor.content_for(:layout, "main.html")
    assert file_content =~ %r/<%= value %>/
  end

  test "when not use a layout" do
    file_content = File.read!(app_views("test.html"))
    Weber.Helper.ContentFor.content_for(:layout, false)
    assert file_content == "Hello\n"
  end

  defp app_views(file) do
    __DIR__ <> "/../weber_fake/lib/views/" <> file
  end
end