Code.require_file "../support/config.exs", __DIR__
Code.require_file "../weber_fake/route.exs", __DIR__

defmodule WeberHelperTest do
  use ExUnit.Case

  import Weber.Helper.IncludeView

  setup_all do
    Weber.stop([])
    root = __DIR__ <> "/../weber_fake"
    app_name = Mix.Project.get.project[:app]
    Weber.run_weber(app_name, Example.Route.get_route, :binary.bin_to_list(root), Config.config)
    :ok
  end

  teardown_all do
    Weber.stop([])
    :ok
  end

  test "give a file to include inside another view without binds" do
    html = include_view("test.html")
    assert(html =~ %r/Hello/)
  end

  test "give a file to include inside another view with binds" do
    html = include_view("test_bind.html", [value: "Hello"])
    assert(html =~ %r/Hello/)
  end

  test "write inside view" do
    app_name = Mix.Project.get.project[:app]
    views_path = :gen_server.call(app_name, :views)
    html = views_path ++ 'test_include_view.html' |> String.from_char_list! |> EEx.eval_file

    assert(html =~ %r/^Hello with value/)
  end
end