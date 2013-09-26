Code.require_file "../support/config.exs", __DIR__

defmodule WeberHelperTest do
  use ExUnit.Case

  import Weber.Helper

  setup_all do
    {:ok, root} = File.cwd
    root = root <> "/test/weber_fake"
    Weber.run_weber(Mix.Project.get, [], :binary.bin_to_list(root), Config.config)
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
end
