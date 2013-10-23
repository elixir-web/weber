defmodule HandlerWeberReqHandlerResultTest do
  use ExUnit.Case
  import Handler.WeberReqHandler.Result

  setup_all do
    root = __DIR__ <> "/../weber_fake"
    app_name = Mix.Project.get.project[:app]
    Weber.run_weber(app_name, Example.Route.get_route, :binary.bin_to_list(root), Config.config)
    :ok
  end

  def controller do
    :"Example.Test"
  end

  def views do
    String.to_char_list!(Path.expand(__DIR__ <> "/../weber_fake/lib/views/"))
  end

  test "request render" do
    request = handle_result({:render, [], []}, controller, views)
    assert request == {:render, 200, "<html>\n  <head>\n    <title>Test</title>\n  </head>\n  <body>\n    Hello\n\n  </body>\n</html>", [{"Content-Type", "text/html"}]}
  end

  test "request render inline" do
    request = handle_result({:render_inline, "Hello inline!", [], []}, controller, views)
    assert request == {:render, 200, "Hello inline!", []}
  end

  test "request file" do
    request = handle_result({:file, String.from_char_list!(views ++ "/test.html"), []}, controller, views)
    assert request == {:file, 200, "Hello\n", [{"Content-Type", ["text/html"]}]}
  end

  test "request redirect" do
    request = handle_result({:redirect, "/"}, controller, views)
    assert request == {:redirect, 301, [{"Location", "/"}, {"Cache-Control", "no-store"}]}
  end

  test "request nothing" do
    request = handle_result({:nothing, []}, controller, views)
    assert request == {:nothing, 200, []}
  end

  test "request text" do
    request = handle_result({:text, "Hello", []}, controller, views)
    assert request == {:text, 200, "Hello", [{"Content-Type", "plain/text"}]}
  end

  test "request json" do
    data = [test: "test"]
    request = handle_result({:json, data, []}, controller, views)
    assert request == {:json, 200, JSON.generate(data), [{"Content-Type", "application/json"}]}
  end

  test "request not found" do
    request = handle_result({:not_found, "Not Found", []})
    assert request == {:not_found, 404, "Not Found", []}
  end
end