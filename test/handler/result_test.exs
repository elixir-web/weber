defmodule HandlerWeberReqHandlerResultTest do
  use ExUnit.Case
  import Handler.WeberReqHandler.Result

  def controller do
    :"Controller.Test"
  end

  def views do
    String.to_char_list!(Path.expand(__DIR__ <> "/../weber_fake/lib/views/"))
  end

  test "request render" do
    request = handle_result({:render, [], []}, controller, views)
    assert request == {:render, "Hello\n", []}
  end

  test "request render inline" do
    request = handle_result({:render_inline, "Hello inline!", [], []}, controller, views)
    assert request == {:render, "Hello inline!", []}
  end

  test "request file" do
    request = handle_result({:file, String.from_char_list!(views ++ "/test.html"), []}, controller, views)
    assert request == {:file, "Hello\n", [{"Content-Type", ["text/html"]}]}
  end

  test "request redirect" do
    request = handle_result({:redirect, "/"}, controller, views)
    assert request == {:redirect, "/"}
  end

  test "request nothing" do
    request = handle_result({:nothing, []}, controller, views)
    assert request == {:nothing, []}
  end

  test "request text" do
    request = handle_result({:text, "Hello", []}, controller, views)
    assert request == {:text, "Hello", []}
  end

  test "request json" do
    data = [test: "test"]
    request = handle_result({:json, data, []}, controller, views)
    assert request == {:json, JSON.generate(data), []}
  end
end