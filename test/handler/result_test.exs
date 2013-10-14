defmodule HandlerWeberReqHandlerResultTest do
  use ExUnit.Case
  import Handler.WeberReqHandler.Result

  def app do
    {:ok, cwd} = File.cwd
    views = String.to_char_list!(Path.expand(cwd <> "/../../test/weber_fake/lib/views/"))
    Handler.WeberReqHandler.Result.App.new controller: :"Controller.Test", views: views
  end

  test "request render" do
    request = request({:render, [], []}, app)
    assert request == {:render, "Hello\n", []}
  end

  test "request render inline" do
    request = request({:render_inline, "Hello inline!", [], []}, app)
    assert request == {:render, "Hello inline!", []}
  end
end