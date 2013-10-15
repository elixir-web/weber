Code.require_file "../support/config.exs", __DIR__
Code.require_file "../weber_fake/route.exs", __DIR__

defmodule HandlerWeberReqHandlerTest do
  use ExUnit.Case
  setup_all do
    root = __DIR__ <> "/../weber_fake"
    app_name = :Example
    Weber.run_weber(app_name, Example.Route.get_route, :binary.bin_to_list(root), Config.config)
    :inets.start
  end

  def response(request) do
    {:ok, {{_, status, _}, _, body}} = :httpc.request(request)
    {status, body}
  end

  test "response body" do
    {status, body} = response('http://localhost:8080')
    
    assert status == 200
    assert body == 'Hello\n'
  end

  test "response body with static file" do
    {status, body} = response('http://localhost:8080/static.html')

    assert status == 200
    assert String.from_char_list!(body) =~ %r/Hello static/
  end

  test "response body 404" do
    {status, body} = response('http://localhost:8080/url_not_exist')

    assert status == 404
    assert String.from_char_list!(body) =~ %r/Page not found/
  end
end