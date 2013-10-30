Code.require_file "../support/config.exs", __DIR__
Code.require_file "../weber_fake/route.exs", __DIR__

defmodule HandlerWeberReqHandlerTest do
  use ExUnit.Case

  setup_all do
    root = __DIR__ <> "/../weber_fake"
    app_name =  Mix.Project.get.project[:app]
    Weber.run_weber(app_name, Example.Route.get_route, :binary.bin_to_list(root), Config.config)
    :ok
  end

  def response(request) do
    {:ok, status, _, client} = :hackney.request(:get, request, [], <<>>, [])
    {:ok, body, _} = :hackney.body(client)
    {status, body}
  end

  test "response body" do
    {status, body} = response('http://localhost:8080')
    
    assert status == 200
    assert body == "Hello\n"
  end

  test "response body with static file" do
    {status, body} = response('http://localhost:8080/static.html')

    assert status == 200
    assert body =~ %r/Hello static/
  end

  test "response body 404" do
    {status, body} = response('http://localhost:8080/url_not_exist')

    assert status == 404
    assert body =~ %r/Page not found/
  end
end