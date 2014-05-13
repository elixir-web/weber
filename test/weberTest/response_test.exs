defmodule WeberHttpResponseTest do
  use ExUnit.Case
   
  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/weber', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "Main\n"})
    assert(status == 200)
  end

  test "json response with custom status" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/json/action', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "{}"})
    assert(status == 201)
  end
  
  test "`redirect` in route test" do
    {:ok, status, _, _client} = :hackney.request(:get, 'http://localhost:8080/redirect', [], <<>>, [])
    assert(status == 302)
  end

  test "`content_for` test" do
    {:ok, _status, _, client} = :hackney.request(:get, 'http://localhost:8080/content_for', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "<!DOCTYPE html>\n<html>\n  <head>\n    <title>\n      My Project\n    </title>\n    <meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\" />\n  </head>\n  <body>\n    <div id=\"container\">\n    Hello Weber!      \n    </div>\n  </body>\n</html> "})
  end

  test "raise unauthorized exception" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/unauthorized', [], <<>>, [])
    body = :hackney.body(client)
    assert(status == 401)
    assert(body == {:ok, "Unauthorized"})
  end

  test "raise 500 exception" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/unknown', [], <<>>, [])
    body = :hackney.body(client)
    assert(status == 500)
    assert(body == {:ok, "An unknown error occurred"})
  end
end
