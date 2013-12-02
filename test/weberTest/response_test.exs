defmodule WeberHttpResponseTest do
  use ExUnit.Case
   
  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/weber', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "Main\n"})
    assert(status == 200)
  end

  test "SimpleResponse Include test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/include', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "Main\n"})
    assert(status == 200)
  end

end
