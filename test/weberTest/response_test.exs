defmodule WeberHttpResponseTest do
  use ExUnit.Case
   
  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/weber', [], <<>>, [])
    {:ok, body, _} = :hackney.body(client)
    assert(body == "Main\n")
    assert(status == 200)
  end

end
