defmodule WeberHttpResponseTest do
  use ExUnit.Case

  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080', [], <<>>, [])
    {:ok, body, _} = :hackney.body(client)
    assert(status == 301)
  end

end