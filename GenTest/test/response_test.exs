defmodule WeberHttpResponseTest do
  use ExUnit.Case

  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/weber', [], <<>>, [])
    assert(status == 404)
  end

end