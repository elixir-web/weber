import Weber.Helper.Html
import Weber.Helper.Partial
import Weber.Helper.ResourceHelper

defmodule WeberPartialTest do

  use ExUnit.Case

  test "can render a partial using a tag" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:8080/partials', [], <<>>, [])
    body = :hackney.body(client)
    assert(body == {:ok, "<div>this is a partial</div>\n<div>and this is another partial</div>"})
    assert(status == 200)
  end
end