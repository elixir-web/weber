defmodule WeberHttpUrlTest do
  use ExUnit.Case

  import Weber.Http.Url

  test "Test for Weber.Http.Url" do
    assert(getBinding("/user/0xAX") == [segment: "user", segment: "0xAX"])
    assert(getBinding("/user") == [segment: "user"])
    assert(getBinding("/") == [segment: ""])
    assert(getBinding("/user/0xAX/password/:password") == [segment: "user", segment: "0xAX", segment: "password", binding: "password"])
    assert(getBinding("/user/0xAX/password/:password/") == [segment: "user", segment: "0xAX", segment: "password", binding: "password"])
    
  end
end