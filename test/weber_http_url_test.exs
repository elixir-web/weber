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

  test "Test for Weber.Http.Url, request with parameters" do
  	assert(getBinding("/user?name=0xAX") == [{:segment,<<"user">>},{:param, :done,<<"name">>,<<"0xAX">>}])

  	assert(getBinding("/user?name=0xAX&name=zeroxAX") == [{:segment,<<"user">>}, {:param,:done,<<"name">>,<<"0xAX">>},
                                                          {:param,:done,<<"name">>,<<"zeroxAX">>}])

  end
end