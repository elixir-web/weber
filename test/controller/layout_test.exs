defmodule WeberControllerLayoutTest do
  use ExUnit.Case

  defmodule Controller.Example do
    use Weber.Controller

    layout 'test.html'
  end

  defmodule Controller.Example2 do
    use Weber.Controller
  end

  test "take layout file correct" do
    assert Controller.Example.__layout__  == 'test.html'
  end

  test "take layout file standard" do
    assert Controller.Example2.__layout__  == 'main.html'
  end
end