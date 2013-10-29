defmodule WeberControllerLayoutTest do
  use ExUnit.Case

  defmodule Controller.Example1 do
    use Weber.Controller

    layout 'test.html'
  end

  defmodule Controller.Example2 do
    use Weber.Controller
  end

  defmodule Controller.Example3 do
    use Weber.Controller

    layout false
  end

  test "take layout file correct" do
    assert Controller.Example1.__layout__  == 'test.html'
  end

  test "take layout file standard" do
    assert Controller.Example2.__layout__  == 'main.html'
  end

  test "don't use layout for controller" do
    assert Controller.Example3.__layout__  == false
  end
end