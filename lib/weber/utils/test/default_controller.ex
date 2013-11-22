defmodule WeberTest.Main do

  use Weber.Controller

  layout false

  def redirect_action(_, conn) do
    {:redirect, "/index.html"}
  end

end