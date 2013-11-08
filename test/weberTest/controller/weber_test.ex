defmodule WeberTest.Main do

  use Weber.Controller

  layout false

  def action(_) do
    {:redirect, "/index.html", []}
  end

end