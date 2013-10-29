defmodule Example.Test do
  use Weber.Controller

  layout false
  def render_site(_) do
    {:render, [], []}
  end
  
end