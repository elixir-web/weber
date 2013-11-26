defmodule TestTestTest.Main do
  
  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], [{"content-type", "text/html"}]}
  end

end

defmodule TestTestTest.Include do
    
  use Weber.Controller

  layout false
  def include_action([], _conn) do
    {:render, [], []}
  end

end