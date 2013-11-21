defmodule #{projectNamespace}.Main do
  
  use Weber.Controller

  layout false
  def action([], conn) do
    {:render, [], []}
  end
        
end
