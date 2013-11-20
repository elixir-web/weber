defmodule #{projectNamespace}.Main do
  
  use Weber.Controller

  layout false
  def action([]) do
    {:render, [], []}
  end
        
end
