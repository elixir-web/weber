defmodule #{projectNamespace}.Main do
  
  use Weber.Controller

  layout false
  def action([]) do
    {:redirect, "/index.html"}
  end
        
end
