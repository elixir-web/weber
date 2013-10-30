defmodule #{projectNamespace}.Main do

  def action([]) do
    {:render, [project: "#{projectName}"], []}
  end
        
end
