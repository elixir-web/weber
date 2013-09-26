defmodule #{proj} do

  def action("GET", []) do
    {:render, [project: "#{projectName}"], []}
  end
        
end
