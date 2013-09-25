            defmodule #{proj} do

                def action("GET", []) do
                    {:render, [project: "#{app}"]}
                end
                    
            end
