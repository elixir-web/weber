defmodule Weber.Controller do

  defmacro __using__(_) do
    quote do
      import Weber.Controller.Layout
      layout('Main.html')
    end
  end

end