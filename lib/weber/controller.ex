defmodule Weber.Controller do

  defmacro __using__(_) do
    quote do
      import Weber.Controller.Layout
      layout('main.html')
    end
  end

end