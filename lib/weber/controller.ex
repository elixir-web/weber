defmodule Weber.Controller do

  defmacro __using__(_) do
  end

  defmacro layout(path) do
    quote do
      def unquote(:__layout__)() do
        unquote(path)
      end
    end
  end
	
end