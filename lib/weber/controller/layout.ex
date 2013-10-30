defmodule Weber.Controller.Layout do

  @moduledoc """
  This module contains layout macro defined. It's useful to change which layout controller needs.
    ##Example
    layout 'test.html'
  """

  @doc false
  defmacro layout(path) do
    quote do
      def unquote(:__layout__)() do
        unquote(path)
      end
      defoverridable [__layout__: 0]
    end
  end
  
end