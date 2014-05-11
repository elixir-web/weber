
defexception WeberControllerException, [:message] do
  def exception([value: value]) do
    WeberControllerException[message: value]
  end
end

defmodule Weber.Controller do

  defmacro __using__(_) do
    quote do
      import Weber.Controller.Layout
      import unquote(__MODULE__)
      layout('Main.html')
      @before_compile unquote(__MODULE__)
      @render_on_raise []
    end
  end

  defmacro __before_compile__(env) do
    render_on_raise = Module.get_attribute(env.module, :render_on_raise)

    quote do
      def raise_keys do
        unquote(render_on_raise) |> Enum.map(fn({k, v}) -> k end)
      end

      def render_value_for_key(key) do
        {_, value} = unquote(render_on_raise) |> Enum.find(fn({k, v}) -> k == key end)
        value
      end
    end
  end

  defmacro render_when_raise(value, result) do
    result = Macro.escape(result)
    quote do
      @render_on_raise [{unquote(value), unquote(result)} | @render_on_raise]
    end
  end

end
