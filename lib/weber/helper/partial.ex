defmodule Weber.Helper.Partial do
  
  @moduledoc """
    Partial rendering helper.

    ## Example

    # Generates <div>this is a partial file</div>
    <%= render "Partial" %>

  """

  def render(file, bindings \\ []) do
  	case bindings do
  	  [] ->
        Path.expand(file <> ".html", Weber.Path.__root__ <> "/lib/views/partials") |> EEx.eval_file()
  	  _ ->
  	  	partial = Module.concat(Elixir.Views.Partials, file)
  	  	partial.render_template(bindings)
  	end
  end
end