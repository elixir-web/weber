defmodule Weber.Helper.Partial do
  
  @moduledoc """
    Partial rendering helper.

    ## Example

    # Generates <div>this is a partial file</div>
    <%= render "partial" %>

  """

  def render(file) do
    Path.expand("_" <> file <> ".html", Weber.Path.__root__ <> "/lib/views/partials") |> EEx.eval_file()
  end
end