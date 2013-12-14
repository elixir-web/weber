defmodule Weber.Helper.ContentFor do
  @moduledoc """
  This module create content_for.
  Please note for your layouts just put <%= content_for_layout %>
  """

  @doc "Variable file_content changes to layout correct and view render"
  defmacro content_for(:layout, path) do
    quote do
      if unquote(path) do
        layout_path = Weber.Path.__root__ <> "/lib/views/" <> "layouts/" <> unquote(path)
        var!(content) = EEx.eval_file(layout_path, [content_for_layout: var!(content)])
      end
    end
  end

end