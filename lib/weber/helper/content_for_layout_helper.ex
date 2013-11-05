defmodule Weber.Helper.ContentFor do
  @moduledoc """
  This module create content_for.
  Please note for your layouts just put <%= content_for_layout %>
  """

  @doc "Variable file_content changes to layout correct and view render"
  defmacro content_for(:layout, path) do
    quote do
      if unquote(path) do
        {:ok, root} = :file.get_cwd()
        layout_path = String.from_char_list!(root ++ '/lib/views/' ++ 'layouts/' ++ unquote(path))
        var!(file_content) = EEx.eval_file(layout_path, [content_for_layout: var!(file_content)])
      end
    end
  end

end