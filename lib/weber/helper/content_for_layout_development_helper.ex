defmodule Weber.Helper.ContentFor.Development do
  @moduledoc """
  This module create content_for.
  Please note for your layouts just put <%= content_for_layout %> in development mode
  """

  @doc "Variable file_content changes to layout correct and view render"
  defmacro content_for(:layout, views, path, data) do
    quote do
      if unquote(path) do
        layout_path = unquote(views) <> "layouts/" <> unquote(path)
        var!(file_content) = EEx.eval_file(layout_path, assigns: :lists.append([content_for_layout: var!(file_content)], unquote(data)))
      end
    end
  end

end