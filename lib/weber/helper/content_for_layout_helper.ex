defmodule Weber.Helper.ContentFor do
  @moduledoc """
  This module create content_for.
  Please note for your layouts just put <%= content_for_layout %>
  """

  @doc "Variable file_content changes to layout correct and view render"
  defmacro content_for(:layout, path, data) do
    quote do
      if unquote(path) do
        layout_path = Weber.Path.__root__ <> "/lib/views/" <> "layouts/" <> unquote(path)
        layout_module = build_module_name(layout_path)
        var!(content) = layout_module.render_template(:lists.append([content_for_layout: var!(content)], unquote(data)))
      end
    end
  end

end