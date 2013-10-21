defmodule Weber.Helper.ContentFor do
  @moduledoc """
  This module create content_for.
  Please note for your layouts just put <%= content_for_layout %>
  """
  defmacro content_for(:layout) do
    quote do
      views = :gen_server.call(Mix.Project.get.project[:app], :views)
      #TODO: Discuss about change layout from controller in issue #68
      layout_path = String.from_char_list!(views ++ 'layouts/main.html')

      var!(file_content) = EEx.eval_file(layout_path, [content_for_layout: var!(file_content)])
    end
  end
end