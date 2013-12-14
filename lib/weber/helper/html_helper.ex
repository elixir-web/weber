defmodule Weber.Helper.Html do
  
  @moduledoc """
  HTML generator helper.

      ## Example
    
      # Generates <p class="class_test">test</p>
      def do_something do
        tag(:p, "test", [class: "class_test"])
      end

      # Generates <div id="test"><p>test</p></div>
      def do_something do
        tag(:div, [id: "test"]) do
          tag(:p, "test")
        end
      end
  """

  @html5_tags [:area, :base, :br, :col, :command, :embed, :hr, :img, :input,
               :keygen, :link, :meta, :param, :source, :track, :wbr]

  def content_for_layout do
    ""
  end
  
  @doc """
  generates html.
  """
  def tag(html_tag, clauses) when is_list(clauses) do
    do_clause = Keyword.get(clauses, :do, "")
    case do_clause do
      "" -> tag(html_tag, do_clause, clauses)
      _ -> tag(html_tag, do_clause)
    end
  end

  @doc """
  Generates html.
  """
  def tag(html_tag, input_html, clauses // []) when is_list(input_html) do
    do_clause = Keyword.get(clauses, :do, "")
    tag(html_tag, do_clause, input_html)
  end

  def tag(html_tag, content, input_html) when is_binary(content) do
     _create_complete_tag(html_tag, content, input_html)
  end
  
  defp _create_complete_tag(html_tag, content, input_html) do
    cond do
      html_tag in @html5_tags -> _create_initial_tag(html_tag, input_html)
      true -> _create_initial_tag(html_tag, input_html) <> content <> _create_end_tag(html_tag) 
    end
  end

  defp _create_initial_tag(html_tag, input_html) do
    "<#{html_tag}#{_generate_input_html(input_html)}" 
  end

  defp _generate_input_html([]) do
    ">"
  end

  defp _generate_input_html([{attr, value}| tail]) when is_boolean(value) do
    " #{attr}#{_generate_input_html(tail)}"
  end

  defp _generate_input_html([{attr, value}| tail]) do
    " #{attr}=\"#{value}\"#{_generate_input_html(tail)}"
  end

  defp _create_end_tag(html_tag) do
    "</#{html_tag}>"
  end
  
end