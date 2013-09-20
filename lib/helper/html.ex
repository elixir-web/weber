defmodule Weber.Helper.Html do
  def tag(html_tag, content, input_html // []) do
    initial_tag = create_initial_tag(html_tag, input_html) 
    end_tag = create_end_tag(html_tag)

    "#{initial_tag}#{content}#{end_tag}"
  end

  defp create_initial_tag(html_tag, input_html) do
    "<#{html_tag}#{generate_input_html(input_html)}" 
  end

  defp generate_input_html([]) do
    ">"
  end

  defp generate_input_html([{attr, value}| tail]) when is_boolean(value) do
    " #{attr}#{generate_input_html(tail)}"
  end

  defp generate_input_html([{attr, value}| tail]) do
    " #{attr}=\"#{value}\"#{generate_input_html(tail)}"
  end

  defp create_end_tag(html_tag) do
    "</#{html_tag}>"
  end
end
