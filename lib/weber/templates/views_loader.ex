defmodule Weber.Templates.ViewsLoader do

  import Weber.Utils

  defmacro compile_views do
    {_, root} = unquote :file.get_cwd
    views = Enum.filter(get_all_files(root ++ '/lib/views/'), fn(f) -> :filename.extension(f) == '.html' end)
    lc view inlist views do
      quote do
        defmodule unquote(Weber.Utils.build_module_name(view)) do
          def __view__, do: unquote(add_helpers_imports(File.read!(view))) 
        end
      end
    end
  end

end
