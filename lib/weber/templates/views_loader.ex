defmodule Weber.Templates.ViewsLoader do

  import Weber.Utils

  defmacro compile_views(root) do
    :io.format("root ~p~n", [root])
    :io.format("get_all_files(root ++ '/lib/views/') ~p~n", [get_all_files(root ++ '/lib/views/')])

    views = Enum.filter(get_all_files(root ++ '/lib/views/'), fn(f) -> :filename.extension(f) == '.html' end)

    :io.format("views from compile_views log ~p~n", [views])

    lc view inlist views do
      quote do
        :io.format("Weber.Utils.build_module_name(view) ~p~n", [Weber.Utils.build_module_name(view)])
        defmodule unquote(Weber.Utils.build_module_name(view)) do
          def __view__, do: unquote(File.read!(view)) 
        end
      end
    end
  end

end
