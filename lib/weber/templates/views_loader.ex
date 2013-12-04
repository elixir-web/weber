defmodule Weber.Templates.ViewsLoader do
  
  import Path
  import Weber.Utils

  defmacro set_up_resources(path) do
    path = Code.eval_quoted(path, [], file: __ENV__.file, line: __ENV__.line) |> elem(0)
    
    views_list_tmp = Enum.filter(get_all_files(:erlang.binary_to_list(path) ++ '/lib/views/'), 
      fn(f) -> :filename.extension(f) == '.html' 
    end)
    
    views_list = Enum.map(views_list_tmp, fn(view) -> 
      Macro.escape({:erlang.list_to_binary(basename(view)), Weber.Utils.build_module_name(view), view}) 
    end)
    
    static_list = get_all_files(:erlang.binary_to_list(path) ++ '/public/')
    
    quote do
      defmodule unquote(Weber.Path) do
        def __root__, do: unquote(path)
        def __views__, do: unquote(views_list)
        def __static__, do: unquote(static_list)
        def __route__, do: unquote(Route.__route__)
      end
    end
  end
  
  defmacro compile_views(root) do
    root = Code.eval_quoted(root, [], file: __ENV__.file, line: __ENV__.line) |> elem(0)
    views = Enum.filter(get_all_files(:erlang.binary_to_list(root) ++ '/lib/views/'), fn(f) -> :filename.extension(f) == '.html' end)
    lc view inlist views do
      content = add_helpers_imports(File.read!(view))
      quote do
        defmodule unquote(Weber.Utils.build_module_name(view)) do
          require EEx
          
          EEx.function_from_string(:def, :render_template, unquote(content), [:assigns])
          
          def __view__, do: unquote(content)

        end
      end
    end
  end

end