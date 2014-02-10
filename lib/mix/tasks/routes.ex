defmodule Mix.Tasks.Weber.Routes do

  def run([]) do
    routes = Route.__route__
    :io.format("~n")
    :io.format("~p routing: ~n", [Keyword.fetch!(Mix.Project.config(), :app)])
    :io.format("~n")
    Enum.each(routes, fn(route) ->
      case routes do
         [[method: method, path: path, controller: controller, action: action]] ->
            :io.format("  ~p  ~p  ~p  ~p ~n", [method, path, controller, action])
         [[method: method, path: path, redirect_path: redirect_path]] ->
            :io.format("  ~p  ~p  ~p ~n", [method, path, redirect_path])
      end
    end)
  end
end