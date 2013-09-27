defmodule Weber.Helper do
  def include_view(file, args // []) do
    views_path = :gen_server.call(Mix.Project.get, :views)
    file = file |> Path.expand(views_path)
    EEx.eval_file(file, args)
  end
end
