defmodule #{proj} do

  import Weber

  def start(_type, _args) do
    {:ok, root} = :file.get_cwd()
    run_weber(:#{proj}, Route.get_route, root, Config.config)
  end

  def stop(_state) do
    :ok
  end
end
