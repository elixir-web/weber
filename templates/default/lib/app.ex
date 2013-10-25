defmodule #{projectNamespace} do

  import Weber

  def start(_type, _args) do
    {:ok, root} = :file.get_cwd()
    run_weber(:#{projectName}, Route.__route__, root, Config.config)
  end

  def stop(_state) do
    :ok
  end
end
