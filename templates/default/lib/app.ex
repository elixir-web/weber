defmodule #{projectNamespace} do
  
  def start(_type, _args) do
  	# get project root
    {_, root} = File.cwd!
  	# compile all views
    Weber.Templates.ViewsLoader.compile_views(root)
    # start weber application
    Weber.run_weber
  end

  def stop(_state) do
    :ok
  end
  
end