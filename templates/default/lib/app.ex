defmodule #{projectNamespace} do
  
  def start(_type, _args) do
  	# compile all views
    Weber.Templates.ViewsLoader.compile_views
    # start weber application
    Weber.run_weber
  end

  def stop(_state) do
    :ok
  end
  
end