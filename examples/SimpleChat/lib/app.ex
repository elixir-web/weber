defmodule Simplechat do
  
  use Weber.Controller

  require Weber.Templates.ViewsLoader

  layout false
  def start(_type, _args) do
    Room.start_link
     # Set resources
    Weber.Templates.ViewsLoader.set_up_resources(File.cwd!)
    # compile all views
    Weber.Templates.ViewsLoader.compile_views(File.cwd!)
    # start weber
    Weber.run_weber
  end

  def stop(_state) do
    :ok
  end
  
end