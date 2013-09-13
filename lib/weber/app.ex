defmodule Weber.App do
    use GenServer.Behaviour

    defrecord WeberApp, 
      routes: nil,
      config: nil,
      root:   nil

    def start_link(routes, root_directory, config) do
        :gen_server.start_link(__MODULE__, [routes, root_directory, config], [])
    end

    def init([routes, root_directory, config]) do
    	  { :ok, WeberApp.new routes: routes, root: root_directory, config: config }
    end

    def handle_cast do
    end

end