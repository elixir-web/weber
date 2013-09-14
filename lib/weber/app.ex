defmodule Weber.App do
    use GenServer.Behaviour

    @moduledoc """
      Weber web application initial process (Starts cowboy, parse routes, and etc...)
    """

    defrecord WeberApp,
      name:   nil, 
      routes: nil,
      config: nil,
      root:   nil,
      static_dir: nil,
      views_dir:  nil
    
    def start_link(app_name, routes, root_directory, config) do
        :gen_server.start_link({:local, app_name}, __MODULE__, [app_name, routes, root_directory, config], [])
    end

    def init([app_name, routes, root_directory, config]) do
        :gen_server.cast(:erlang.self(), :init)
        { :ok, WeberApp.new name: app_name, 
                            routes: routes, 
                            root: root_directory, 
                            config: config,
                            static_dir: root_directory ++ '/lib/static/',
                            views_dir:  root_directory ++ '/lib/views/' }
    end

    def handle_cast(:init, state) do
        Cowboy.start(state.name, state.config)
        {:noreply, state}
    end

    def handle_call(:routes, _from, state) do
      { :reply, state.routes, state }
    end

    def handle_call(:config, _from, state) do
      { :reply, state.config, state }
    end

    def handle_call(:root, _from, state) do
      { :reply, state.root, state }
    end
end