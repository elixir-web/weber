defmodule Weber.App do
    use GenServer.Behaviour

    @moduledoc """
      Weber web application initial process (Starts cowboy, parse routes, and etc...)
    """

    defrecord WeberApp, 
      routes: nil,
      config: nil,
      root:   nil
    
    def start_link(routes, root_directory, config) do
        :gen_server.start_link({:local, :app}, __MODULE__, [routes, root_directory, config], [])
    end

    def init([routes, root_directory, config]) do
        :gen_server.cast(:erlang.self(), :init)
        { :ok, WeberApp.new routes: routes, root: root_directory, config: config }
    end

    def handle_cast(:init, state) do
        Cowboy.start(state.config)
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