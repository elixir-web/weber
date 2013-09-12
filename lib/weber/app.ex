defmodule Weber.App do
    use GenServer.Behaviour

    def start_link(routes, root_directory) do
        :gen_server.start_link(__MODULE__, [routes, root_directory], [])
    end

    def init([routes, root_directory]) do
        { :ok, routes }
    end

end