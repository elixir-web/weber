defmodule Simplechat do

    import Weber

    def start(_type, _args) do
    	Room.start_link
        {:ok, root} = :file.get_cwd()
        run_weber(:Simplechat, Route.get_route, root, Config.config)
    end

    def stop(_state) do
       :ok
    end
end