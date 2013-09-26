defmodule Room do
    use GenServer.Behaviour

    def start_link() do
        :gen_server.start_link({:local, :room}, __MODULE__, [], [])
    end

    def init([]) do
        {:ok, []}
    end

    def handle_cast({:new_user, pid}, state) do
        {:noreply, [pid | state] }
    end

    def handle_cast({:delete_user, pid}, state) do
        {:noreply, :lists.delete(pid, state)}
    end

    def handle_cast({:send, message}, state) do    
        Enum.each(state, fn(pid) -> 
            pid <- message 
        end)
        {:noreply, state}
    end

end