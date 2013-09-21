defmodule Simplechat.Main.Chat do

    def render_chat("GET", [{<<"username">>, username}]) do
        {:render, [username: username], []}
    end

    def websocket_init(pid) do
        :gen_server.cast(:room, {:new_user, pid})
    end

    def websocket_message(pid, message) do
        :gen_server.cast(:room, {:send, message})
    end

    def websocket_terminate(pid) do
        :gen_server.cast(:room, {:delete_user, pid})
    end

end