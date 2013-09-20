defmodule Route do

    import Weber.Route

    @route on("/", :Simplechat.Main.Login, :render_login)
        |> on("/join/:username", :Simplechat.Main.Login, :join)
        |> on("/chat/:username", :Simplechat.Main.Chat, :render_chat)
        |> on("/websocket/add", :Simplechat.Main.Websocket, :incoming_message) 

    def get_route do
        @route
    end
end