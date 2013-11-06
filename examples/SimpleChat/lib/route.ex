defmodule Route do

  import Weber.Route
    
  require Weber.Route

  route on("GET",  "/", :Simplechat.Main.Login, :render_login)
     |> on("ANY", "/join/:username", :Simplechat.Main.Login, :join)
     |> on("ANY", "/chat", :Simplechat.Main.Chat, :render_chat)
            
end