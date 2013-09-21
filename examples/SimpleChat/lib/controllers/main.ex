defmodule Simplechat.Main.Login do

    def render_login("GET", []) do
        {:render, [project: "SimpleChat"], []}
    end

    def join("POST", [{"username", username}]) do
    	{:json, [chat: username], [{"Content-Type", "application/json"}]}
    end

end