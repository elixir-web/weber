defmodule Simpletodo.Main do

	import Simplemodel

    def action("GET", []) do
        {:render, [project: "simpleTodo"]}
    end

    def add("POST", [binding: body]) do
    	new(body)
        {:json, [response: "ok"]}
    end

end
