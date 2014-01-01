defmodule GenTest.Main do

  use Weber.Controller

  layout "application.html"

  def index([], _conn) do
    {:render, [project: "Cool project", title: "Hello World from Weber"], []}
  end

end
