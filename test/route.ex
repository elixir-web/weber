defmodule Route do

  import Weber.Route
  require Weber.Route
  
  route on("GET", "/weber", :TestTestTest.Main, :action)
     |> on("GET", "/include", :TestTestTest.Include, :include_action)
     |> on("GET", "/add/:username", :TestTestTest.Main, :add_username_action)
     |> on("GET", "/delete/:username/id/:id", :TestTestTest.Main, :delete_username_action)
     |> redirect("GET", "/redirect", "/weber")

end