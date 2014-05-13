defmodule Route do

  import Weber.Route
  require Weber.Route
  
  route on("GET", "/weber", :TestTestTest.Main, :action)
     |> on("GET", "/include", :TestTestTest.Include, :include_action)
     |> on("GET", "/json/action", :TestTestTest.JSON, :json_action)
     |> on("GET", "/add/:username", :TestTestTest.Main, :add_username_action)
     |> on("GET", "/delete/:username/id/:id", :TestTestTest.Main, :delete_username_action)
     |> on("GET", "/content_for", :TestTestTest.ContentFor, :content_for_action)
     |> on("GET", "/partials", :TestTestTest.Partials, :partials)
     |> on("GET", "/unauthorized", :TestTestTest.Exceptions, :unauthorized_action)
     |> on("GET", "/unknown", :TestTestTest.Exceptions, :error_500_action)
     |> redirect("GET", "/redirect", "/weber")

end
