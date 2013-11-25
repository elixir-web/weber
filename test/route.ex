defmodule Route do

  import Weber.Route
  require Weber.Route
  
  route on("GET", "/weber", :TestTestTest.Main, :action)
     |> on("GET", "/include", :TestTestTest.IncludeTest, :include_action)

end