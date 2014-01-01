defmodule Route do

  import Weber.Route
  require Weber.Route

  route on("GET", "/", :GenTest.Main, :index)

end
