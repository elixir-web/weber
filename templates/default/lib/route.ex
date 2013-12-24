defmodule Route do

  import Weber.Route
  require Weber.Route

  route on("GET", "/", :#{projectNamespace}.Main, :index)

end
