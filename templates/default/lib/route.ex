defmodule Route do

  import Weber.Route
  require Weber.Route

  route on("GET", "/", :#{projectNamespace}.Main, :action)

end
