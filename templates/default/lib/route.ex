defmodule Route do

  import Weber.Route
  require Weber.Route
  
  route on("/", :#{projectNamespace}.Main, :action)

end
