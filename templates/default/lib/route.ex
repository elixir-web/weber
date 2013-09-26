defmodule Route do

  import Weber.Route

  @route on("/", :#{projectNamespace}.Main, :action)

  def get_route do
    @route
  end
end
