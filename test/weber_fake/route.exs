Code.require_file "lib/controllers/test.exs", __DIR__

defmodule Example.Route do
  import Weber.Route

  @route on("GET", "/", :Example.Test, :render_site)

  def get_route do
    @route
  end
end