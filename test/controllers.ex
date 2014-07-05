defmodule TestTestTest.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], [{"content-type", "text/html"}]}
  end

  def action1([binding: value], _conn) do
    {:text, "Main Controller: " <> value, []}
  end

end

defmodule TestTestTest.Include do

  use Weber.Controller

  layout false
  def include_action([], _conn) do
    {:render, [], []}
  end

end

defmodule TestTestTest.ContentFor do
  use Weber.Controller

  layout "Layout.html"

  def content_for_action([], _conn) do
    {:render, [], []}
  end

end

defmodule TestTestTest.Partials do
  use Weber.Controller

  layout false
  def partials([], _conn) do
    {:render, [], [{"content-type", "text/html"}]}
  end
end

defmodule TestTestTest.JSON do
  use Weber.Controller

  layout false
  def json_action([], _conn) do
    {:json, 201, [], []}
  end
end

defmodule TestTestTest.Exceptions do
  use Weber.Controller

  layout false

  render_when_raise :unknown, {:text, 500, "An unknown error occurred", []}
  render_when_raise :unauthorized, {:text, 401, "Unauthorized", []}

  def unauthorized_action([], _conn) do
    if true do
      raise_and_render :unauthorized
    end
    {:json, 200, [], []}
  end

  def error_500_action([], _conn) do
    raise_and_render :unknown
    {:json, 200, [], []}
  end

end


defmodule TestTestTest.Redirect do
  use Weber.Controller

  layout false

  def render_other_action([], _conn) do
    {:render_other_action, :action1, [binding: "original value"]}
  end

  def render_other_controller([], _conn) do
    {:render_other_controller, :TestTestTest.Main, :action1, [binding: "original value"]}
  end

  def action1([binding: value], _conn) do
    {:text, value, []}
  end

end
