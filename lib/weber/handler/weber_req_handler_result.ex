defmodule Handler.WeberReqHandler.Result do
  @moduledoc """
  This module provides the handle result in production mode
  """

  import Weber.Utils
  alias Handler.WeberReqHandler.Default
  require Weber.Helper.ContentFor

  defrecord App,
    controller: nil,
    action: nil,
    conn:  nil

  @doc "Handle response from controller"
  def handle_result(res, conn // nil, controller // nil, action // nil) do
    request(res, App.new conn: conn, controller: controller, action: action)
  end

  defp request({:render, data}, app) do
    request({:render, data, []}, app)
  end

  defp request({:render, data, headers}, app) do
    file_content = Module.concat([Elixir, Views, List.last(Module.split app.controller), app.action])
    case :lists.keyfind(:__layout__, 1, app.controller.__info__(:functions)) do
      false ->
        {:render, 200, file_content.render_template(:lists.append(data, [conn: app.conn])), headers}
      _ ->
        content = file_content.render_template(:lists.append(data, [conn: app.conn]))
        Weber.Helper.ContentFor.content_for(:layout, app.controller.__layout__, data)
        {:render, 200, content, headers}
    end
  end

  defp request(res, app) do
    Default.request(res, app)
  end

end
