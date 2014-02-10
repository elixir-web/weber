defmodule Handler.WeberReqHandler.Development.Result do

  @moduledoc """
  This module provides the handle result in development mode
  """

  import Weber.Utils
  alias Handler.WeberReqHandler.Default

  require Weber.Helper.ContentFor.Development

  defrecord App,
    controller: nil,
    action: nil,
    conn:  nil

  @doc "Handle response from controller"
  def handle_result(res, conn \\ nil, controller \\ nil, action \\ nil) do
    request(res, App.new conn: conn, controller: controller, action: action)
  end

  defp request({:render, data}, app) do
    request({:render, data, []}, app)
  end

  defp request({:render, data, headers}, app) do
    root = File.cwd!
    controller = Module.split(app.controller) |> List.last |> String.downcase
    {:ok, file_content} = File.read(root <> "/lib/views/" <> controller <> "/" <> String.downcase(app.action) <> ".html")

    case :lists.keyfind(:__layout__, 1, app.controller.__info__(:functions)) do
      false ->
        {:render, 200, (EEx.eval_string add_helpers_imports(file_content), assigns: data), headers}
      _ ->
        Weber.Helper.ContentFor.Development.content_for(:layout, root <> "/lib/views/", app.controller.__layout__, data)
        {:render, 200, file_content, headers}
    end
  end

  defp request(res, app) do
    Default.request(res, app)
  end

end
