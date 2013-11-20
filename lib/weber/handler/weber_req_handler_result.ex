defmodule Handler.WeberReqHandler.Result do
  @moduledoc """
  This module provides the handle result
  """

  import Weber.Utils
  require Weber.Helper.ContentFor
  
  defrecord App,
    controller: nil,
    views: nil,
    conn:  nil
  
  @doc "Handle response from controller"
  def handle_result(res, conn // nil, controller // nil, views // nil) do
    request(res, App.new conn: conn, controller: controller, views: views)
  end

  defp request({:render, data, headers}, app) do
    filename = List.last(Module.split app.controller) <> ".html"
    file_content = find_file_path(Weber.Path.__views__, filename) |> elem(1)

    Weber.Helper.ContentFor.content_for(:layout, app.controller.__layout__)
    {:render, 200, file_content.render_template(:lists.append(data, [conn: app.conn])), headers}
  end
  
  defp request({:render_inline, data, params, headers}, _app) do
    {:render, 200, (EEx.eval_string data, params), headers}
  end

  defp request({:file, path, headers}, _app) do
    {:ok, file_content} = File.read(path)
    {:file, 200, file_content, :lists.append([{"Content-Type", :mimetypes.filename(path)}], headers)}
  end

  defp request({:redirect, location}, _app) do
    {:redirect, 301, [{"Location", location}, {"Cache-Control", "no-store"}]}
  end

  defp request({:nothing, headers}, _app) do
    {:nothing, 200, headers}
  end

  defp request({:text, data, headers}, _app) do
    {:text, 200, data, :lists.append([{"Content-Type", "plain/text"}], headers)}
  end
  
  defp request({:json, data, headers}, _app) do
    {:json, 200, JSON.generate(data), :lists.append([{"Content-Type", "application/json"}], headers)}
  end

  defp request({:not_found, data, _headers}, _app) do
    {:not_found, 404, data, [{"Content-Type", "text/html"}]}
  end
end