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

  defp request({:render, data}, app) do
    request({:render, data, []}, app)
  end

  defp request({:render, data, headers}, app) do
    filename = List.last(Module.split app.controller) <> ".html"
    file_content = find_file_path(Weber.Path.__views__, filename) |> elem(1)
    case :lists.keyfind(:__layout__, 1, app.controller.__info__(:functions)) do
      false ->
        {:render, 200, file_content.render_template(:lists.append(data, [conn: app.conn])), headers}
      _ ->
        content = file_content.__view__
        Weber.Helper.ContentFor.content_for(:layout, app.controller.__layout__)
        {:render, 200, EEx.eval_string(content, []), headers}
    end
  end
  
  defp request({:render_other, module, data}, app) do
    request({:render_other, module, data, []}, app)
  end

  defp request({:render_other, module, data, headers}, app) do
    {:render, 200, module.render_template(:lists.append(data, [conn: app.conn])), headers}
  end
  
  defp request({:render_inline, data, params}, _app) do
    {:render, 200, (EEx.eval_string data, params), []}
  end

  defp request({:file, path, headers}, _app) do
    {:ok, file_content} = File.read(path)
    case :lists.keyfind("content-type", 1, headers) do
      false -> {:file, 200, file_content, :lists.append([{"content-type", "application/octet-stream"}], headers)}
      _ -> 
        {:file, 200, file_content, headers}
    end
  end

  defp request({:redirect, location}, _app) do
    {:redirect, 302, "", [{"Location", location}]}
  end

  defp request({:nothing, headers}, _app) do
    {:nothing, 200, "", headers}
  end

  defp request({:text, data}, app) do
    {:text, 200, data, []}
  end

  defp request({:text, data, headers}, _app) do
    {:text, 200, data, headers}
  end
  
  defp request({:json, data}, app) do
    request({:json, data, []}, app)
  end  

  defp request({:json, data, headers}, _app) do
    {:json, 200, JSON.generate(data), :lists.append([{"Content-Type", "application/json"}], headers)}
  end

  defp request({:not_found, data, _headers}, _app) do
    {:not_found, 404, data, [{"Content-Type", "text/html"}]}
  end
end