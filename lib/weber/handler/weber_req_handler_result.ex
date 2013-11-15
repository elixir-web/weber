defmodule Handler.WeberReqHandler.Result do
  @moduledoc """
  This module provides the handle result
  """

  import Weber.Utils
  require Weber.Helper.ContentFor

  defrecord App,
    controller: nil,
    views: nil
    
  @doc "Handle response from controller"
  def handle_result(res, controller // nil, views // nil) do
    app = App.new controller: controller, views: views
    request(res, app)
  end

  defp request({:render, data, headers}, app) do
    filename = String.downcase List.last(Module.split app.controller) <> ".html" 
    file_content = build_module_name(:lists.nth(1, find_file_path(get_all_files(app.views), filename)))
    :io.format("file_content ~p~n", [file_content])

    Weber.Helper.ContentFor.content_for(:layout, app.controller.__layout__)

    {:render, 200, EEx.eval_string(file_content.__view__(), data), headers}
  end

  defp request({:render_other, filename, headers}, app) do
    {:ok, file_content} = File.read(:lists.nth(1, find_file_path(get_all_files(app.views), filename)))
    Weber.Helper.ContentFor.content_for(:layout, app.controller.__layout__)
    {:render, 200, EEx.eval_string(file_content, []), headers} 
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