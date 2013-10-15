defmodule Handler.WeberReqHandler.Result do
  @moduledoc """
  This module provides the handle result
  """
  import Weber.Utils

  defrecord App,
    controller: nil,
    views: nil
    
  @doc "Handle response from controller"
  def handle_result(res, controller, views) do
    app = App.new controller: controller, views: views
    request(res, app)
  end

  defp request({:render, data, headers}, app) do
    filename = atom_to_list(app.controller)
                 |> :string.tokens('.')
                 |> List.last
                 |> :lists.append(".html")
                 |> :erlang.list_to_binary
                 |> String.downcase
                 |> :erlang.binary_to_list
    {:ok, file_content} = File.read(:lists.nth(1, find_file_path(get_all_files(app.views), filename)))
    {:render, (EEx.eval_string file_content, data), headers}
  end

  defp request({:render_inline, data, params, headers}, _app) do
    {:render, (EEx.eval_string data, params), headers}
  end

  defp request({:file, path, headers}, _app) do
    {:ok, file_content} = File.read(path)
    {:file, file_content, :lists.append([{"Content-Type", :mimetypes.filename(path)}], headers)}
  end

  defp request({:redirect, location}, _app) do
    {:redirect, location}
  end

  defp request({:nothing, headers}, _app) do
    {:nothing, headers}
  end

  defp request({:text, data, headers}, _app) do
    {:text, data, headers}
  end
  
  defp request({:json, data, headers}, _app) do
    {:json, JSON.generate(data), headers}
  end
end