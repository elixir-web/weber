defmodule Handler.WeberReqHandler.Default do

  def request({:render_inline, data, params}, _app) do
    {:render, 200, (EEx.eval_string data, assigns: params), []}
  end

  def request({:file, path, headers}, _app) do
    {:ok, file_content} = File.read(path)
    case :lists.keyfind("content-type", 1, headers) do
      false -> {:file, 200, file_content, :lists.append([{"content-type", "application/octet-stream"}], headers)}
      _ ->
        {:file, 200, file_content, headers}
    end
  end

  def request({:redirect, location}, _app) do
    {:redirect, 302, "", [{"Location", location}]}
  end

  def request({:nothing, headers}, _app) do
    {:nothing, 200, "", headers}
  end

  def request({:text, data}, _app) do
    {:text, 200, data, []}
  end

  def request({:text, data, headers}, _app) do
    {:text, 200, data, headers}
  end

  def request({:text, status, data, headers}, _app) do
    {:text, status, data, headers}
  end

  def request({:json, data}, app) do
    request({:json, data, []}, app)
  end

  def request({:json, data, headers}, _app) do
    {:ok, json} = JSEX.encode(data)
    {:json, 200, json, :lists.append([{"Content-Type", "application/json"}], headers)}
  end

  def request({:json, status, data, headers}, _app) do
    {:ok, json} = JSEX.encode(data)
    {:json, status, json, :lists.append([{"Content-Type", "application/json"}], headers)}
  end

  def request({:not_found, data, _headers}, _app) do
    {:not_found, 404, data, [{"Content-Type", "text/html"}]}
  end

  def request({:render_other_action, action, data}, app) do
    complete_action = (Atom.to_string(app.controller) <> "#" <> Atom.to_string(action)) |> String.slice(7..-1)
    request({:render_other_controller, complete_action, data}, app)
  end

  def request({:render_other_controller, complete_action, data}, app) do
    {:render_other_controller, complete_action, data}
  end

  def request({:render_other_controller, controller, action, data}, app) when is_atom(controller) and is_atom(action) do
    {:render_other_controller, Atom.to_string(controller) <> "#" <> Atom.to_string(action), data}
  end

end
