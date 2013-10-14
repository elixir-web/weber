defmodule Handler.WeberReqHandler do
    
  @moduledoc """
    Weber http request cowboy handler.
  """

  import Weber.Utils
  import Weber.Route
  import Weber.Http.Url
  import Handler.Weber404Handler

  defrecord State, 
    app_name: nil,
    cookie:   nil

  def init(_transport, req, name) do
    case :ets.lookup(:req_storage, self) do
      [] -> 
        :ets.insert(:req_storage, {self, req})
      _  -> 
        :ets.delete(:req_storage, self)
        :ets.insert(:req_storage, {self, req})
    end
        
    {:ok, req, State.new app_name: name }
  end

  def handle(req, state) do
    # get method
    {method, req2} = :cowboy_req.method(req)
    # get path
    {path, req3} = :cowboy_req.path(req2)

    config = :gen_server.call(state.app_name, :config)
    routes = :gen_server.call(state.app_name, :routes)
    static = :gen_server.call(state.app_name, :static)
    views = :gen_server.call(state.app_name,  :views)
    root = :gen_server.call(state.app_name,  :root)

    case :lists.flatten(match_routes(path, routes)) do
      [] -> 
        case try_to_find_static_resource(path, static, views, root) do
          404 ->
            {:ok, req4} = :cowboy_req.reply(200, [], get404, req3)
            {:ok, req4, state}  
          res ->
            {:ok, data} = File.read(res)
            {:ok, req4} = :cowboy_req.reply(200, [{"Content-Type", :mimetypes.filename(res)}], data, req3)
            {:ok, req4, state}  
        end              
      [{:path, matched_path}, {:controller, controller}, {:action, action}] ->
        cookie = case Weber.Http.Params.cookies do
          [] ->
            :gen_server.call(:session_manager, {:create_new_session, Weber.Http.Cookie.generate_session_id, self}) 
              |> :erlang.binary_to_list
              |> :lists.concat
              |> :lists.concat
          _cookie_already_exists ->
            :erlang.binary_to_list(Weber.Http.Params.get_cookie("weber")) 
        end

        # set up cookie
        {_, session}  = :lists.keyfind(:session, 1, config)
        {_, max_age}  = :lists.keyfind(:max_age, 1, session)
        req4 = :cowboy_req.set_resp_cookie("weber", cookie, [{:max_age, max_age}], req3)
        # get response from controller
        result = Module.function(controller, action, 2).(method, getAllBinding(path, matched_path))
        # handle controller's response
        res = handle_result(result, controller, views)
        case res do
          {:redirect, location} ->
            {:ok, req5} = :cowboy_req.reply(301, [{"Location", location}, {"Cache-Control", "no-store"}], <<"">>, req4)
          {:nothing, headers} ->
            {:ok, req5} = :cowboy_req.reply(200, headers, <<"">>, req4)
          {:text, data, headers} ->
            {:ok, req5} = :cowboy_req.reply(200, :lists.append([{"Content-Type", "plain/text"}], headers), data, req4)
          {:json, data, headers} ->
            {:ok, req5} = :cowboy_req.reply(200, :lists.append([{"Content-Type", "application/json"}], headers), data, req4)
          {:file, data, headers} ->
            {:ok, req5} = :cowboy_req.reply(200, headers, data, req4)
          {:render, data, headers} ->
            {:ok, req5} = :cowboy_req.reply(200, [{"Content-Type", "text/html"} | headers], data, req4)
        end
        {:ok, req5, state}
    end
  end

  def terminate(_reason, _req, _state) do
    :ets.delete(:req_storage, self)
    :ok
  end

  #
  #  Handle response from controller
  #
  defp handle_result(res, controller, views) do
    case res do
      {:render, data, headers} -> 
        filename = atom_to_list(controller) 
                       |> :string.tokens('.') 
                       |> List.last
                       |> :lists.append(".html")
                       |> :erlang.list_to_binary
                       |> String.downcase
                       |> :erlang.binary_to_list
        {:ok, file_content} = File.read(:lists.nth(1, find_file_path(get_all_files(views), filename)))
        {:render, (EEx.eval_string file_content, data), headers}
      {:render_inline, data, params, headers} ->
        {:render, (EEx.eval_string data, params), headers}
      {:file, path, headers} ->
        {:ok, file_content} = File.read(path)
        {:file, file_content, :lists.append([{"Content-Type", :mimetypes.filename(path)}], headers)}
      {:redirect, location} ->
        {:redirect, location}
      {:nothing, headers} ->
        {:nothing, headers}
      {:text, data, headers} ->
        {:text, data, headers}
      {:json, data, headers} ->
        {:json, JSON.generate(data), headers}
    end
  end

  #
  #  Try to find static resource and send response
  #
  defp try_to_find_static_resource(path, static, views, _root) do
    resource = List.last(:string.tokens(:erlang.binary_to_list(path), '/'))
    case find_file_path(get_all_files(views), resource) do
      [] ->
        case find_file_path(get_all_files(static), resource) do
          [] ->
            404
          [resource_name] ->
            resource_name
        end
      [resource_name] ->
        resource_name
    end
  end
end