defmodule Handler.WeberReqHandler do
    
  @moduledoc """
    Weber http request cowboy handler.
  """

  import Weber.Utils
  import Weber.Route
  import Weber.Http.Url
  import Handler.Weber404Handler

  defrecord State, 
    app_name: nil

  def init(_transport, req, name) do
    case :ets.lookup(:req_storage, self) do
      [] -> :ets.insert(:req_storage, {self, req})
      _  -> 
        :ets.delete(:req_storage, self)
        :ets.insert(:req_storage, {self, req})
      end
    {:ok, req, State.new app_name: name}
  end

  def handle(req, state) do
    # get method
    {method, req2} = :cowboy_req.method(req)
    # get path
    {path, req3} = :cowboy_req.path(req2)

    routes = :gen_server.call(state.app_name, :routes)
    static = :gen_server.call(state.app_name, :static)
    views = :gen_server.call(state.app_name,  :views)
    root = :gen_server.call(state.app_name,  :root)
    
    case :lists.flatten(match_routes(path, routes)) do
      [] -> 
        res = try_to_find_static_resource(path, static, views, root)
        case res do
          404 ->
            {:ok, req4} = :cowboy_req.reply(200, [], get404, req3)
            {:ok, req4, state}  
          _ ->
            {:ok, data} = File.read(res)
            {:ok, req4} = :cowboy_req.reply(200, [{"Content-Type", :mimetypes.filename(res)}], data, req3)
            {:ok, req4, state}  
        end              
      [{:path, matched_path}, {:controller, controller}, {:action, action}] ->
        # get response from controller
        result = Module.function(controller, action, 2).(method, getAllBinding(path, matched_path))
        # handle controller's response
        res = handle_result(result, controller, views)
        case res do
          {:redirect, location} ->
            {:ok, req4} = :cowboy_req.reply(301, [{"Location", location}, {"Cache-Control", "no-store"}], <<"">>, req3)
            {:ok, req4, state}
          {:nothing, headers} ->
            {:ok, req4} = :cowboy_req.reply(200, headers, <<"">>, req3)
            {:ok, req4, state}
          {data, headers} ->
            case res do
              {:render, _, _} ->
                {:ok, req4} = :cowboy_req.reply(200, [{"Content-Type", "text/html"} | headers], data, req3)
              _ ->
                {:ok, req4} = :cowboy_req.reply(200, headers, data, req3)
            end
            {:ok, req4, state}
        end
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
        #
        # TODO remake with |>
        #
        filename = :erlang.binary_to_list(String.downcase(:erlang.list_to_binary(List.last(:string.tokens(atom_to_list(controller), '.')) ++ ".html")))
        views_filenames = get_all_files(views)
        view_file = find_file_path(views_filenames, filename)
        case view_file do
          [] ->
            :io.format("[Error] No view file ~n")
          _ ->
            {:ok, d} = File.read(:lists.nth(1, view_file))
            {(EEx.eval_string d, data), headers}
        end
      {:redirect, location} ->
        {:redirect, location}
      {:nothing, headers} ->
        {:nothing, headers};
      {:json, data, headers} ->
        {JSON.generate(data), headers}
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