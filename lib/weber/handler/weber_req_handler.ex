defmodule Handler.WeberReqHandler do
    
  @moduledoc """
    Weber http request cowboy handler.
  """

  import Weber.Utils
  import Weber.Route
  import Weber.Session
  import Weber.Http.Url
  
  import Handler.Weber404Handler
  import Handler.WeberReqHandler.Result
  import Handler.WeberReqHandler.Response

  defrecord State, 
    cookie:   nil

  def init({:tcp, :http}, req, _opts) do
    case :ets.lookup(:req_storage, self) do
      [] -> 
        :ets.insert(:req_storage, {self, req})
      _  -> 
        :ets.delete(:req_storage, self)
        :ets.insert(:req_storage, {self, req})
    end
    {:ok, req, {} }
  end

  def handle(req, state) do
    # get project root
    {:ok, root} = :file.get_cwd()
    # views directory
    views = root ++ '/lib/views/'
    # static directory
    static = root ++ '/public/'
    # get method
    {method, req2} = :cowboy_req.method(req)
    # get path
    {path, req3} = :cowboy_req.path(req2)
    
    route = case Code.ensure_loaded?(Route) do
      true -> Route.__route__
      false -> Weber.DefaultRoute.__route__
    end

    config = case Code.ensure_loaded?(Config) do
      true -> Config.config
      false -> Weber.DefaultConfig.config
    end 

    # match routes
    case :lists.flatten(match_routes(path, route, method)) do
      [] ->
        # Get static file or page not found
        try_to_find_static_resource(path, static, views, root) |> handle_result |> handle_request(req3, state)
      [{:method, _method}, {:path, matched_path}, {:controller, controller}, {:action, action}] ->
        # Check cookie
        cookie = case Weber.Http.Params.get_cookie("weber") do
          :undefined ->
            :gen_server.call(:session_manager, {:create_new_session, Weber.Http.Cookie.generate_session_id, self})
          weber_cookie ->
            :gen_server.cast(:session_manager, {:check_cookie, weber_cookie, self})
            weber_cookie
        end
        # set up cookie
        {_, session}  = :lists.keyfind(:session, 1, config)
        {_, max_age}  = :lists.keyfind(:max_age, 1, session)
        req4 = :cowboy_req.set_resp_cookie("weber", cookie, [{:max_age, max_age}], req3)
        # get accept language
        lang = case get_lang(:cowboy_req.header("accept-language", req)) do
                 :undefined -> "en_US"
                 l -> l 
               end
        # update accept language
        set_session_val(:locale, lang)
        # get response from controller
        result = Module.function(controller, action, 1).(getAllBinding(path, matched_path))
        # handle controller's response, see in Handler.WeberReqHandler.Result
        handle_result(result, controller, views) |> handle_request(req4, state)
    end
  end

  def terminate(_reason, _req, _state) do
    :ets.delete(:req_storage, self)
    :ok
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
            {:not_found, get404, []}
          [resource_name] ->
            {:file, resource_name, []}
        end
      [resource_name] ->
        {:file, resource_name, []}
    end
  end

  #
  # Get accept language
  #
  def get_lang({:undefined, _}) do
    :undefined
  end

  def get_lang({l, _}) do
    [lang | _] = :string.tokens(:erlang.binary_to_list(l), ',')
    :erlang.list_to_binary(lang)
  end

end