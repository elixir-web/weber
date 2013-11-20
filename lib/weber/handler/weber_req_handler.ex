defmodule Handler.WeberReqHandler do
    
  @moduledoc """
    Weber http request cowboy handler.
  """

  import Weber.Utils
  import Weber.Route
  import Weber.Session
  import Weber.Http.Url

  import Plug.Connection
  
  import Handler.Weber404Handler
  import Handler.WeberReqHandler.Result
  import Handler.WeberReqHandler.Response

  @connection Plug.Adapters.Cowboy.Connection

  defrecord State, 
    config: nil

  def init({:tcp, :http}, req, config) do
    {:ok, req, State.new config: config }
  end

  def handle(req, state) do
    conn = @connection.conn(req, :tcp)
    conn = assign(conn, :req, req)

    root = Weber.Path.__root__
    views = Weber.Path.__views__
    static = Weber.Path.__static__

    # get method
    {method, req2} = :cowboy_req.method(req)
    # get path
    {path, req3} = :cowboy_req.path(req2)
    
    # match routes
    case :lists.flatten(match_routes(path, Route.__route__, method)) do
      [] ->
        # Get static file or page not found
        try_to_find_static_resource(path, static, views, root) |> handle_result |> handle_request(req3, state)
      [{:method, _method}, {:path, matched_path}, {:controller, controller}, {:action, action}] ->
        # Check cookie
        cookie = case Weber.Http.Params.get_cookie_p("weber", req3) do
          :undefined ->
            session_id = Weber.Http.Cookie.generate_session_id
            :gen_server.cast(:session_manager, {:create_new_session, session_id, self})
            session_id |> :erlang.binary_to_list |> :lists.concat |> :lists.concat |> :erlang.list_to_binary
          weber_cookie ->
            :gen_server.cast(:session_manager, {:check_cookie, weber_cookie, self})
            weber_cookie
        end
        
        # set up cookie
        {_, session}  = :lists.keyfind(:session, 1, state.config)
        {_, max_age}  = :lists.keyfind(:max_age, 1, session)
        req4 = :cowboy_req.set_resp_cookie("weber", cookie, [{:max_age, max_age}], req3)

        # get accept language
        lang = case get_lang(:cowboy_req.header("accept-language", req)) do
                 :undefined -> "en_US"
                 l -> String.replace(l, "-", "_") 
               end

        # check 'lang' process
        locale_process = Process.whereis(binary_to_atom(lang <> ".json"))
        case locale_process do
          nil -> 
            case File.read(root <> "/deps/weber/lib/weber/i18n/localization/locale/" <> lang <> ".json") do
              {:ok, locale_data} -> Weber.Localization.Locale.start_link(binary_to_atom(lang <> ".json"), locale_data)
              _ -> :ok
            end
          _ -> :ok
        end

        # update accept language
        set_session_val(conn, :locale, lang)
        # get response from controller
        result = Module.function(controller, action, 2).(getAllBinding(path, matched_path), conn)
        # handle controller's response, see in Handler.WeberReqHandler.Result
        handle_result(result, conn, controller, views) |> handle_request(req4, state)
    end
  end

  def terminate(_reason, _req, _state) do
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