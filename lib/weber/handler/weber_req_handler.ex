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

    # get path
    {path, req2} = :cowboy_req.path(req)

    # match routes
    case :lists.flatten(match_routes(path, Weber.Path.__route__, conn.method)) do
      [] ->
        try_to_find_static_resource(path) |> handle_result |> handle_request(req2, state)
      [{:method, _method}, {:path, _matched_path}, {:redirect_path, redirect_path}] ->
        {:redirect, redirect_path} |> handle_result |> handle_request(req2, state)
      [{:method, _method}, {:path, matched_path}, {:controller, controller}, {:action, action}] ->
        req3 = case Keyword.get(state.config, :use_sessions) do
          true ->
            # Check cookie
            cookie = case Weber.Http.Params.get_cookie_p("weber", req2) do
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
            :cowboy_req.set_resp_cookie("weber", cookie, [{:max_age, max_age}], req2)
          _ ->
            req2
        end

        case Keyword.get(state.config, :use_internationalization) do
          true ->
            # get accept language
            lang = case get_lang(:cowboy_req.header("accept-language", req)) do
                     :undefined -> "en_US"
                     l -> String.replace(l, "-", "_") 
                   end
            # check 'lang' process
            locale_process = Process.whereis(binary_to_atom(lang <> ".json"))
            case locale_process do
              nil ->
                case File.read(Weber.Path.__root__ <> "/deps/weber/lib/weber/i18n/localization/locale/" <> lang <> ".json") do
                  {:ok, locale_data} -> Weber.Localization.Locale.start_link(binary_to_atom(lang <> ".json"), locale_data)
                  _ -> :ok
                end
              _ -> :ok
            end
            # update accept language
            set_session_val(conn, :locale, lang)
          _ ->
            :ok
        end

        # get response from controller
        result = Module.function(controller, action, 2).(getAllBinding(path, matched_path), conn)
        # handle controller's response, see in Handler.WeberReqHandler.Result
        handle_result(result, conn, controller) |> handle_request(req3, state)
    end
  end

  def terminate(_reason, _req, _state) do
    :ok
  end

  #
  #  Try to find static resource and send response
  #
  defp try_to_find_static_resource(path) do
    resource = List.last(:string.tokens(:erlang.binary_to_list(path), '/'))
    case find_static_file_path(Weber.Path.__static__, resource) do
      [] ->
        {:not_found, get404, []}
      [resource_name] ->
        {:file, resource_name, [{"content-type", get_mime_type(resource_name)}]}
    end
  end

  #
  # Get mimetype
  #
  defp get_mime_type(resource) do
    case :filename.extension(resource) do
      '.css' -> "text/css"
      '.gif' -> "image/gif"
      '.html' -> "text/html"
      '.htm' -> "text/html"
      '.ico' -> "image/x-icon"
      '.jpeg' -> "image/jpeg"
      '.js' -> "application/javascript"
      _ -> "application/octet-stream"
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