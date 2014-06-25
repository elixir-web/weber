defmodule Handler.WeberReqHandler do

  @moduledoc """
    Weber http request cowboy handler.
  """

  import Weber.Utils
  import Weber.Route
  import Weber.Session
  import Weber.Http.Url
  import Weber.Controller

  require Lager

  import Plug.Conn

  import Handler.Weber404Handler
  import Handler.WeberReqHandler.Response

  @connection Plug.Adapters.Cowboy.Conn

  defmodule State do
    defstruct config: nil, handler: nil
  end

  def init({:tcp, :http}, req, {config, handler}) do
    {:ok, req, %State{config: config, handler: handler} }
  end

  def handle(req, state) do
    conn = @connection.conn(req, :tcp)
    conn = assign(conn, :req, req)

    #if Keyword.get(Config.config, :reload) == true do
    #  Weber.Reload.purge()
    #end
    
    # get path
    {path, req2} = :cowboy_req.path(req)

    case :lists.keyfind(:log, 1, Config.config) do
      {:ok, true} -> 
        Lager.info  "[" <> get_time() <> "]" <> " " <> "[" <> conn.method <> "] " <> path
      _ -> :ok
    end

    # match routes
    case match_routes(path, Weber.Path.__route__, conn.method) do
      [] ->
        try_to_find_static_resource(path) |> state.handler.handle_result |> handle_request(req2, state)
      [{:method, _method}, {:path, _matched_path}, {:redirect_path, redirect_path}] ->
        {:redirect, redirect_path} |> state.handler.handle_result |> handle_request(req2, state)
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
            locale_process = Process.whereis(String.to_atom(lang <> ".json"))
            case locale_process do
              nil ->
                case File.read(Weber.Path.__root__ <> "/deps/weber/lib/weber/i18n/localization/locale/" <> lang <> ".json") do
                  {:ok, locale_data} -> Weber.Localization.Locale.start_link(String.to_atom(lang <> ".json"), locale_data)
                  _ -> :ok
                end
              _ -> :ok
            end
            # update accept language
            set_session_val(conn, :locale, lang)
          _ -> :ok
        end

        conn = case :lists.keyfind(:__before__, 2, controller.__info__(:functions)) do
          false -> conn
          _ -> controller.__before__(action, conn)
        end

        get_response(controller, action, getAllBinding(path, matched_path), conn, req3, state)
    end
  end

  def get_response(controller, action, data, conn, req, state) do
    result = try do
      Module.function(controller, action, 2).(data, conn)
    rescue
      e in WeberControllerException ->
        if e.message in controller.raise_keys do
          controller.render_value_for_key(e.message)
        else
          IO.ANSI.escape("%{red}    #{e.message}\n" <> Exception.format_stacktrace(System.stacktrace)) |> IO.puts
          reraise e, System.stacktrace
        end
    end

    result = state.handler.handle_result(result, conn, controller, Weber.Utils.capitalize(Atom.to_string(action)))
    # check for action/controller redirect
    case result do
      {:render_other_controller, _, _} ->
        [controller, action] = result |> elem(1) |> String.split("#")
        {controller, _} = Code.eval_string(controller)
        get_response(controller, String.to_atom(action), result |> elem(2), conn, req, state)
      _ ->
        handle_request(result, req, state, {controller, action, conn})
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
