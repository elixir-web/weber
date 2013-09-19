defmodule Handler.WeberReqHandler do
    
    @moduledoc """
        Weber http request cowboy handler.
    """

    import Weber.Route
    import Weber.Http.Url
    import Handler.Weber404Handler

    defrecord State, 
        app_name: nil

    def init(_transport, req, name) do
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
        
        successful_route = :lists.flatten(match_routes(path, routes))

        case successful_route do
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
                res = handle_result(result, controller, views, static)

                case res do
                    {:redirect, location} ->
                        {:ok, req4} = :cowboy_req.reply(301, [{"Location", "/chat.html"}, {"Cache-Control", "no-store"}], <<"">>, req3)
                        {:ok, req4, state}
                    _ ->
                        {:ok, req4} = :cowboy_req.reply(200, [{"Content-Type", <<"text/html">>}], res, req3)
                        {:ok, req4, state}
                end
        end
    end

    def terminate(_reason, _req, _state) do
        :ok
    end

    @doc """
        Handle response from controller
    """
    def handle_result(res, controller, views, static) do
        case res do
            {:render, data} -> 
                #
                # TODO remake with |>
                #
                filename = :erlang.binary_to_list(String.downcase(:erlang.list_to_binary(List.last(:string.tokens(atom_to_list(controller), '.')) ++ ".html")))

                #
                # Try to find this view file 
                #
                views_filenames = get_all_files(views)
                view_file = Enum.filter(views_filenames, fn(file) ->
                                              Path.basename(file) == filename
                                          end)

                case view_file do
                    [] ->
                        :io.format("[Error] No view file ~n")
                    _ ->
                        {:ok, d} = File.read(:lists.nth(1, view_file))
                        EEx.eval_string d, data
                end
            {:redirect, location} ->
                {:redirect, location}
            {:json, data} ->
                JSON.generate(data)
        end
    end

    @doc """
        Try to find static resource and send response
    """
    def try_to_find_static_resource(path, static, views, _root) do
        p = :erlang.binary_to_list(path)
        resource = List.last(:string.tokens(p, '/'))

        views_filenames = get_all_files(views)
        static_filenames = get_all_files(static)

        view_file = Enum.filter(views_filenames, fn(filename) ->
                                Path.basename(filename) == resource
                                end)

        static_file = Enum.filter(static_filenames, fn(filename) ->
                                  Path.basename(filename) == resource
                                  end)
           
        case view_file do
            [] ->
                case static_file do
                    [] ->
                        404
                    [resource_name] ->
                        resource_name
                end
            [resource_name] ->
                resource_name
        end
    end

    @doc """
        Recursively get all files from directory.
    """
    def get_all_files(dir) do
        find_files = fn(f, acc) -> [f | acc] end
        :filelib.fold_files(dir, ".*", true, find_files, [])
    end

end