defmodule Handler.WeberReqHandler.Response do
    
  def handle_request({_type, status, data, headers}, req, state) do
    {:ok, req2} = :cowboy_req.reply(status, headers, data, req)
    {:ok, req2, state}
  end

  def handle_request({_type, status, data, headers}, req, state, {controller, action, conn}) do
    {:ok, req2} = :cowboy_req.reply(status, headers, data, req)
    case :lists.keyfind(:__after__, 2, controller.__info__(:functions)) do
      false -> :ok
      _ -> controller.__after__(action, conn)
    end 
    {:ok, req2, state}
  end

end