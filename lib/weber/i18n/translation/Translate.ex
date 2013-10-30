defmodule Weber.Translation.Translate do

  use GenServer.Behaviour

  defrecord Translate,
    t: nil

  def start_link(name, lang) do
    :gen_server.start_link({:local, name}, __MODULE__, [lang], [])
  end

  def init([lang]) do
    { :ok, Translate.new t: JSON.parse(lang)}
  end

  def handle_call({:translate, key}, _from, state) do
    {:reply, translation(key, state.t), state}
  end

  def translation(_key, []) do
    []
  end

  def translation(key, [{h, v} | t]) do
    case key == h do
      true -> v
      _ -> translation(key, t)
    end
  end

end