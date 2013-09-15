defmodule Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def url do
    "ecto://postgres:postgres@localhost/simpletodo"
  end
end

defmodule Todo do
  use Ecto.Model

  queryable "todo" do
    field :note, :string
  end

end

defmodule Simplemodel do
  import Ecto.Query

  def get_todo do
    query = from todo in Todo,
            limit: 10
            
    Repo.all(query)
  end

  def new(note) do
    Repo.create(Todo.new(note: note))
  end

end