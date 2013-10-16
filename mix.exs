defmodule Weber.Mixfile do
  use Mix.Project

  def project do
    [ app: :weber,
      version: "0.0.2",
      deps: deps(Mix.env) ]
  end

  def application do
    [
      description: "weber - is Elixir MVC web framework.",
      registered: [:weber],
      mod: { Weber, [] }
    ]
  end

  defp deps(:prod) do
    [
      {:cowboy, "0.8.6", github: "extend/cowboy"},
      {:ecto, github: "elixir-lang/ecto"},
      {:pgsql, github: "ericmj/pgsql", branch: "elixir"},
      {:exjson, github: "guedes/exjson"},
      {:mimetypes, github: "spawngrid/mimetypes", override: true}
    ]
  end

  defp deps(:test) do
    deps(:prod) ++ [{ :hackney, github: "benoitc/hackney" }]
  end
  
  defp deps(_) do
    deps(:prod)
  end
end