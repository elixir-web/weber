defmodule Weber.Mixfile do
  use Mix.Project

  def project do
    [ app: :weber,
      version: "0.0.1",
      elixir: "~> 0.10.1-dev",
      deps: deps ]
  end

  def application do
    [
      description: "weber - is Elixir MVC web framework.",
      registered: [:weber],
      mod: { Weber, [] }
    ]
  end

  defp deps do
    [
      {:cowboy, "0.8.6", github: "extend/cowboy"},
      {:ecto, github: "elixir-lang/ecto"},
      {:pgsql, github: "ericmj/pgsql", branch: "elixir"},
      {:exjson, github: "guedes/exjson"},
      {:mimetypes, github: "spawngrid/mimetypes"}
    ]
  end
end