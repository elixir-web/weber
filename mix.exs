Code.ensure_loaded?(Hex) and Hex.start
 
defmodule Weber.Mixfile do
  use Mix.Project
 
  def project do
    [ app: :weber,
      version: "0.1.1",
      name: "Weber",
      elixir: "0.13.2-dev",
      deps: deps(Mix.env), 
      source_url: "https://github.com/0xAX/weber",
      homepage_url: "http://0xax.github.io/weber/index.html",
      description: "weber - is Elixir MVC web framework.",
      package: package
    ]
  end
 
  def application do
    [
      description: 'weber - is Elixir MVC web framework.',
      registered: [:weber],
      mod: { Weber, [] },
      lager: [
        {:handlers, [
          {:lager_console_backend, :info},
          {:lager_file_backend, [{:file, "error.log"}, {:level, :error}]},
          {:lager_file_backend, [{:file, "console.log"}, {:level, :info}]},
        ]}
      ]
    ]
  end
 
  defp deps(:prod) do
    [
      {:cowboy, github: "extend/cowboy" },
      {:exjson, github: "guedes/exjson"},
      {:plug, github: "elixir-lang/plug"},
      {:exlager, github: "khia/exlager"},
      {:weberContrib, github: "0xAX/weber-contrib"}
    ]
  end
 
  defp deps(:test) do
    deps(:prod) ++ [{ :hackney, github: "benoitc/hackney" }]
  end
  
  defp deps(_) do
    deps(:prod)
  end
 
  defp package do
    [
      files: ["lib", "test", "tmp", "templates", "examples", "README.md", "LICENSE", "ChangeLog.md", "mix.exs", "Makefile"],
      contributors: ["0xAX"],
      licenses: ["MIT"],
      links: [ 
              { "GitHub", "https://github.com/elixir-web/weber" },
              { "Docs", "http://0xax.github.io/weber/index.html" } ] 
      ]
  end
 
end