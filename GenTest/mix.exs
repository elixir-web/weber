defmodule GenTest.Mixfile do
  use Mix.Project

  def project do
    [
      app: :GenTest,
      version: "0.0.1",
      deps: deps(Mix.env)
    ]
  end

  def application do
    [
      applications: [],
      mod: {GenTest, []}
    ]
  end

  defp deps(:prod) do
    [
      { :weber, github: "0xAX/weber" }
    ]
  end

  defp deps(:test) do
    deps(:prod) ++ [{ :hackney, github: "benoitc/hackney" }]
  end

  defp deps(_) do
    deps(:prod)
  end
end
