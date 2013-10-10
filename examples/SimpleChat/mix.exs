defmodule Simplechat.Mixfile do
  use Mix.Project

  def project do
    [ 
      app: :SimpleChat,
      version: "0.0.2",
      deps: deps
    ]
  end

  def application do
    [
      applications: [:weber],
      mod: {Simplechat, []}
    ]
  end

  defp deps do
    [ 
      { :weber, github: "0xAX/weber" } 
    ]
  end
end
