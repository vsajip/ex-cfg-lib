#
# Copyright (C) 2021 Vinay Sajip <vinay_sajip@yahoo.co.uk>
#
# See LICENSE file for usage rights.
#
defmodule CFG.MixProject do
  use Mix.Project

  def project do
    [
      app: :cfg_lib,
      version: "0.1.0",
      elixir: "~> 1.1-dev",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: description(),
      source_url: "https://github.com/vsajip/ex-cfg-lib",
      homepage_url: "https://docs.red-dove.com/cfg/",
      package: package(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      applications: [:complex_num],
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:complex_num, "~> 1.0.0"},
      {:ex_doc, "~> 0.25", only: [:dev, :test], runtime: false}
    ]
  end

  defp description() do
    "A library for working with the CFG configuration format."
  end

  defp package() do
    [
      licenses: ["BSD-3-Clause"],
      links: %{
        "Source repository" => "https://github.com/vsajip/ex-cfg-lib",
        "CFG documentation" => "https://docs.red-dove.com/cfg/"
      }
    ]
  end
end
