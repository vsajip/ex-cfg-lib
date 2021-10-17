# CFG.Config

An Elixir library for working with the CFG configuration format.

## Installation

The package can be installed by adding `cfg_lib` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:cfg_lib, "~> 0.1.0"}
  ]
end
```


## Usage

The CFG configuration format is a text format for configuration files which is similar to, and a superset of, the JSON format. It dates from before its first announcement in [2008](https://wiki.python.org/moin/HierConfig) and has the following aims:

* Allow a hierarchical configuration scheme with support for key-value mappings and lists.
* Support cross-references between one part of the configuration and another.
* Provide a string interpolation facility to easily build up configuration values from other configuration values.
* Provide the ability to compose configurations (using include and merge facilities).
* Provide the ability to access real application objects safely, where supported by the platform.
* Be completely declarative.

It overcomes a number of drawbacks of JSON when used as a configuration format:

* JSON is more verbose than necessary.
* JSON doesn’t allow comments.
* JSON doesn’t provide first-class support for dates and multi-line strings.
* JSON doesn’t allow trailing commas in lists and mappings.
* JSON doesn’t provide easy cross-referencing, interpolation, or composition.

A simple example
================

With the following configuration file, `test0.cfg`:
```text
a: 'Hello, '
b: 'world!'
c: {
  d: 'e'
}
'f.g': 'h'
christmas_morning: `2019-12-25 08:39:49`
home: `$HOME`
foo: `$FOO|bar`
```

You can load and query the above configuration using [iex](https://hexdocs.pm/iex/IEx.html):

Loading a configuration
-----------------------

The configuration above can be loaded as shown below. In the REPL shell:
```text
iex(1)> alias CFG.Config
CFG.Config
iex(2)> {:ok, cfg} = Config.from_file("test0.cfg")
{:ok, #PID<0.218.0>}
```

The successful call returns a `Config` which can be used to query the configuration.

Access elements with keys
-------------------------
Accessing elements of the configuration with a simple key is not much harder than using a map:
```text
iex(3)> Config.get(cfg, "a")
{:ok, "Hello, "}
iex(4)> Config.get(cfg, "b")
{:ok, "world!"}
```

Access elements with paths
--------------------------
As well as simple keys, elements can also be accessed using path strings:
```text
iex(5)> Config.get(cfg, "c.d")
{:ok, "e"}
```
Here, the desired value is obtained in a single step, by (under the hood) walking the path `c.d` – first getting the mapping at key `c`, and then the value at `d` in the resulting mapping.

Note that you can have simple keys which look like paths:
```text
iex(6)> Config.get(cfg, "f.g")
{:ok, "h"}
```
If a key is given that exists in the configuration, it is used as such, and if it is not present in the configuration, an attempt is made to interpret it as a path. Thus, `f.g` is present and accessed via key, whereas `c.d` is not an existing key, so is interpreted as a path.

Access to date/time objects
---------------------------
You can also get native Elixir date/time objects from a configuration, by using an ISO date/time pattern in a backtick-string:
```text
iex(7)> Config.get(cfg, "christmas_morning")
{:ok, ~U[2019-12-25 08:39:49.000000Z]}
```
Access to other Elixir/Erlang objects
---------------------------------------
Access to other Elixir/Erlang objects is also possible using the backtick-string syntax, provided that they are one of:
* Environment variables
* Public functions in public modules which take no arguments
```text
iex(8)> {:ok, dt} = Config.get(cfg, "now")
{:ok, ~U[2021-10-16 12:37:37.781391Z]}
iex(9)> DateTime.diff(DateTime.utc_now, dt)
6
 ```

Access to environment variables
-------------------------------
To access an environment variable, use a backtick-string of the form `$VARNAME`:
```text
iex(10)> elem(Config.get(cfg, "home"), 1) == System.get_env("HOME")
true
```
You can specify a default value to be used if an environment variable isn’t present using the `$VARNAME|default-value` form. Whatever string follows the pipe character (including the empty string) is returned if the VARNAME is not a variable in the environment.
```text
iex(11)> Config.get(cfg, "foo")
{:ok, "bar"}
```

For more information, see [the CFG documentation](https://docs.red-dove.com/cfg/index.html).
