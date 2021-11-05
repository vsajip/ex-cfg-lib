#
# Copyright (C) 2021 Vinay Sajip <vinay_sajip@yahoo.co.uk>
#
# See LICENSE file for usage rights.
#
# defmodule CFGTest do
#   use ExUnit.Case
#   # doctest CFG
# end

defmodule Utils do
  @moduledoc false
  alias CFG.Location

  def data_path(s) do
    Path.join("resources", s)
  end

  def loc(l, c) do
    Location.new(l, c)
  end
end

defmodule LocationTest do
  @moduledoc false
  use ExUnit.Case
  alias CFG.Location, as: Location

  test "initialize" do
    assert Location.new() == %Location{line: 1, column: 1}
    assert Location.new(3, 4) == %Location{line: 3, column: 4}
  end

  test "advance col" do
    loc = Location.new(3, 4)
    next = Location.next_col(loc)
    assert next == %Location{line: 3, column: 5}
  end

  test "advance line" do
    loc = Location.new(3, 4)
    next = Location.next_line(loc)
    assert next == %Location{line: 4, column: 1}
  end
end

defmodule TokenizerTest do
  @moduledoc false
  use ExUnit.Case
  alias ComplexNum.Cartesian, as: Complex
  alias CFG.{Tokenizer, Token, Location, RecognizerError}
  require Logger
  import Utils

  # defp fragments do
  test "fragments" do
    cases = [
      ["", :EOF, "", nil, 1, 1, 1, 1],
      ["# a comment\n", :NEWLINE, "# a comment", nil, 1, 1, 2, 0],
      ["\r", :NEWLINE, "\n", nil, 1, 1, 2, 0],
      ["\n", :NEWLINE, "\n", nil, 1, 1, 2, 0],
      ["\t\r\n", :NEWLINE, "\n", nil, 1, 2, 2, 0],
      ["foo", :WORD, "foo", nil, 1, 1, 1, 3],
      ["foo:", :WORD, "foo", nil, 1, 1, 1, 3],
      # ["\\\nfoo", :WORD, "foo", nil, 2, 1, 2, 3],
      [" foo", :WORD, "foo", nil, 1, 2, 1, 4],
      ["`foo`", :BACKTICK, "`foo`", "foo", 1, 1, 1, 5],
      ["'foo'", :STRING, "'foo'", "foo", 1, 1, 1, 5],
      ["123", :INTEGER, "123", 123, 1, 1, 1, 3],
      ["123:", :INTEGER, "123", 123, 1, 1, 1, 3],
      ["1_2_3", :INTEGER, "1_2_3", 123, 1, 1, 1, 5],
      ["-123", :INTEGER, "-123", -123, 1, 1, 1, 4],
      ["'''bar'''", :STRING, "'''bar'''", "bar", 1, 1, 1, 9],
      ["\"\"\"bar\"\"\"", :STRING, "\"\"\"bar\"\"\"", "bar", 1, 1, 1, 9],
      [" 2.718281828 ", :FLOAT, "2.718281828", 2.718281828, 1, 2, 1, 12],
      [" 2.718_281_828 ", :FLOAT, "2.718_281_828", 2.718281828, 1, 2, 1, 14],
      ["-2.718281828 ", :FLOAT, "-2.718281828", -2.718281828, 1, 1, 1, 12],
      [".5", :FLOAT, ".5", 0.5, 1, 1, 1, 2],
      ["-.5", :FLOAT, "-.5", -0.5, 1, 1, 1, 3],
      ["1e8", :FLOAT, "1e8", 1.0e8, 1, 1, 1, 3],
      ["-1e8", :FLOAT, "-1e8", -1.0e8, 1, 1, 1, 4],
      ["1e-8", :FLOAT, "1e-8", 1.0e-8, 1, 1, 1, 4],
      ["1_0e1_0", :FLOAT, "1_0e1_0", 1.0e11, 1, 1, 1, 7],
      ["-1e-8", :FLOAT, "-1e-8", -1.0e-8, 1, 1, 1, 5],
      ["1j", :COMPLEX, "1j", Complex.new(0, 1), 1, 1, 1, 2],
      ["1j ", :COMPLEX, "1j", Complex.new(0, 1), 1, 1, 1, 2],
      ["-1j", :COMPLEX, "-1j", Complex.new(0, -1), 1, 1, 1, 3],
      ["-.5j", :COMPLEX, "-.5j", Complex.new(0, -0.5), 1, 1, 1, 4],
      ["-1e-8J", :COMPLEX, "-1e-8J", Complex.new(0, -1.0e-8), 1, 1, 1, 6],
      ["0b0001_0110_0111", :INTEGER, "0b0001_0110_0111", 0x167, 1, 1, 1, 16],
      ["0o123", :INTEGER, "0o123", 83, 1, 1, 1, 5],
      ["0x123aBc", :INTEGER, "0x123aBc", 0x123ABC, 1, 1, 1, 8],
      ["=x", :ASSIGN, "=", nil, 1, 1, 1, 1],
      ["==x", :EQ, "==", nil, 1, 1, 1, 2],
      [":x", :COLON, ":", nil, 1, 1, 1, 1],
      ["-x", :MINUS, "-", nil, 1, 1, 1, 1],
      ["+x", :PLUS, "+", nil, 1, 1, 1, 1],
      ["*x", :STAR, "*", nil, 1, 1, 1, 1],
      ["**x", :POWER, "**", nil, 1, 1, 1, 2],
      ["/x", :SLASH, "/", nil, 1, 1, 1, 1],
      ["//x", :SLASHSLASH, "//", nil, 1, 1, 1, 2],
      ["%x", :MODULO, "%", nil, 1, 1, 1, 1],
      [",x", :COMMA, ",", nil, 1, 1, 1, 1],
      ["{x", :LCURLY, "{", nil, 1, 1, 1, 1],
      ["}x", :RCURLY, "}", nil, 1, 1, 1, 1],
      ["[x", :LBRACK, "[", nil, 1, 1, 1, 1],
      ["]x", :RBRACK, "]", nil, 1, 1, 1, 1],
      ["(x", :LPAREN, "(", nil, 1, 1, 1, 1],
      [")x", :RPAREN, ")", nil, 1, 1, 1, 1],
      ["@x", :AT, "@", nil, 1, 1, 1, 1],
      ["$x", :DOLLAR, "$", nil, 1, 1, 1, 1],
      ["<x", :LT, "<", nil, 1, 1, 1, 1],
      ["<=x", :LE, "<=", nil, 1, 1, 1, 2],
      ["<<x", :LSHIFT, "<<", nil, 1, 1, 1, 2],
      ["<>x", :ALT_NEQ, "<>", nil, 1, 1, 1, 2],
      [">x", :GT, ">", nil, 1, 1, 1, 1],
      [">=x", :GE, ">=", nil, 1, 1, 1, 2],
      [">>x", :RSHIFT, ">>", nil, 1, 1, 1, 2],
      ["!x", :NOT, "!", nil, 1, 1, 1, 1],
      ["!=x", :NEQ, "!=", nil, 1, 1, 1, 2],
      ["~x", :BITNOT, "~", nil, 1, 1, 1, 1],
      ["&x", :BITAND, "&", nil, 1, 1, 1, 1],
      ["|x", :BITOR, "|", nil, 1, 1, 1, 1],
      ["^x", :BITXOR, "^", nil, 1, 1, 1, 1],
      ["true", :TRUE, "true", true, 1, 1, 1, 4],
      ["false", :FALSE, "false", false, 1, 1, 1, 5],
      ["null", :NONE, "null", nil, 1, 1, 1, 4],
      ["is", :IS, "is", nil, 1, 1, 1, 2],
      ["in", :IN, "in", nil, 1, 1, 1, 2],
      ["not", :NOT, "not", nil, 1, 1, 1, 3],
      ["and", :AND, "and", nil, 1, 1, 1, 3],
      ["or", :OR, "or", nil, 1, 1, 1, 2]
    ]

    Enum.each(cases, fn case ->
      [source, kind, text, value, sl, sc, el, ec] = case
      # Logger.debug("== Fragment /#{source}/:")
      tokenizer = Tokenizer.from_source(source)
      {:ok, t} = Tokenizer.get_token(tokenizer)
      assert !is_nil(t)
      assert kind == t.kind
      assert text == t.text

      if value != t.value || sl != t.start.line || sc != t.start.column ||
           el != t.end.line || ec != t.end.column do
        Logger.debug("#{source} -> #{t}")
      end

      assert value == t.value
      assert sl == t.start.line
      assert sc == t.start.column
      assert el == t.end.line
      assert ec == t.end.column
    end)
  end

  # defp bad_fragments do
  test "bad fragments" do
    cases = [
      ["`foo", 1, 4, :unterminated_backtick, "`foo"],
      ["`foo\n", 1, 5, :newlines_not_allowed, "`foo"],
      ["'foo", 1, 4, :unterminated_string, "'foo"],
      ["\"foo", 1, 4, :unterminated_string, "\"foo"],
      ["\"foo\n", 1, 5, :newlines_not_allowed, "\"foo"],
      ["1.2.3", 1, 4, :bad_number, "1.2."],
      ["1..2", 1, 3, :bad_number, "1.."],
      ["1__2", 1, 3, :bad_number, "1__"],
      ["1._2", 1, 3, :bad_number, "1._"],
      ["1e", 1, 2, :bad_number, "1e"],
      ["-1e--", 1, 5, :bad_number, "-1e--"],
      ["1e_", 1, 3, :bad_number, "1e_"],
      ["-1e-", 1, 4, :bad_number, "-1e-"],
      ["1je", 1, 3, :bad_number, "1je"],
      ["1_", 1, 2, :bad_number, "1_"],
      ["0b2", 1, 3, :bad_number, "0b2"],
      ["0o8", 1, 3, :bad_number, "0o8"],
      ["0xZ", 1, 3, :bad_number, "0xZ"],
      ["-0b2", 1, 4, :bad_number, "-0b2"],
      ["-0o8", 1, 4, :bad_number, "-0o8"],
      ["-0xZ", 1, 4, :bad_number, "-0xZ"],
      ["123A", 1, 4, :bad_number, "123A"],
      ["123_A", 1, 5, :bad_number, "123_A"],
      ["-1e-8_", 1, 6, :bad_number, "-1e-8_"],
      ["-1e-8Z", 1, 6, :bad_number, "-1e-8Z"],
      ["079", 1, 3, :bad_octal_constant, "079"],
      ["'\\xZZ'", 1, 1, :invalid_escape, "xZZ"],
      ["'\\u000Z'", 1, 1, :invalid_escape, "u000Z"],
      ["'\\U0000000Z'", 1, 1, :invalid_escape, "U0000000Z"],
      [" ;", 1, 2, :unexpected_char, ";"],
      [" \\ ", 1, 2, :unexpected_char, "\\"]
    ]

    Enum.each(cases, fn case ->
      [source, el, ec, reason, detail] = case
      tokenizer = Tokenizer.from_source(source)

      {:error, %RecognizerError{location: loc, detail: d, reason: r} = e} =
        Tokenizer.get_token(tokenizer)

      if el != loc.line || ec != loc.column || reason != r || detail != d do
        Logger.debug(RecognizerError.message(e))
      end

      assert el == loc.line
      assert ec == loc.column
      assert reason == r
      assert detail == d
      # IO.puts("source: #{source}, loc: #{loc}, message: #{msg}")
    end)
  end

  test "strings" do
    cases = [
      ["'\\n'", "\n", 1, 1, 1, 4],
      ["'\\a\\b\\f\\n\\r\\t\\v\\\\'", "\a\b\f\n\r\t\v\\", 1, 1, 1, 18],
      ["\"\"\"abc\ndef\n\"\"\"", "abc\ndef\n", 1, 1, 3, 3],
      ["'\\xfcber'", "über", 1, 1, 1, 9],
      ["'\\u03bb-calculus'", "λ-calculus", 1, 1, 1, 17],
      ["'\\U0001f602-tears-joy'", "😂-tears-joy", 1, 1, 1, 22],
      # empties
      ["''", "", 1, 1, 1, 2],
      ["\"\"", "", 1, 1, 1, 2],
      ["''''''", "", 1, 1, 1, 6],
      ["\"\"\"\"\"\"", "", 1, 1, 1, 6]
    ]

    Enum.each(cases, fn case ->
      [source, v, sl, sc, el, ec] = case
      # Logger.debug("== String /#{source}/:")
      tokenizer = Tokenizer.from_source(source)
      {:ok, t} = Tokenizer.get_token(tokenizer)
      assert !is_nil(t)
      assert t.kind == :STRING
      assert t.text == source
      assert t.value == v
      assert t.start.line == sl
      assert t.start.column == sc
      assert t.end.line == el
      assert t.end.column == ec
    end)
  end

  defp scan_pos(line) do
    Enum.map(Regex.scan(~r/\d+/, line), fn s ->
      {v, _} = Integer.parse(List.first(s))
      v
    end)
  end

  test "locations" do
    lines =
      File.stream!(data_path("pos.forms.cfg.txt"))
      # |> Stream.map(&String.trim_trailing/1)
      # |> Enum.to_list
      |> Enum.map(&scan_pos/1)

    tokenizer = Tokenizer.from_file(data_path("forms.cfg"))

    Enum.map(lines, fn line ->
      [sl, sc, el, ec] = line
      {:ok, t} = Tokenizer.get_token(tokenizer)

      if sl != t.start.line || sc != t.start.column || el != t.end.line || ec != t.end.column do
        Logger.error("#{t}")
      end

      assert sl == t.start.line
      assert sc == t.start.column
      assert el == t.end.line
      assert ec == t.end.column
    end)
  end

  def load_data(fname) do
    stream = File.stream!(data_path(fname))
    {:ok, pid} = Agent.start_link(fn -> %{key: nil, lines: [], result: %{}} end)

    _ =
      Enum.reduce_while(stream, pid, fn line, pid ->
        state = Agent.get(pid, fn state -> state end)
        m = Regex.run(~r/^-- ([A-Z]\d+) -+/, line)

        if m == nil do
          # There must be a key in place
          if state.key == nil do
            Agent.update(pid, fn state ->
              Map.put(state, :error, {:error, "No key for line: #{String.trim_trailing(line)}"})
            end)

            {:halt, pid}
          else
            Agent.update(pid, fn state -> %{state | lines: state.lines ++ [line]} end)
            {:cont, pid}
          end
        else
          if Enum.empty?(state.lines) do
            Agent.update(pid, fn state -> %{state | key: List.last(m)} end)
          else
            result = Map.put(state.result, state.key, Enum.join(state.lines))

            Agent.update(pid, fn state ->
              %{state | key: List.last(m), lines: [], result: result}
            end)
          end

          {:cont, pid}
        end
      end)

    state = Agent.get(pid, fn state -> state end)

    if length(state.lines) > 0 && !is_nil(state.key) do
      result = Map.put(state.result, state.key, Enum.join(state.lines))
      Agent.update(pid, fn state -> %{state | result: result} end)
    end

    result = Agent.get(pid, fn state -> Map.get(state, :error, {:ok, state.result}) end)
    Agent.stop(pid)
    result
  end

  test "data" do
    {:ok, cases} = load_data("testdata.txt")

    expected = %{
      "C01" => [
        %Token{
          kind: :NEWLINE,
          text: "# Basic mapping with a single key-value pair and comment",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'Hello, world!'",
          value: "Hello, world!",
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 25},
          end: %Location{line: 2, column: 25}
        }
      ],
      "C02" => [
        %Token{
          kind: :NEWLINE,
          text: "# Mapping with multiple key-value pairs",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'Hello, world!'",
          value: "Hello, world!",
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 25},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "ident",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 6},
          end: %Location{line: 3, column: 6}
        },
        %Token{
          kind: :INTEGER,
          text: "42",
          value: 42,
          start: %Location{line: 3, column: 8},
          end: %Location{line: 3, column: 9}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 3, column: 10},
          end: %Location{line: 3, column: 10}
        }
      ],
      "C03" => [
        %Token{
          kind: :NEWLINE,
          text: "# Mapping with interspersed comments",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'Hello, world!'",
          value: "Hello, world!",
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 25},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# A separating comment",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "ident",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 6},
          end: %Location{line: 4, column: 6}
        },
        %Token{
          kind: :INTEGER,
          text: "43",
          value: 43,
          start: %Location{line: 4, column: 8},
          end: %Location{line: 4, column: 9}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 4, column: 10},
          end: %Location{line: 4, column: 10}
        }
      ],
      "C04" => [
        %Token{
          kind: :NEWLINE,
          text: "# With included trailing commas for both list and mapping",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "numbers",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 10}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 2, column: 11},
          end: %Location{line: 2, column: 11}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 12},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :INTEGER,
          text: "0x012",
          value: 18,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 18}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 19},
          end: %Location{line: 2, column: 19}
        },
        %Token{
          kind: :INTEGER,
          text: "013",
          value: 11,
          start: %Location{line: 2, column: 21},
          end: %Location{line: 2, column: 23}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 24},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :INTEGER,
          text: "1014",
          value: 1014,
          start: %Location{line: 2, column: 26},
          end: %Location{line: 2, column: 29}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 30},
          end: %Location{line: 2, column: 30}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 2, column: 32},
          end: %Location{line: 2, column: 32}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 33},
          end: %Location{line: 2, column: 33}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 34},
          end: %Location{line: 2, column: 34}
        }
      ],
      "C05" => [
        %Token{
          kind: :WORD,
          text: "complex",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :COMPLEX,
          text: "0j",
          value: Complex.new(0, 0),
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 12}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :COMPLEX,
          text: "1j",
          value: Complex.new(0, 1),
          start: %Location{line: 1, column: 15},
          end: %Location{line: 1, column: 16}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 17},
          end: %Location{line: 1, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 18},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :COMPLEX,
          text: ".4j",
          value: Complex.new(0, 0.4),
          start: %Location{line: 2, column: 11},
          end: %Location{line: 2, column: 13}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 14}
        },
        %Token{
          kind: :COMPLEX,
          text: "0.7j",
          value: Complex.new(0, 0.7),
          start: %Location{line: 2, column: 16},
          end: %Location{line: 2, column: 19}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 2, column: 20},
          end: %Location{line: 2, column: 20}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 21},
          end: %Location{line: 2, column: 21}
        }
      ],
      "C06" => [
        %Token{
          kind: :WORD,
          text: "nested",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 7},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 1, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "c",
          value: nil,
          start: %Location{line: 1, column: 16},
          end: %Location{line: 1, column: 16}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 18},
          end: %Location{line: 1, column: 18}
        },
        %Token{
          kind: :WORD,
          text: "d",
          value: nil,
          start: %Location{line: 1, column: 20},
          end: %Location{line: 1, column: 20}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 21},
          end: %Location{line: 1, column: 21}
        },
        %Token{
          kind: :STRING,
          text: "'e f'",
          value: "e f",
          start: %Location{line: 1, column: 23},
          end: %Location{line: 1, column: 27}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 28},
          end: %Location{line: 1, column: 28}
        },
        %Token{
          kind: :STRING,
          text: "'g'",
          value: "g",
          start: %Location{line: 1, column: 30},
          end: %Location{line: 1, column: 32}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 1, column: 33},
          end: %Location{line: 1, column: 33}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 1, column: 34},
          end: %Location{line: 1, column: 34}
        }
      ],
      "C07" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 4},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :INTEGER,
          text: "1",
          value: 1,
          start: %Location{line: 1, column: 7},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :INTEGER,
          text: "2",
          value: 2,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :INTEGER,
          text: "3",
          value: 3,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 1, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 15},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "bar",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 4},
          end: %Location{line: 4, column: 4}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 4, column: 6},
          end: %Location{line: 4, column: 6}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 7},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :INTEGER,
          text: "4",
          value: 4,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 5}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 5, column: 7},
          end: %Location{line: 5, column: 7}
        },
        %Token{
          kind: :WORD,
          text: "x",
          value: nil,
          start: %Location{line: 5, column: 9},
          end: %Location{line: 5, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "# random comment",
          value: nil,
          start: %Location{line: 5, column: 13},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 7, column: 1},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :INTEGER,
          text: "5",
          value: 5,
          start: %Location{line: 8, column: 5},
          end: %Location{line: 8, column: 5}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 8, column: 6},
          end: %Location{line: 8, column: 6}
        },
        %Token{
          kind: :NEWLINE,
          text: "# another one",
          value: nil,
          start: %Location{line: 8, column: 13},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 1},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :INTEGER,
          text: "6",
          value: 6,
          start: %Location{line: 10, column: 5},
          end: %Location{line: 10, column: 5}
        },
        %Token{
          kind: :NEWLINE,
          text: "# and one more",
          value: nil,
          start: %Location{line: 10, column: 13},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 11, column: 1},
          end: %Location{line: 11, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 11, column: 2},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "baz",
          value: nil,
          start: %Location{line: 12, column: 1},
          end: %Location{line: 12, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 12, column: 4},
          end: %Location{line: 12, column: 4}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 12, column: 6},
          end: %Location{line: 12, column: 6}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 7},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 13, column: 5},
          end: %Location{line: 13, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 13, column: 8},
          end: %Location{line: 13, column: 8}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 13, column: 10},
          end: %Location{line: 13, column: 10}
        },
        %Token{
          kind: :INTEGER,
          text: "1",
          value: 1,
          start: %Location{line: 13, column: 11},
          end: %Location{line: 13, column: 11}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 13, column: 12},
          end: %Location{line: 13, column: 12}
        },
        %Token{
          kind: :INTEGER,
          text: "2",
          value: 2,
          start: %Location{line: 13, column: 14},
          end: %Location{line: 13, column: 14}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 13, column: 15},
          end: %Location{line: 13, column: 15}
        },
        %Token{
          kind: :INTEGER,
          text: "3",
          value: 3,
          start: %Location{line: 13, column: 17},
          end: %Location{line: 13, column: 17}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 13, column: 18},
          end: %Location{line: 13, column: 18}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 13, column: 19},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 1},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "bar",
          value: nil,
          start: %Location{line: 15, column: 5},
          end: %Location{line: 15, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 15, column: 8},
          end: %Location{line: 15, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'baz'",
          value: "baz",
          start: %Location{line: 15, column: 10},
          end: %Location{line: 15, column: 14}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 15, column: 16},
          end: %Location{line: 15, column: 16}
        },
        %Token{
          kind: :INTEGER,
          text: "3",
          value: 3,
          start: %Location{line: 15, column: 18},
          end: %Location{line: 15, column: 18}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 15, column: 19},
          end: %Location{line: 16, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 16, column: 1},
          end: %Location{line: 16, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 16, column: 2},
          end: %Location{line: 16, column: 2}
        }
      ],
      "C08" => [
        %Token{
          kind: :WORD,
          text: "total_period",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 12}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 1, column: 14}
        },
        %Token{
          kind: :INTEGER,
          text: "100",
          value: 100,
          start: %Location{line: 1, column: 16},
          end: %Location{line: 1, column: 18}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 19},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "header_time",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 12},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :FLOAT,
          text: "0.3",
          value: 0.3,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 16}
        },
        %Token{
          kind: :STAR,
          text: "*",
          value: nil,
          start: %Location{line: 2, column: 18},
          end: %Location{line: 2, column: 18}
        },
        %Token{
          kind: :WORD,
          text: "total_period",
          value: nil,
          start: %Location{line: 2, column: 20},
          end: %Location{line: 2, column: 31}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 32},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "steady_time",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 12},
          end: %Location{line: 3, column: 12}
        },
        %Token{
          kind: :FLOAT,
          text: "0.5",
          value: 0.5,
          start: %Location{line: 3, column: 14},
          end: %Location{line: 3, column: 16}
        },
        %Token{
          kind: :STAR,
          text: "*",
          value: nil,
          start: %Location{line: 3, column: 18},
          end: %Location{line: 3, column: 18}
        },
        %Token{
          kind: :WORD,
          text: "total_period",
          value: nil,
          start: %Location{line: 3, column: 20},
          end: %Location{line: 3, column: 31}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 32},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "trailer_time",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 12}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 13},
          end: %Location{line: 4, column: 13}
        },
        %Token{
          kind: :FLOAT,
          text: "0.2",
          value: 0.2,
          start: %Location{line: 4, column: 15},
          end: %Location{line: 4, column: 17}
        },
        %Token{
          kind: :STAR,
          text: "*",
          value: nil,
          start: %Location{line: 4, column: 19},
          end: %Location{line: 4, column: 19}
        },
        %Token{
          kind: :WORD,
          text: "total_period",
          value: nil,
          start: %Location{line: 4, column: 21},
          end: %Location{line: 4, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 33},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "base_prefix",
          value: nil,
          start: %Location{line: 5, column: 1},
          end: %Location{line: 5, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'/my/app/'",
          value: "/my/app/",
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 23}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 24},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "log_file",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 9},
          end: %Location{line: 6, column: 9}
        },
        %Token{
          kind: :WORD,
          text: "base_prefix",
          value: nil,
          start: %Location{line: 6, column: 11},
          end: %Location{line: 6, column: 21}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 6, column: 23},
          end: %Location{line: 6, column: 23}
        },
        %Token{
          kind: :STRING,
          text: "'test.log'",
          value: "test.log",
          start: %Location{line: 6, column: 25},
          end: %Location{line: 6, column: 34}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 6, column: 35},
          end: %Location{line: 6, column: 35}
        }
      ],
      "C09" => [
        %Token{
          kind: :NEWLINE,
          text: "# The message to print (this is a comment)",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'Hello, world!'",
          value: "Hello, world!",
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 25},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 7},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 3, column: 9},
          end: %Location{line: 3, column: 20}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 3, column: 21},
          end: %Location{line: 3, column: 21}
        }
      ],
      "C10" => [
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 3, column: 3},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 3, column: 5},
          end: %Location{line: 3, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 12},
          end: %Location{line: 3, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 3, column: 14},
          end: %Location{line: 3, column: 25}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 26},
          end: %Location{line: 3, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 3, column: 28},
          end: %Location{line: 3, column: 34}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 35},
          end: %Location{line: 3, column: 35}
        },
        %Token{
          kind: :STRING,
          text: "'Welcome'",
          value: "Welcome",
          start: %Location{line: 3, column: 37},
          end: %Location{line: 3, column: 45}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 3, column: 47},
          end: %Location{line: 3, column: 47}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 48},
          end: %Location{line: 3, column: 48}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 49},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 4, column: 3},
          end: %Location{line: 4, column: 3}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 12},
          end: %Location{line: 4, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdout`",
          value: "sys.stdout",
          start: %Location{line: 4, column: 14},
          end: %Location{line: 4, column: 25}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 4, column: 26},
          end: %Location{line: 4, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 4, column: 28},
          end: %Location{line: 4, column: 34}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 35},
          end: %Location{line: 4, column: 35}
        },
        %Token{
          kind: :STRING,
          text: "'Welkom'",
          value: "Welkom",
          start: %Location{line: 4, column: 37},
          end: %Location{line: 4, column: 44}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 4, column: 46},
          end: %Location{line: 4, column: 46}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 4, column: 47},
          end: %Location{line: 4, column: 47}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 48},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 5, column: 3},
          end: %Location{line: 5, column: 3}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 25}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 5, column: 26},
          end: %Location{line: 5, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 5, column: 28},
          end: %Location{line: 5, column: 34}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 35},
          end: %Location{line: 5, column: 35}
        },
        %Token{
          kind: :STRING,
          text: "'Bienvenue'",
          value: "Bienvenue",
          start: %Location{line: 5, column: 37},
          end: %Location{line: 5, column: 47}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 5, column: 49},
          end: %Location{line: 5, column: 49}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 5, column: 50},
          end: %Location{line: 5, column: 50}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 51},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 6, column: 2},
          end: %Location{line: 6, column: 2}
        }
      ],
      "C11" => [
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 3, column: 3},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 4},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 12},
          end: %Location{line: 4, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 4, column: 14},
          end: %Location{line: 4, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 26},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :WORD,
          text: "Welcome",
          value: nil,
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 20}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 21},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 6, column: 5},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 9},
          end: %Location{line: 6, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Harry'",
          value: "Harry",
          start: %Location{line: 6, column: 11},
          end: %Location{line: 6, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 18},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 7, column: 3},
          end: %Location{line: 7, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 7, column: 4},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 8, column: 3},
          end: %Location{line: 8, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 8, column: 4},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 9, column: 5},
          end: %Location{line: 9, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 9, column: 12},
          end: %Location{line: 9, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdout`",
          value: "sys.stdout",
          start: %Location{line: 9, column: 14},
          end: %Location{line: 9, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 26},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 10, column: 5},
          end: %Location{line: 10, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 10, column: 12},
          end: %Location{line: 10, column: 12}
        },
        %Token{
          kind: :WORD,
          text: "Welkom",
          value: nil,
          start: %Location{line: 10, column: 14},
          end: %Location{line: 10, column: 19}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 10, column: 20},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 11, column: 5},
          end: %Location{line: 11, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 11, column: 9},
          end: %Location{line: 11, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Ruud'",
          value: "Ruud",
          start: %Location{line: 11, column: 11},
          end: %Location{line: 11, column: 16}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 11, column: 17},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 12, column: 3},
          end: %Location{line: 12, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 4},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 13, column: 3},
          end: %Location{line: 13, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 13, column: 4},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 14, column: 5},
          end: %Location{line: 14, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 14, column: 13},
          end: %Location{line: 14, column: 13}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 14, column: 15},
          end: %Location{line: 14, column: 26}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 27},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 15, column: 5},
          end: %Location{line: 15, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 15, column: 13},
          end: %Location{line: 15, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "Bienvenue",
          value: nil,
          start: %Location{line: 15, column: 15},
          end: %Location{line: 15, column: 23}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 15, column: 24},
          end: %Location{line: 16, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 16, column: 5},
          end: %Location{line: 16, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 16, column: 13},
          end: %Location{line: 16, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "Yves",
          value: nil,
          start: %Location{line: 16, column: 15},
          end: %Location{line: 16, column: 18}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 16, column: 19},
          end: %Location{line: 17, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 17, column: 3},
          end: %Location{line: 17, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 17, column: 4},
          end: %Location{line: 18, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 18, column: 1},
          end: %Location{line: 18, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 18, column: 2},
          end: %Location{line: 18, column: 2}
        }
      ],
      "C12" => [
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 3, column: 3},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 4},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 12},
          end: %Location{line: 4, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 4, column: 14},
          end: %Location{line: 4, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 26},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'Welcome'",
          value: "Welcome",
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 22}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 23},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 6, column: 5},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 9},
          end: %Location{line: 6, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Harry'",
          value: "Harry",
          start: %Location{line: 6, column: 11},
          end: %Location{line: 6, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 18},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 7, column: 3},
          end: %Location{line: 7, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 7, column: 4},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 8, column: 3},
          end: %Location{line: 8, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 8, column: 4},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 9, column: 5},
          end: %Location{line: 9, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 9, column: 12},
          end: %Location{line: 9, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdout`",
          value: "sys.stdout",
          start: %Location{line: 9, column: 14},
          end: %Location{line: 9, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 26},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 10, column: 5},
          end: %Location{line: 10, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 10, column: 12},
          end: %Location{line: 10, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'Welkom'",
          value: "Welkom",
          start: %Location{line: 10, column: 14},
          end: %Location{line: 10, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 10, column: 22},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 11, column: 5},
          end: %Location{line: 11, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 11, column: 9},
          end: %Location{line: 11, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Ruud'",
          value: "Ruud",
          start: %Location{line: 11, column: 11},
          end: %Location{line: 11, column: 16}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 11, column: 17},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 12, column: 3},
          end: %Location{line: 12, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 4},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 13, column: 3},
          end: %Location{line: 13, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 13, column: 4},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 14, column: 5},
          end: %Location{line: 14, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 14, column: 12},
          end: %Location{line: 14, column: 12}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 14, column: 14},
          end: %Location{line: 14, column: 14}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 15},
          end: %Location{line: 14, column: 15}
        },
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 14, column: 16},
          end: %Location{line: 14, column: 23}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 14, column: 24},
          end: %Location{line: 14, column: 24}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 14, column: 25},
          end: %Location{line: 14, column: 25}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 14, column: 26},
          end: %Location{line: 14, column: 26}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 14, column: 27},
          end: %Location{line: 14, column: 27}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 14, column: 28},
          end: %Location{line: 14, column: 33}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 14, column: 34},
          end: %Location{line: 14, column: 34}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 35},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 15, column: 5},
          end: %Location{line: 15, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 15, column: 12},
          end: %Location{line: 15, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'Bienvenue'",
          value: "Bienvenue",
          start: %Location{line: 15, column: 14},
          end: %Location{line: 15, column: 24}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 15, column: 25},
          end: %Location{line: 16, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 16, column: 5},
          end: %Location{line: 16, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 16, column: 9},
          end: %Location{line: 16, column: 9}
        },
        %Token{
          kind: :WORD,
          text: "Yves",
          value: nil,
          start: %Location{line: 16, column: 11},
          end: %Location{line: 16, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 16, column: 15},
          end: %Location{line: 17, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 17, column: 3},
          end: %Location{line: 17, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 17, column: 4},
          end: %Location{line: 18, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 18, column: 1},
          end: %Location{line: 18, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 18, column: 2},
          end: %Location{line: 18, column: 2}
        }
      ],
      "C13" => [
        %Token{
          kind: :WORD,
          text: "logging",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :AT,
          text: "@",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :STRING,
          text: "\"logging.cfg\"",
          value: "logging.cfg",
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 23}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 24},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "test",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 4}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 5},
          end: %Location{line: 2, column: 5}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 2, column: 7},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :WORD,
          text: "logging",
          value: nil,
          start: %Location{line: 2, column: 9},
          end: %Location{line: 2, column: 15}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 2, column: 16},
          end: %Location{line: 2, column: 16}
        },
        %Token{
          kind: :WORD,
          text: "handler",
          value: nil,
          start: %Location{line: 2, column: 17},
          end: %Location{line: 2, column: 23}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 2, column: 24},
          end: %Location{line: 2, column: 24}
        },
        %Token{
          kind: :WORD,
          text: "email",
          value: nil,
          start: %Location{line: 2, column: 25},
          end: %Location{line: 2, column: 29}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 2, column: 30},
          end: %Location{line: 2, column: 30}
        },
        %Token{
          kind: :WORD,
          text: "from",
          value: nil,
          start: %Location{line: 2, column: 31},
          end: %Location{line: 2, column: 34}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 2, column: 35},
          end: %Location{line: 2, column: 35}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 36},
          end: %Location{line: 2, column: 36}
        }
      ],
      "C14" => [
        %Token{
          kind: :NEWLINE,
          text: "# root logger configuration",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "root",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 4}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 5},
          end: %Location{line: 2, column: 5}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 6},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 2},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "level",
          value: nil,
          start: %Location{line: 4, column: 3},
          end: %Location{line: 4, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 13},
          end: %Location{line: 4, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "DEBUG",
          value: nil,
          start: %Location{line: 4, column: 15},
          end: %Location{line: 4, column: 19}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 20},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 5, column: 3},
          end: %Location{line: 5, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 13},
          end: %Location{line: 5, column: 13}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 5, column: 15},
          end: %Location{line: 5, column: 15}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 5, column: 16},
          end: %Location{line: 5, column: 16}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 5, column: 17},
          end: %Location{line: 5, column: 17}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 5, column: 18},
          end: %Location{line: 5, column: 25}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 5, column: 26},
          end: %Location{line: 5, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "console",
          value: nil,
          start: %Location{line: 5, column: 27},
          end: %Location{line: 5, column: 33}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 5, column: 34},
          end: %Location{line: 5, column: 34}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 5, column: 35},
          end: %Location{line: 5, column: 35}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 5, column: 37},
          end: %Location{line: 5, column: 37}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 5, column: 38},
          end: %Location{line: 5, column: 38}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 5, column: 39},
          end: %Location{line: 5, column: 46}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 5, column: 47},
          end: %Location{line: 5, column: 47}
        },
        %Token{
          kind: :WORD,
          text: "file",
          value: nil,
          start: %Location{line: 5, column: 48},
          end: %Location{line: 5, column: 51}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 5, column: 52},
          end: %Location{line: 5, column: 52}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 5, column: 53},
          end: %Location{line: 5, column: 53}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 5, column: 55},
          end: %Location{line: 5, column: 55}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 5, column: 56},
          end: %Location{line: 5, column: 56}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 5, column: 57},
          end: %Location{line: 5, column: 64}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 5, column: 65},
          end: %Location{line: 5, column: 65}
        },
        %Token{
          kind: :WORD,
          text: "email",
          value: nil,
          start: %Location{line: 5, column: 66},
          end: %Location{line: 5, column: 70}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 5, column: 71},
          end: %Location{line: 5, column: 71}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 5, column: 72},
          end: %Location{line: 5, column: 72}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 73},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 2},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# logging handlers",
          value: nil,
          start: %Location{line: 7, column: 1},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 8, column: 1},
          end: %Location{line: 8, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 8, column: 9},
          end: %Location{line: 8, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 8, column: 10},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 9, column: 1},
          end: %Location{line: 9, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 2},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "console",
          value: nil,
          start: %Location{line: 10, column: 3},
          end: %Location{line: 10, column: 9}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 10, column: 10},
          end: %Location{line: 10, column: 10}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 10, column: 13},
          end: %Location{line: 10, column: 13}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 10, column: 14},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# the class to instantiate",
          value: nil,
          start: %Location{line: 11, column: 15},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "StreamHandler",
          value: nil,
          start: %Location{line: 12, column: 15},
          end: %Location{line: 12, column: 27}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 12, column: 28},
          end: %Location{line: 12, column: 28}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 29},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# how to configure the instance",
          value: nil,
          start: %Location{line: 13, column: 15},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 15},
          end: %Location{line: 14, column: 15}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 16},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "level",
          value: nil,
          start: %Location{line: 15, column: 17},
          end: %Location{line: 15, column: 21}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 15, column: 23},
          end: %Location{line: 15, column: 23}
        },
        %Token{
          kind: :WORD,
          text: "WARNING",
          value: nil,
          start: %Location{line: 15, column: 25},
          end: %Location{line: 15, column: 31}
        },
        %Token{
          kind: :NEWLINE,
          text: "# the logger level",
          value: nil,
          start: %Location{line: 15, column: 45},
          end: %Location{line: 16, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 16, column: 17},
          end: %Location{line: 16, column: 22}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 16, column: 25},
          end: %Location{line: 16, column: 25}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 16, column: 27},
          end: %Location{line: 16, column: 38}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 16, column: 40},
          end: %Location{line: 16, column: 40}
        },
        %Token{
          kind: :NEWLINE,
          text: "# the stream to use",
          value: nil,
          start: %Location{line: 16, column: 45},
          end: %Location{line: 17, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 17, column: 13},
          end: %Location{line: 17, column: 13}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 17, column: 14},
          end: %Location{line: 18, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "file",
          value: nil,
          start: %Location{line: 18, column: 3},
          end: %Location{line: 18, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 18, column: 7},
          end: %Location{line: 18, column: 7}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 18, column: 13},
          end: %Location{line: 18, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "FileHandler",
          value: nil,
          start: %Location{line: 18, column: 15},
          end: %Location{line: 18, column: 25}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 18, column: 26},
          end: %Location{line: 18, column: 26}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 18, column: 28},
          end: %Location{line: 18, column: 28}
        },
        %Token{
          kind: :WORD,
          text: "filename",
          value: nil,
          start: %Location{line: 18, column: 30},
          end: %Location{line: 18, column: 37}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 18, column: 38},
          end: %Location{line: 18, column: 38}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 18, column: 40},
          end: %Location{line: 18, column: 40}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 18, column: 41},
          end: %Location{line: 18, column: 41}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 18, column: 42},
          end: %Location{line: 18, column: 44}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 18, column: 45},
          end: %Location{line: 18, column: 45}
        },
        %Token{
          kind: :WORD,
          text: "base",
          value: nil,
          start: %Location{line: 18, column: 46},
          end: %Location{line: 18, column: 49}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 18, column: 50},
          end: %Location{line: 18, column: 50}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 18, column: 52},
          end: %Location{line: 18, column: 52}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 18, column: 54},
          end: %Location{line: 18, column: 54}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 18, column: 55},
          end: %Location{line: 18, column: 55}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 18, column: 56},
          end: %Location{line: 18, column: 58}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 18, column: 59},
          end: %Location{line: 18, column: 59}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 18, column: 60},
          end: %Location{line: 18, column: 63}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 18, column: 64},
          end: %Location{line: 18, column: 64}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 18, column: 66},
          end: %Location{line: 18, column: 66}
        },
        %Token{
          kind: :STRING,
          text: "'.log'",
          value: ".log",
          start: %Location{line: 18, column: 68},
          end: %Location{line: 18, column: 73}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 18, column: 74},
          end: %Location{line: 18, column: 74}
        },
        %Token{
          kind: :WORD,
          text: "mode",
          value: nil,
          start: %Location{line: 18, column: 76},
          end: %Location{line: 18, column: 79}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 18, column: 81},
          end: %Location{line: 18, column: 81}
        },
        %Token{
          kind: :STRING,
          text: "'a'",
          value: "a",
          start: %Location{line: 18, column: 83},
          end: %Location{line: 18, column: 85}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 18, column: 87},
          end: %Location{line: 18, column: 87}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 18, column: 89},
          end: %Location{line: 18, column: 89}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 18, column: 90},
          end: %Location{line: 19, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "socket",
          value: nil,
          start: %Location{line: 19, column: 3},
          end: %Location{line: 19, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 19, column: 9},
          end: %Location{line: 19, column: 9}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 19, column: 13},
          end: %Location{line: 19, column: 13}
        },
        %Token{
          kind: :BACKTICK,
          text: "`handlers.SocketHandler`",
          value: "handlers.SocketHandler",
          start: %Location{line: 19, column: 15},
          end: %Location{line: 19, column: 38}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 19, column: 39},
          end: %Location{line: 19, column: 39}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 19, column: 41},
          end: %Location{line: 19, column: 41}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 19, column: 42},
          end: %Location{line: 20, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "host",
          value: nil,
          start: %Location{line: 20, column: 19},
          end: %Location{line: 20, column: 22}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 20, column: 23},
          end: %Location{line: 20, column: 23}
        },
        %Token{
          kind: :WORD,
          text: "localhost",
          value: nil,
          start: %Location{line: 20, column: 25},
          end: %Location{line: 20, column: 33}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 20, column: 34},
          end: %Location{line: 20, column: 34}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 20, column: 35},
          end: %Location{line: 21, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# use this port for now",
          value: nil,
          start: %Location{line: 21, column: 19},
          end: %Location{line: 22, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "port",
          value: nil,
          start: %Location{line: 22, column: 19},
          end: %Location{line: 22, column: 22}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 22, column: 23},
          end: %Location{line: 22, column: 23}
        },
        %Token{
          kind: :BACKTICK,
          text: "`handlers.DEFAULT_TCP_LOGGING_PORT`",
          value: "handlers.DEFAULT_TCP_LOGGING_PORT",
          start: %Location{line: 22, column: 25},
          end: %Location{line: 22, column: 59}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 22, column: 60},
          end: %Location{line: 22, column: 60}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 22, column: 62},
          end: %Location{line: 22, column: 62}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 22, column: 63},
          end: %Location{line: 23, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "nt_eventlog",
          value: nil,
          start: %Location{line: 23, column: 3},
          end: %Location{line: 23, column: 13}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 23, column: 14},
          end: %Location{line: 23, column: 14}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 23, column: 16},
          end: %Location{line: 23, column: 16}
        },
        %Token{
          kind: :BACKTICK,
          text: "`handlers.NTEventLogHandler`",
          value: "handlers.NTEventLogHandler",
          start: %Location{line: 23, column: 17},
          end: %Location{line: 23, column: 44}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 23, column: 45},
          end: %Location{line: 23, column: 45}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 23, column: 47},
          end: %Location{line: 23, column: 47}
        },
        %Token{
          kind: :WORD,
          text: "appname",
          value: nil,
          start: %Location{line: 23, column: 49},
          end: %Location{line: 23, column: 55}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 23, column: 56},
          end: %Location{line: 23, column: 56}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 23, column: 58},
          end: %Location{line: 23, column: 58}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 23, column: 59},
          end: %Location{line: 23, column: 59}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 23, column: 60},
          end: %Location{line: 23, column: 62}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 23, column: 63},
          end: %Location{line: 23, column: 63}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 23, column: 64},
          end: %Location{line: 23, column: 67}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 23, column: 68},
          end: %Location{line: 23, column: 68}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 23, column: 69},
          end: %Location{line: 23, column: 69}
        },
        %Token{
          kind: :WORD,
          text: "logtype",
          value: nil,
          start: %Location{line: 23, column: 71},
          end: %Location{line: 23, column: 77}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 23, column: 79},
          end: %Location{line: 23, column: 79}
        },
        %Token{
          kind: :WORD,
          text: "Application",
          value: nil,
          start: %Location{line: 23, column: 81},
          end: %Location{line: 23, column: 91}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 23, column: 93},
          end: %Location{line: 23, column: 93}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 23, column: 95},
          end: %Location{line: 23, column: 95}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 23, column: 96},
          end: %Location{line: 24, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "email",
          value: nil,
          start: %Location{line: 24, column: 3},
          end: %Location{line: 24, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 24, column: 8},
          end: %Location{line: 24, column: 8}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 24, column: 13},
          end: %Location{line: 24, column: 13}
        },
        %Token{
          kind: :BACKTICK,
          text: "`handlers.SMTPHandler`",
          value: "handlers.SMTPHandler",
          start: %Location{line: 24, column: 15},
          end: %Location{line: 24, column: 36}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 24, column: 37},
          end: %Location{line: 24, column: 37}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 24, column: 38},
          end: %Location{line: 25, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 25, column: 15},
          end: %Location{line: 25, column: 15}
        },
        %Token{
          kind: :WORD,
          text: "level",
          value: nil,
          start: %Location{line: 25, column: 17},
          end: %Location{line: 25, column: 21}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 25, column: 22},
          end: %Location{line: 25, column: 22}
        },
        %Token{
          kind: :WORD,
          text: "CRITICAL",
          value: nil,
          start: %Location{line: 25, column: 24},
          end: %Location{line: 25, column: 31}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 25, column: 32},
          end: %Location{line: 25, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 25, column: 33},
          end: %Location{line: 26, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "host",
          value: nil,
          start: %Location{line: 26, column: 17},
          end: %Location{line: 26, column: 20}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 26, column: 21},
          end: %Location{line: 26, column: 21}
        },
        %Token{
          kind: :WORD,
          text: "localhost",
          value: nil,
          start: %Location{line: 26, column: 23},
          end: %Location{line: 26, column: 31}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 26, column: 32},
          end: %Location{line: 26, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 26, column: 33},
          end: %Location{line: 27, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "port",
          value: nil,
          start: %Location{line: 27, column: 17},
          end: %Location{line: 27, column: 20}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 27, column: 21},
          end: %Location{line: 27, column: 21}
        },
        %Token{
          kind: :INTEGER,
          text: "25",
          value: 25,
          start: %Location{line: 27, column: 23},
          end: %Location{line: 27, column: 24}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 27, column: 25},
          end: %Location{line: 27, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 27, column: 26},
          end: %Location{line: 28, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "from",
          value: nil,
          start: %Location{line: 28, column: 17},
          end: %Location{line: 28, column: 20}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 28, column: 21},
          end: %Location{line: 28, column: 21}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 28, column: 23},
          end: %Location{line: 28, column: 23}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 28, column: 24},
          end: %Location{line: 28, column: 24}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 28, column: 25},
          end: %Location{line: 28, column: 27}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 28, column: 28},
          end: %Location{line: 28, column: 28}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 28, column: 29},
          end: %Location{line: 28, column: 32}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 28, column: 33},
          end: %Location{line: 28, column: 33}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 28, column: 35},
          end: %Location{line: 28, column: 35}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 28, column: 37},
          end: %Location{line: 28, column: 37}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 28, column: 38},
          end: %Location{line: 28, column: 38}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 28, column: 39},
          end: %Location{line: 28, column: 41}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 28, column: 42},
          end: %Location{line: 28, column: 42}
        },
        %Token{
          kind: :WORD,
          text: "mail_domain",
          value: nil,
          start: %Location{line: 28, column: 43},
          end: %Location{line: 28, column: 53}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 28, column: 54},
          end: %Location{line: 28, column: 54}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 28, column: 55},
          end: %Location{line: 28, column: 55}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 28, column: 56},
          end: %Location{line: 29, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "to",
          value: nil,
          start: %Location{line: 29, column: 17},
          end: %Location{line: 29, column: 18}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 29, column: 19},
          end: %Location{line: 29, column: 19}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 29, column: 21},
          end: %Location{line: 29, column: 21}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 29, column: 22},
          end: %Location{line: 29, column: 22}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 29, column: 23},
          end: %Location{line: 29, column: 23}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 29, column: 24},
          end: %Location{line: 29, column: 26}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 29, column: 27},
          end: %Location{line: 29, column: 27}
        },
        %Token{
          kind: :WORD,
          text: "support_team",
          value: nil,
          start: %Location{line: 29, column: 28},
          end: %Location{line: 29, column: 39}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 29, column: 40},
          end: %Location{line: 29, column: 40}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 29, column: 42},
          end: %Location{line: 29, column: 42}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 29, column: 44},
          end: %Location{line: 29, column: 44}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 29, column: 45},
          end: %Location{line: 29, column: 45}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 29, column: 46},
          end: %Location{line: 29, column: 48}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 29, column: 49},
          end: %Location{line: 29, column: 49}
        },
        %Token{
          kind: :WORD,
          text: "mail_domain",
          value: nil,
          start: %Location{line: 29, column: 50},
          end: %Location{line: 29, column: 60}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 29, column: 61},
          end: %Location{line: 29, column: 61}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 29, column: 62},
          end: %Location{line: 29, column: 62}
        },
        %Token{
          kind: :STRING,
          text: "'QA'",
          value: "QA",
          start: %Location{line: 29, column: 64},
          end: %Location{line: 29, column: 67}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 29, column: 69},
          end: %Location{line: 29, column: 69}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 29, column: 71},
          end: %Location{line: 29, column: 71}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 29, column: 72},
          end: %Location{line: 29, column: 72}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 29, column: 73},
          end: %Location{line: 29, column: 75}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 29, column: 76},
          end: %Location{line: 29, column: 76}
        },
        %Token{
          kind: :WORD,
          text: "mail_domain",
          value: nil,
          start: %Location{line: 29, column: 77},
          end: %Location{line: 29, column: 87}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 29, column: 88},
          end: %Location{line: 29, column: 88}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 29, column: 89},
          end: %Location{line: 29, column: 89}
        },
        %Token{
          kind: :STRING,
          text: "'product_manager'",
          value: "product_manager",
          start: %Location{line: 29, column: 91},
          end: %Location{line: 29, column: 107}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 29, column: 109},
          end: %Location{line: 29, column: 109}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 29, column: 111},
          end: %Location{line: 29, column: 111}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 29, column: 112},
          end: %Location{line: 29, column: 112}
        },
        %Token{
          kind: :WORD,
          text: "app",
          value: nil,
          start: %Location{line: 29, column: 113},
          end: %Location{line: 29, column: 115}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 29, column: 116},
          end: %Location{line: 29, column: 116}
        },
        %Token{
          kind: :WORD,
          text: "mail_domain",
          value: nil,
          start: %Location{line: 29, column: 117},
          end: %Location{line: 29, column: 127}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 29, column: 128},
          end: %Location{line: 29, column: 128}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 29, column: 129},
          end: %Location{line: 29, column: 129}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 29, column: 130},
          end: %Location{line: 29, column: 130}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 29, column: 131},
          end: %Location{line: 30, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "subject",
          value: nil,
          start: %Location{line: 30, column: 17},
          end: %Location{line: 30, column: 23}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 30, column: 24},
          end: %Location{line: 30, column: 24}
        },
        %Token{
          kind: :STRING,
          text: "'Take cover'",
          value: "Take cover",
          start: %Location{line: 30, column: 26},
          end: %Location{line: 30, column: 37}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 30, column: 39},
          end: %Location{line: 30, column: 39}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 30, column: 41},
          end: %Location{line: 30, column: 41}
        },
        %Token{
          kind: :NEWLINE,
          text: "# random comment",
          value: nil,
          start: %Location{line: 30, column: 43},
          end: %Location{line: 31, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 31, column: 1},
          end: %Location{line: 31, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 31, column: 2},
          end: %Location{line: 32, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# the loggers which are configured",
          value: nil,
          start: %Location{line: 32, column: 1},
          end: %Location{line: 33, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "loggers",
          value: nil,
          start: %Location{line: 33, column: 1},
          end: %Location{line: 33, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 33, column: 8},
          end: %Location{line: 33, column: 8}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 33, column: 9},
          end: %Location{line: 34, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 34, column: 1},
          end: %Location{line: 34, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 34, column: 2},
          end: %Location{line: 35, column: 0}
        },
        %Token{
          kind: :STRING,
          text: "\"input\"",
          value: "input",
          start: %Location{line: 35, column: 3},
          end: %Location{line: 35, column: 9}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 35, column: 15},
          end: %Location{line: 35, column: 15}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 35, column: 17},
          end: %Location{line: 35, column: 17}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 35, column: 19},
          end: %Location{line: 35, column: 26}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 35, column: 27},
          end: %Location{line: 35, column: 27}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 35, column: 29},
          end: %Location{line: 35, column: 29}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 35, column: 30},
          end: %Location{line: 35, column: 30}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 35, column: 31},
          end: %Location{line: 35, column: 31}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 35, column: 32},
          end: %Location{line: 35, column: 39}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 35, column: 40},
          end: %Location{line: 35, column: 40}
        },
        %Token{
          kind: :WORD,
          text: "socket",
          value: nil,
          start: %Location{line: 35, column: 41},
          end: %Location{line: 35, column: 46}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 35, column: 47},
          end: %Location{line: 35, column: 47}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 35, column: 48},
          end: %Location{line: 35, column: 48}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 35, column: 50},
          end: %Location{line: 35, column: 50}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 35, column: 51},
          end: %Location{line: 36, column: 0}
        },
        %Token{
          kind: :STRING,
          text: "\"input.xls\"",
          value: "input.xls",
          start: %Location{line: 36, column: 3},
          end: %Location{line: 36, column: 13}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 36, column: 15},
          end: %Location{line: 36, column: 15}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 36, column: 17},
          end: %Location{line: 36, column: 17}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 36, column: 19},
          end: %Location{line: 36, column: 26}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 36, column: 27},
          end: %Location{line: 36, column: 27}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 36, column: 29},
          end: %Location{line: 36, column: 29}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 36, column: 30},
          end: %Location{line: 36, column: 30}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 36, column: 31},
          end: %Location{line: 36, column: 31}
        },
        %Token{
          kind: :WORD,
          text: "handlers",
          value: nil,
          start: %Location{line: 36, column: 32},
          end: %Location{line: 36, column: 39}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 36, column: 40},
          end: %Location{line: 36, column: 40}
        },
        %Token{
          kind: :WORD,
          text: "nt_eventlog",
          value: nil,
          start: %Location{line: 36, column: 41},
          end: %Location{line: 36, column: 51}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 36, column: 52},
          end: %Location{line: 36, column: 52}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 36, column: 53},
          end: %Location{line: 36, column: 53}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 36, column: 55},
          end: %Location{line: 36, column: 55}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 36, column: 56},
          end: %Location{line: 37, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 37, column: 1},
          end: %Location{line: 37, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 37, column: 2},
          end: %Location{line: 37, column: 2}
        }
      ],
      "C15" => [
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 1}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 2},
          end: %Location{line: 1, column: 2}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 1, column: 4},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 1, column: 5},
          end: %Location{line: 1, column: 5}
        },
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :WORD,
          text: "bar",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 12}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 1, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "baz",
          value: nil,
          start: %Location{line: 1, column: 15},
          end: %Location{line: 1, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 18},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 2, column: 2}
        },
        %Token{
          kind: :BACKTICK,
          text: "`bish.bash`",
          value: "bish.bash",
          start: %Location{line: 2, column: 4},
          end: %Location{line: 2, column: 14}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 2, column: 15},
          end: %Location{line: 2, column: 15}
        },
        %Token{
          kind: :WORD,
          text: "bosh",
          value: nil,
          start: %Location{line: 2, column: 16},
          end: %Location{line: 2, column: 19}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 20},
          end: %Location{line: 2, column: 20}
        }
      ],
      "C16" => [
        %Token{
          kind: :WORD,
          text: "test",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :FALSE,
          text: "false",
          value: false,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 12}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "another_test",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 13},
          end: %Location{line: 2, column: 13}
        },
        %Token{
          kind: :TRUE,
          text: "true",
          value: true,
          start: %Location{line: 2, column: 15},
          end: %Location{line: 2, column: 18}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 19},
          end: %Location{line: 2, column: 19}
        }
      ],
      "C17" => [
        %Token{
          kind: :WORD,
          text: "test",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :NONE,
          text: "null",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 1, column: 12},
          end: %Location{line: 1, column: 12}
        }
      ],
      "C18" => [
        %Token{
          kind: :WORD,
          text: "root",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 5},
          end: %Location{line: 1, column: 5}
        },
        %Token{
          kind: :INTEGER,
          text: "1",
          value: 1,
          start: %Location{line: 1, column: 7},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 7},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :FLOAT,
          text: "1.7",
          value: 1.7,
          start: %Location{line: 2, column: 9},
          end: %Location{line: 2, column: 11}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 12},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "neg",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 4},
          end: %Location{line: 3, column: 4}
        },
        %Token{
          kind: :INTEGER,
          text: "-1",
          value: -1,
          start: %Location{line: 3, column: 6},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 8},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "negfloat",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 9},
          end: %Location{line: 4, column: 9}
        },
        %Token{
          kind: :FLOAT,
          text: "-2.0",
          value: -2.0,
          start: %Location{line: 4, column: 11},
          end: %Location{line: 4, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 15},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "posexponent",
          value: nil,
          start: %Location{line: 5, column: 1},
          end: %Location{line: 5, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :FLOAT,
          text: "2.0999999e-08",
          value: 2.0999999e-08,
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 26}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 27},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "negexponent",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 12},
          end: %Location{line: 6, column: 12}
        },
        %Token{
          kind: :FLOAT,
          text: "-2.0999999e-08",
          value: -2.0999999e-08,
          start: %Location{line: 6, column: 14},
          end: %Location{line: 6, column: 27}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 28},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "exponent",
          value: nil,
          start: %Location{line: 7, column: 1},
          end: %Location{line: 7, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 7, column: 9},
          end: %Location{line: 7, column: 9}
        },
        %Token{
          kind: :FLOAT,
          text: "2.0999999e08",
          value: 209_999_990.0,
          start: %Location{line: 7, column: 11},
          end: %Location{line: 7, column: 22}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 7, column: 23},
          end: %Location{line: 7, column: 23}
        }
      ],
      "C19" => [
        %Token{
          kind: :WORD,
          text: "mixed",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "\"VALIGN\"",
          value: "VALIGN",
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 17}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 18},
          end: %Location{line: 1, column: 18}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 20},
          end: %Location{line: 1, column: 20}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 1, column: 22},
          end: %Location{line: 1, column: 22}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 23},
          end: %Location{line: 1, column: 23}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 1, column: 25},
          end: %Location{line: 1, column: 25}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 1, column: 27},
          end: %Location{line: 1, column: 27}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 28},
          end: %Location{line: 1, column: 28}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 30},
          end: %Location{line: 1, column: 30}
        },
        %Token{
          kind: :INTEGER,
          text: "-1",
          value: -1,
          start: %Location{line: 1, column: 32},
          end: %Location{line: 1, column: 33}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 34},
          end: %Location{line: 1, column: 34}
        },
        %Token{
          kind: :INTEGER,
          text: "-1",
          value: -1,
          start: %Location{line: 1, column: 36},
          end: %Location{line: 1, column: 37}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 1, column: 39},
          end: %Location{line: 1, column: 39}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 1, column: 40},
          end: %Location{line: 1, column: 40}
        },
        %Token{
          kind: :STRING,
          text: "\"TOP\"",
          value: "TOP",
          start: %Location{line: 1, column: 42},
          end: %Location{line: 1, column: 46}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 1, column: 48},
          end: %Location{line: 1, column: 48}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 49},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "simple",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 7},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 9},
          end: %Location{line: 2, column: 9}
        },
        %Token{
          kind: :INTEGER,
          text: "1",
          value: 1,
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 10}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 11},
          end: %Location{line: 2, column: 11}
        },
        %Token{
          kind: :INTEGER,
          text: "2",
          value: 2,
          start: %Location{line: 2, column: 13},
          end: %Location{line: 2, column: 13}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 15},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "nested",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 7},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 3, column: 9},
          end: %Location{line: 3, column: 9}
        },
        %Token{
          kind: :INTEGER,
          text: "1",
          value: 1,
          start: %Location{line: 3, column: 10},
          end: %Location{line: 3, column: 10}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 11},
          end: %Location{line: 3, column: 11}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 3, column: 13},
          end: %Location{line: 3, column: 13}
        },
        %Token{
          kind: :INTEGER,
          text: "2",
          value: 2,
          start: %Location{line: 3, column: 14},
          end: %Location{line: 3, column: 14}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 15},
          end: %Location{line: 3, column: 15}
        },
        %Token{
          kind: :INTEGER,
          text: "3",
          value: 3,
          start: %Location{line: 3, column: 17},
          end: %Location{line: 3, column: 17}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 3, column: 18},
          end: %Location{line: 3, column: 18}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 19},
          end: %Location{line: 3, column: 19}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 3, column: 21},
          end: %Location{line: 3, column: 21}
        },
        %Token{
          kind: :INTEGER,
          text: "4",
          value: 4,
          start: %Location{line: 3, column: 22},
          end: %Location{line: 3, column: 22}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 23},
          end: %Location{line: 3, column: 23}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 3, column: 25},
          end: %Location{line: 3, column: 25}
        },
        %Token{
          kind: :INTEGER,
          text: "5",
          value: 5,
          start: %Location{line: 3, column: 26},
          end: %Location{line: 3, column: 26}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 3, column: 27},
          end: %Location{line: 3, column: 27}
        },
        %Token{
          kind: :INTEGER,
          text: "6",
          value: 6,
          start: %Location{line: 3, column: 29},
          end: %Location{line: 3, column: 29}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 3, column: 30},
          end: %Location{line: 3, column: 30}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 3, column: 31},
          end: %Location{line: 3, column: 31}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 3, column: 32},
          end: %Location{line: 3, column: 32}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 3, column: 33},
          end: %Location{line: 3, column: 33}
        }
      ],
      "C20" => [
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :INTEGER,
          text: "10",
          value: 10,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 12},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :INTEGER,
          text: "5",
          value: 5,
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 10}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 11},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "value3",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 8},
          end: %Location{line: 3, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "'abc'",
          value: "abc",
          start: %Location{line: 3, column: 10},
          end: %Location{line: 3, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 15},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "value4",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 8},
          end: %Location{line: 4, column: 8}
        },
        %Token{
          kind: :STRING,
          text: "\"\'ghi\'\"",
          value: "'ghi'",
          start: %Location{line: 4, column: 10},
          end: %Location{line: 4, column: 16}
        },
        %Token{
          kind: :STRING,
          text: "\'\"jkl\"\'",
          value: "\"jkl\"",
          start: %Location{line: 4, column: 18},
          end: %Location{line: 4, column: 24}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 25},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "value5",
          value: nil,
          start: %Location{line: 5, column: 1},
          end: %Location{line: 5, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 8},
          end: %Location{line: 5, column: 8}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 5, column: 10},
          end: %Location{line: 5, column: 10}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 11},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "value6",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 8},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 6, column: 10},
          end: %Location{line: 6, column: 10}
        },
        %Token{
          kind: :STRING,
          text: "'a'",
          value: "a",
          start: %Location{line: 6, column: 12},
          end: %Location{line: 6, column: 14}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 16},
          end: %Location{line: 6, column: 16}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 6, column: 18},
          end: %Location{line: 6, column: 18}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 6, column: 19},
          end: %Location{line: 6, column: 19}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 6, column: 20},
          end: %Location{line: 6, column: 25}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 6, column: 26},
          end: %Location{line: 6, column: 26}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 6, column: 27},
          end: %Location{line: 6, column: 27}
        },
        %Token{
          kind: :STRING,
          text: "'b'",
          value: "b",
          start: %Location{line: 6, column: 29},
          end: %Location{line: 6, column: 31}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 32},
          end: %Location{line: 6, column: 32}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 6, column: 34},
          end: %Location{line: 6, column: 34}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 6, column: 35},
          end: %Location{line: 6, column: 35}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 6, column: 36},
          end: %Location{line: 6, column: 41}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 6, column: 42},
          end: %Location{line: 6, column: 42}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 6, column: 44},
          end: %Location{line: 6, column: 44}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 45},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived1",
          value: nil,
          start: %Location{line: 7, column: 1},
          end: %Location{line: 7, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 7, column: 10},
          end: %Location{line: 7, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 7, column: 12},
          end: %Location{line: 7, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 7, column: 13},
          end: %Location{line: 7, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 7, column: 14},
          end: %Location{line: 7, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 7, column: 20},
          end: %Location{line: 7, column: 20}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 7, column: 22},
          end: %Location{line: 7, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 7, column: 24},
          end: %Location{line: 7, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 7, column: 25},
          end: %Location{line: 7, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 7, column: 26},
          end: %Location{line: 7, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 7, column: 32},
          end: %Location{line: 7, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 7, column: 33},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived2",
          value: nil,
          start: %Location{line: 8, column: 1},
          end: %Location{line: 8, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 8, column: 10},
          end: %Location{line: 8, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 8, column: 12},
          end: %Location{line: 8, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 8, column: 13},
          end: %Location{line: 8, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 8, column: 14},
          end: %Location{line: 8, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 8, column: 20},
          end: %Location{line: 8, column: 20}
        },
        %Token{
          kind: :MINUS,
          text: "-",
          value: nil,
          start: %Location{line: 8, column: 22},
          end: %Location{line: 8, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 8, column: 24},
          end: %Location{line: 8, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 8, column: 25},
          end: %Location{line: 8, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 8, column: 26},
          end: %Location{line: 8, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 8, column: 32},
          end: %Location{line: 8, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 8, column: 33},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived3",
          value: nil,
          start: %Location{line: 9, column: 1},
          end: %Location{line: 9, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 9, column: 10},
          end: %Location{line: 9, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 9, column: 12},
          end: %Location{line: 9, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 9, column: 13},
          end: %Location{line: 9, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 9, column: 14},
          end: %Location{line: 9, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 9, column: 20},
          end: %Location{line: 9, column: 20}
        },
        %Token{
          kind: :STAR,
          text: "*",
          value: nil,
          start: %Location{line: 9, column: 22},
          end: %Location{line: 9, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 9, column: 24},
          end: %Location{line: 9, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 9, column: 25},
          end: %Location{line: 9, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 9, column: 26},
          end: %Location{line: 9, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 9, column: 32},
          end: %Location{line: 9, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 33},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived4",
          value: nil,
          start: %Location{line: 10, column: 1},
          end: %Location{line: 10, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 10, column: 10},
          end: %Location{line: 10, column: 10}
        },
        %Token{
          kind: :LPAREN,
          text: "(",
          value: nil,
          start: %Location{line: 10, column: 12},
          end: %Location{line: 10, column: 12}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 10, column: 13},
          end: %Location{line: 10, column: 13}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 10, column: 14},
          end: %Location{line: 10, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 10, column: 15},
          end: %Location{line: 10, column: 20}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 10, column: 21},
          end: %Location{line: 10, column: 21}
        },
        %Token{
          kind: :SLASH,
          text: "/",
          value: nil,
          start: %Location{line: 10, column: 23},
          end: %Location{line: 10, column: 23}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 10, column: 25},
          end: %Location{line: 10, column: 25}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 10, column: 26},
          end: %Location{line: 10, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 10, column: 27},
          end: %Location{line: 10, column: 32}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 10, column: 33},
          end: %Location{line: 10, column: 33}
        },
        %Token{
          kind: :RPAREN,
          text: ")",
          value: nil,
          start: %Location{line: 10, column: 34},
          end: %Location{line: 10, column: 34}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 10, column: 36},
          end: %Location{line: 10, column: 36}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 10, column: 38},
          end: %Location{line: 10, column: 38}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 10, column: 39},
          end: %Location{line: 10, column: 39}
        },
        %Token{
          kind: :WORD,
          text: "value5",
          value: nil,
          start: %Location{line: 10, column: 40},
          end: %Location{line: 10, column: 45}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 10, column: 46},
          end: %Location{line: 10, column: 46}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 10, column: 47},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived5",
          value: nil,
          start: %Location{line: 11, column: 1},
          end: %Location{line: 11, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 11, column: 10},
          end: %Location{line: 11, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 11, column: 12},
          end: %Location{line: 11, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 11, column: 13},
          end: %Location{line: 11, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 11, column: 14},
          end: %Location{line: 11, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 11, column: 20},
          end: %Location{line: 11, column: 20}
        },
        %Token{
          kind: :MODULO,
          text: "%",
          value: nil,
          start: %Location{line: 11, column: 22},
          end: %Location{line: 11, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 11, column: 24},
          end: %Location{line: 11, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 11, column: 25},
          end: %Location{line: 11, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value2",
          value: nil,
          start: %Location{line: 11, column: 26},
          end: %Location{line: 11, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 11, column: 32},
          end: %Location{line: 11, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 11, column: 33},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived6",
          value: nil,
          start: %Location{line: 12, column: 1},
          end: %Location{line: 12, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 12, column: 10},
          end: %Location{line: 12, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 12, column: 12},
          end: %Location{line: 12, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 12, column: 13},
          end: %Location{line: 12, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value3",
          value: nil,
          start: %Location{line: 12, column: 14},
          end: %Location{line: 12, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 12, column: 20},
          end: %Location{line: 12, column: 20}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 12, column: 22},
          end: %Location{line: 12, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 12, column: 24},
          end: %Location{line: 12, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 12, column: 25},
          end: %Location{line: 12, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value4",
          value: nil,
          start: %Location{line: 12, column: 26},
          end: %Location{line: 12, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 12, column: 32},
          end: %Location{line: 12, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 33},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived7",
          value: nil,
          start: %Location{line: 13, column: 1},
          end: %Location{line: 13, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 13, column: 10},
          end: %Location{line: 13, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 13, column: 12},
          end: %Location{line: 13, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 13, column: 13},
          end: %Location{line: 13, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value3",
          value: nil,
          start: %Location{line: 13, column: 14},
          end: %Location{line: 13, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 13, column: 20},
          end: %Location{line: 13, column: 20}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 13, column: 22},
          end: %Location{line: 13, column: 22}
        },
        %Token{
          kind: :STRING,
          text: "'def'",
          value: "def",
          start: %Location{line: 13, column: 24},
          end: %Location{line: 13, column: 28}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 13, column: 30},
          end: %Location{line: 13, column: 30}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 13, column: 32},
          end: %Location{line: 13, column: 32}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 13, column: 33},
          end: %Location{line: 13, column: 33}
        },
        %Token{
          kind: :WORD,
          text: "value4",
          value: nil,
          start: %Location{line: 13, column: 34},
          end: %Location{line: 13, column: 39}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 13, column: 40},
          end: %Location{line: 13, column: 40}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 13, column: 41},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived8",
          value: nil,
          start: %Location{line: 14, column: 1},
          end: %Location{line: 14, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 14, column: 10},
          end: %Location{line: 14, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 14, column: 12},
          end: %Location{line: 14, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 13},
          end: %Location{line: 14, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value3",
          value: nil,
          start: %Location{line: 14, column: 14},
          end: %Location{line: 14, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 14, column: 20},
          end: %Location{line: 14, column: 20}
        },
        %Token{
          kind: :MINUS,
          text: "-",
          value: nil,
          start: %Location{line: 14, column: 22},
          end: %Location{line: 14, column: 22}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 14, column: 24},
          end: %Location{line: 14, column: 24}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 25},
          end: %Location{line: 14, column: 25}
        },
        %Token{
          kind: :WORD,
          text: "value4",
          value: nil,
          start: %Location{line: 14, column: 26},
          end: %Location{line: 14, column: 31}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 14, column: 32},
          end: %Location{line: 14, column: 32}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 33},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived9",
          value: nil,
          start: %Location{line: 15, column: 1},
          end: %Location{line: 15, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 15, column: 10},
          end: %Location{line: 15, column: 10}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 15, column: 12},
          end: %Location{line: 15, column: 12}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 15, column: 13},
          end: %Location{line: 15, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 15, column: 14},
          end: %Location{line: 15, column: 19}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 15, column: 20},
          end: %Location{line: 15, column: 20}
        },
        %Token{
          kind: :SLASHSLASH,
          text: "//",
          value: nil,
          start: %Location{line: 15, column: 22},
          end: %Location{line: 15, column: 23}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 15, column: 25},
          end: %Location{line: 15, column: 25}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 15, column: 26},
          end: %Location{line: 15, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "value5",
          value: nil,
          start: %Location{line: 15, column: 27},
          end: %Location{line: 15, column: 32}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 15, column: 33},
          end: %Location{line: 15, column: 33}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 15, column: 34},
          end: %Location{line: 16, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived10",
          value: nil,
          start: %Location{line: 16, column: 1},
          end: %Location{line: 16, column: 9}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 16, column: 11},
          end: %Location{line: 16, column: 11}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 16, column: 13},
          end: %Location{line: 16, column: 13}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 16, column: 14},
          end: %Location{line: 16, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "value1",
          value: nil,
          start: %Location{line: 16, column: 15},
          end: %Location{line: 16, column: 20}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 16, column: 21},
          end: %Location{line: 16, column: 21}
        },
        %Token{
          kind: :MODULO,
          text: "%",
          value: nil,
          start: %Location{line: 16, column: 23},
          end: %Location{line: 16, column: 23}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 16, column: 25},
          end: %Location{line: 16, column: 25}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 16, column: 26},
          end: %Location{line: 16, column: 26}
        },
        %Token{
          kind: :WORD,
          text: "value5",
          value: nil,
          start: %Location{line: 16, column: 27},
          end: %Location{line: 16, column: 32}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 16, column: 33},
          end: %Location{line: 16, column: 33}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 16, column: 34},
          end: %Location{line: 17, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived11",
          value: nil,
          start: %Location{line: 17, column: 1},
          end: %Location{line: 17, column: 9}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 17, column: 11},
          end: %Location{line: 17, column: 11}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 17, column: 13},
          end: %Location{line: 17, column: 13}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 17, column: 14},
          end: %Location{line: 17, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "value17",
          value: nil,
          start: %Location{line: 17, column: 15},
          end: %Location{line: 17, column: 21}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 17, column: 22},
          end: %Location{line: 17, column: 22}
        },
        %Token{
          kind: :NEWLINE,
          text: "# non-existent",
          value: nil,
          start: %Location{line: 17, column: 41},
          end: %Location{line: 18, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived12",
          value: nil,
          start: %Location{line: 18, column: 1},
          end: %Location{line: 18, column: 9}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 18, column: 11},
          end: %Location{line: 18, column: 11}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 18, column: 13},
          end: %Location{line: 18, column: 13}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 18, column: 14},
          end: %Location{line: 18, column: 14}
        },
        %Token{
          kind: :WORD,
          text: "value6",
          value: nil,
          start: %Location{line: 18, column: 15},
          end: %Location{line: 18, column: 20}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 18, column: 21},
          end: %Location{line: 18, column: 21}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 18, column: 22},
          end: %Location{line: 18, column: 22}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 18, column: 23},
          end: %Location{line: 18, column: 23}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 18, column: 25},
          end: %Location{line: 18, column: 25}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 18, column: 27},
          end: %Location{line: 18, column: 27}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 18, column: 28},
          end: %Location{line: 18, column: 28}
        },
        %Token{
          kind: :WORD,
          text: "value6",
          value: nil,
          start: %Location{line: 18, column: 29},
          end: %Location{line: 18, column: 34}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 18, column: 35},
          end: %Location{line: 18, column: 35}
        },
        %Token{
          kind: :DOT,
          text: ".",
          value: nil,
          start: %Location{line: 18, column: 36},
          end: %Location{line: 18, column: 36}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 18, column: 37},
          end: %Location{line: 18, column: 37}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 18, column: 38},
          end: %Location{line: 18, column: 38}
        }
      ],
      "C21" => [
        %Token{
          kind: :WORD,
          text: "stderr",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 22},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stdout",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdout`",
          value: "sys.stdout",
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 22},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stdin",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 3, column: 7},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdin`",
          value: "sys.stdin",
          start: %Location{line: 3, column: 9},
          end: %Location{line: 3, column: 19}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 20},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "debug",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 7},
          end: %Location{line: 4, column: 7}
        },
        %Token{
          kind: :BACKTICK,
          text: "`debug`",
          value: "debug",
          start: %Location{line: 4, column: 9},
          end: %Location{line: 4, column: 15}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 16},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "DEBUG",
          value: nil,
          start: %Location{line: 5, column: 1},
          end: %Location{line: 5, column: 5}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 7},
          end: %Location{line: 5, column: 7}
        },
        %Token{
          kind: :BACKTICK,
          text: "`DEBUG`",
          value: "DEBUG",
          start: %Location{line: 5, column: 9},
          end: %Location{line: 5, column: 15}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 16},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "derived",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 8},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 6, column: 10},
          end: %Location{line: 6, column: 10}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 6, column: 11},
          end: %Location{line: 6, column: 11}
        },
        %Token{
          kind: :WORD,
          text: "DEBUG",
          value: nil,
          start: %Location{line: 6, column: 12},
          end: %Location{line: 6, column: 16}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 6, column: 17},
          end: %Location{line: 6, column: 17}
        },
        %Token{
          kind: :STAR,
          text: "*",
          value: nil,
          start: %Location{line: 6, column: 19},
          end: %Location{line: 6, column: 19}
        },
        %Token{
          kind: :INTEGER,
          text: "10",
          value: 10,
          start: %Location{line: 6, column: 21},
          end: %Location{line: 6, column: 22}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 6, column: 23},
          end: %Location{line: 6, column: 23}
        }
      ],
      "C22" => [
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 3, column: 3},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 4},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 4, column: 12},
          end: %Location{line: 4, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stderr`",
          value: "sys.stderr",
          start: %Location{line: 4, column: 14},
          end: %Location{line: 4, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 26},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 12},
          end: %Location{line: 5, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'Welcome'",
          value: "Welcome",
          start: %Location{line: 5, column: 14},
          end: %Location{line: 5, column: 22}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 23},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 6, column: 5},
          end: %Location{line: 6, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 6, column: 9},
          end: %Location{line: 6, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Harry'",
          value: "Harry",
          start: %Location{line: 6, column: 11},
          end: %Location{line: 6, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 6, column: 18},
          end: %Location{line: 7, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 7, column: 3},
          end: %Location{line: 7, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 7, column: 4},
          end: %Location{line: 8, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 8, column: 3},
          end: %Location{line: 8, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 8, column: 4},
          end: %Location{line: 9, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "stream",
          value: nil,
          start: %Location{line: 9, column: 5},
          end: %Location{line: 9, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 9, column: 12},
          end: %Location{line: 9, column: 12}
        },
        %Token{
          kind: :BACKTICK,
          text: "`sys.stdout`",
          value: "sys.stdout",
          start: %Location{line: 9, column: 14},
          end: %Location{line: 9, column: 25}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 9, column: 26},
          end: %Location{line: 10, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 10, column: 5},
          end: %Location{line: 10, column: 11}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 10, column: 12},
          end: %Location{line: 10, column: 12}
        },
        %Token{
          kind: :STRING,
          text: "'Welkom'",
          value: "Welkom",
          start: %Location{line: 10, column: 14},
          end: %Location{line: 10, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 10, column: 22},
          end: %Location{line: 11, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 11, column: 5},
          end: %Location{line: 11, column: 8}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 11, column: 9},
          end: %Location{line: 11, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Ruud'",
          value: "Ruud",
          start: %Location{line: 11, column: 11},
          end: %Location{line: 11, column: 16}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 11, column: 17},
          end: %Location{line: 12, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 12, column: 3},
          end: %Location{line: 12, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 12, column: 4},
          end: %Location{line: 13, column: 0}
        },
        %Token{
          kind: :NEWLINE,
          text: "# override an existing value with specific elements",
          value: nil,
          start: %Location{line: 13, column: 3},
          end: %Location{line: 14, column: 0}
        },
        %Token{
          kind: :DOLLAR,
          text: "$",
          value: nil,
          start: %Location{line: 14, column: 3},
          end: %Location{line: 14, column: 3}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 4},
          end: %Location{line: 14, column: 4}
        },
        %Token{
          kind: :WORD,
          text: "messages",
          value: nil,
          start: %Location{line: 14, column: 5},
          end: %Location{line: 14, column: 12}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 14, column: 13},
          end: %Location{line: 14, column: 13}
        },
        %Token{
          kind: :INTEGER,
          text: "0",
          value: 0,
          start: %Location{line: 14, column: 14},
          end: %Location{line: 14, column: 14}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 14, column: 15},
          end: %Location{line: 14, column: 15}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 14, column: 16},
          end: %Location{line: 14, column: 16}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 14, column: 18},
          end: %Location{line: 14, column: 18}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 14, column: 20},
          end: %Location{line: 14, column: 20}
        },
        %Token{
          kind: :WORD,
          text: "message",
          value: nil,
          start: %Location{line: 14, column: 21},
          end: %Location{line: 14, column: 27}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 14, column: 28},
          end: %Location{line: 14, column: 28}
        },
        %Token{
          kind: :WORD,
          text: "Bienvenue",
          value: nil,
          start: %Location{line: 14, column: 30},
          end: %Location{line: 14, column: 38}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 14, column: 39},
          end: %Location{line: 14, column: 39}
        },
        %Token{
          kind: :WORD,
          text: "name",
          value: nil,
          start: %Location{line: 14, column: 41},
          end: %Location{line: 14, column: 44}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 14, column: 45},
          end: %Location{line: 14, column: 45}
        },
        %Token{
          kind: :WORD,
          text: "Yves",
          value: nil,
          start: %Location{line: 14, column: 47},
          end: %Location{line: 14, column: 50}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 14, column: 51},
          end: %Location{line: 14, column: 51}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 14, column: 52},
          end: %Location{line: 15, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 15, column: 1},
          end: %Location{line: 15, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 15, column: 2},
          end: %Location{line: 15, column: 2}
        }
      ],
      "C23" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 4},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 5},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "bar",
          value: nil,
          start: %Location{line: 3, column: 5},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 8},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "baz",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 7}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 8},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "bozz",
          value: nil,
          start: %Location{line: 5, column: 5},
          end: %Location{line: 5, column: 8}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 5, column: 9},
          end: %Location{line: 6, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 6, column: 1},
          end: %Location{line: 6, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 6, column: 2},
          end: %Location{line: 6, column: 2}
        }
      ],
      "C24" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 4},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 6},
          end: %Location{line: 1, column: 6}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 7},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :STRING,
          text: "'bar'",
          value: "bar",
          start: %Location{line: 2, column: 5},
          end: %Location{line: 2, column: 9}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 10}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 11},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 3, column: 5},
          end: %Location{line: 3, column: 5}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 3, column: 7},
          end: %Location{line: 3, column: 7}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 3, column: 9},
          end: %Location{line: 3, column: 9}
        },
        %Token{
          kind: :MINUS,
          text: "-",
          value: nil,
          start: %Location{line: 3, column: 11},
          end: %Location{line: 3, column: 11}
        },
        %Token{
          kind: :WORD,
          text: "c",
          value: nil,
          start: %Location{line: 3, column: 13},
          end: %Location{line: 3, column: 13}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 3, column: 15},
          end: %Location{line: 3, column: 15}
        },
        %Token{
          kind: :WORD,
          text: "d",
          value: nil,
          start: %Location{line: 3, column: 17},
          end: %Location{line: 3, column: 17}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 18},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 4, column: 1},
          end: %Location{line: 4, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 4, column: 2},
          end: %Location{line: 4, column: 2}
        }
      ],
      "C25" => [
        %Token{
          kind: :WORD,
          text: "unicode",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :ASSIGN,
          text: "=",
          value: nil,
          start: %Location{line: 1, column: 9},
          end: %Location{line: 1, column: 9}
        },
        %Token{
          kind: :STRING,
          text: "'Grüß Gott'",
          value: "Grüß Gott",
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 22},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "more_unicode",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 13},
          end: %Location{line: 2, column: 13}
        },
        %Token{
          kind: :STRING,
          text: "'Øresund'",
          value: "Øresund",
          start: %Location{line: 2, column: 15},
          end: %Location{line: 2, column: 23}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 24},
          end: %Location{line: 2, column: 24}
        }
      ],
      "C26" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 5},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :STRING,
          text: "'bar'",
          value: "bar",
          start: %Location{line: 2, column: 9},
          end: %Location{line: 2, column: 13}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 14}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 15},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 3, column: 9},
          end: %Location{line: 3, column: 9}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 3, column: 11},
          end: %Location{line: 3, column: 11}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 3, column: 13},
          end: %Location{line: 3, column: 13}
        },
        %Token{
          kind: :MINUS,
          text: "-",
          value: nil,
          start: %Location{line: 3, column: 15},
          end: %Location{line: 3, column: 15}
        },
        %Token{
          kind: :WORD,
          text: "c",
          value: nil,
          start: %Location{line: 3, column: 17},
          end: %Location{line: 3, column: 17}
        },
        %Token{
          kind: :PLUS,
          text: "+",
          value: nil,
          start: %Location{line: 3, column: 19},
          end: %Location{line: 3, column: 19}
        },
        %Token{
          kind: :WORD,
          text: "d",
          value: nil,
          start: %Location{line: 3, column: 21},
          end: %Location{line: 3, column: 21}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 3, column: 22},
          end: %Location{line: 4, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 4, column: 5},
          end: %Location{line: 4, column: 5}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 4, column: 6},
          end: %Location{line: 4, column: 6}
        }
      ],
      "C27" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 5},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 1, column: 8}
        },
        %Token{
          kind: :LPAREN,
          text: "(",
          value: nil,
          start: %Location{line: 1, column: 10},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :BITAND,
          text: "&",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 1, column: 15},
          end: %Location{line: 1, column: 15}
        },
        %Token{
          kind: :RPAREN,
          text: ")",
          value: nil,
          start: %Location{line: 1, column: 16},
          end: %Location{line: 1, column: 16}
        },
        %Token{
          kind: :BITXOR,
          text: "^",
          value: nil,
          start: %Location{line: 1, column: 18},
          end: %Location{line: 1, column: 18}
        },
        %Token{
          kind: :LPAREN,
          text: "(",
          value: nil,
          start: %Location{line: 1, column: 20},
          end: %Location{line: 1, column: 20}
        },
        %Token{
          kind: :WORD,
          text: "c",
          value: nil,
          start: %Location{line: 1, column: 21},
          end: %Location{line: 1, column: 21}
        },
        %Token{
          kind: :BITOR,
          text: "|",
          value: nil,
          start: %Location{line: 1, column: 23},
          end: %Location{line: 1, column: 23}
        },
        %Token{
          kind: :WORD,
          text: "d",
          value: nil,
          start: %Location{line: 1, column: 25},
          end: %Location{line: 1, column: 25}
        },
        %Token{
          kind: :RPAREN,
          text: ")",
          value: nil,
          start: %Location{line: 1, column: 26},
          end: %Location{line: 1, column: 26}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 1, column: 27},
          end: %Location{line: 1, column: 27}
        }
      ],
      "C28" => [
        %Token{
          kind: :WORD,
          text: "foo",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 4},
          end: %Location{line: 1, column: 4}
        },
        %Token{
          kind: :STRING,
          text: "'''\na multi-line string with internal\n'' and \"\" sequences\n'''",
          value: "\na multi-line string with internal\n'' and \"\" sequences\n",
          start: %Location{line: 1, column: 6},
          end: %Location{line: 4, column: 3}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 4, column: 4},
          end: %Location{line: 5, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "bar",
          value: nil,
          start: %Location{line: 5, column: 1},
          end: %Location{line: 5, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 5, column: 4},
          end: %Location{line: 5, column: 4}
        },
        %Token{
          kind: :STRING,
          text: "\"\"\"\nanother multi-line string with internal\n'' and \"\" sequences\n\"\"\"",
          value: "\nanother multi-line string with internal\n'' and \"\" sequences\n",
          start: %Location{line: 5, column: 6},
          end: %Location{line: 8, column: 3}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 8, column: 4},
          end: %Location{line: 8, column: 4}
        }
      ],
      "C29" => [
        %Token{
          kind: :WORD,
          text: "empty_dict",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 2, column: 2}
        }
      ],
      "C30" => [
        %Token{
          kind: :WORD,
          text: "empty_list",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 1, column: 11},
          end: %Location{line: 1, column: 11}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 1, column: 13},
          end: %Location{line: 1, column: 13}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 14},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 2},
          end: %Location{line: 2, column: 2}
        }
      ],
      "D01" => [
        %Token{
          kind: :NEWLINE,
          text: "# a plain scalar value is not legal",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :INTEGER,
          text: "123",
          value: 123,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 3}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 4},
          end: %Location{line: 2, column: 4}
        }
      ],
      "D02" => [
        %Token{
          kind: :NEWLINE,
          text: "# nor is a list (unless using the 'container' rule)",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LBRACK,
          text: "[",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :INTEGER,
          text: "123",
          value: 123,
          start: %Location{line: 2, column: 3},
          end: %Location{line: 2, column: 5}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 6},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :STRING,
          text: "'abc'",
          value: "abc",
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :RBRACK,
          text: "]",
          value: nil,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 14}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 15},
          end: %Location{line: 2, column: 15}
        }
      ],
      "D03" => [
        %Token{
          kind: :NEWLINE,
          text: "# nor is a mapping (unless using the 'container' rule)",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :LCURLY,
          text: "{",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 1}
        },
        %Token{
          kind: :WORD,
          text: "a",
          value: nil,
          start: %Location{line: 2, column: 3},
          end: %Location{line: 2, column: 3}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 5},
          end: %Location{line: 2, column: 5}
        },
        %Token{
          kind: :INTEGER,
          text: "7",
          value: 7,
          start: %Location{line: 2, column: 7},
          end: %Location{line: 2, column: 7}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 8},
          end: %Location{line: 2, column: 8}
        },
        %Token{
          kind: :WORD,
          text: "b",
          value: nil,
          start: %Location{line: 2, column: 10},
          end: %Location{line: 2, column: 10}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 12},
          end: %Location{line: 2, column: 12}
        },
        %Token{
          kind: :FLOAT,
          text: "1.3",
          value: 1.3,
          start: %Location{line: 2, column: 14},
          end: %Location{line: 2, column: 16}
        },
        %Token{
          kind: :COMMA,
          text: ",",
          value: nil,
          start: %Location{line: 2, column: 17},
          end: %Location{line: 2, column: 17}
        },
        %Token{
          kind: :WORD,
          text: "c",
          value: nil,
          start: %Location{line: 2, column: 19},
          end: %Location{line: 2, column: 19}
        },
        %Token{
          kind: :COLON,
          text: ":",
          value: nil,
          start: %Location{line: 2, column: 21},
          end: %Location{line: 2, column: 21}
        },
        %Token{
          kind: :STRING,
          text: "'test'",
          value: "test",
          start: %Location{line: 2, column: 23},
          end: %Location{line: 2, column: 28}
        },
        %Token{
          kind: :RCURLY,
          text: "}",
          value: nil,
          start: %Location{line: 2, column: 30},
          end: %Location{line: 2, column: 30}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 2, column: 31},
          end: %Location{line: 2, column: 31}
        }
      ],
      "D09" => [
        %Token{
          kind: :WORD,
          text: "Gubbins",
          value: nil,
          start: %Location{line: 1, column: 1},
          end: %Location{line: 1, column: 7}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 1, column: 8},
          end: %Location{line: 2, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "at",
          value: nil,
          start: %Location{line: 2, column: 1},
          end: %Location{line: 2, column: 2}
        },
        %Token{
          kind: :WORD,
          text: "the",
          value: nil,
          start: %Location{line: 2, column: 4},
          end: %Location{line: 2, column: 6}
        },
        %Token{
          kind: :NEWLINE,
          text: "\n",
          value: nil,
          start: %Location{line: 2, column: 7},
          end: %Location{line: 3, column: 0}
        },
        %Token{
          kind: :WORD,
          text: "end",
          value: nil,
          start: %Location{line: 3, column: 1},
          end: %Location{line: 3, column: 3}
        },
        %Token{
          kind: :EOF,
          text: "",
          value: nil,
          start: %Location{line: 3, column: 4},
          end: %Location{line: 3, column: 4}
        }
      ]
    }

    # For each element in expected, compare tokens
    Enum.each(expected, fn {key, tlist} ->
      source = Map.get(cases, key)
      # Logger.debug("Processing #{key}:#{source}!")
      assert !is_nil(source)
      source = String.slice(source, 0..-2)
      tokenizer = Tokenizer.from_source(source)

      Enum.map(tlist, fn et ->
        {:ok, t} = Tokenizer.get_token(tokenizer)

        if t != et do
          Logger.debug("Failed for #{key}:\nt: #{t}\net: #{et}")
        end

        assert t == et
      end)
    end)
  end
end

defmodule ParserTest do
  @moduledoc false
  use ExUnit.Case
  alias ComplexNum.Cartesian, as: Complex

  alias CFG.{
    Parser,
    Token,
    Location,
    BinaryNode,
    UnaryNode,
    SliceNode,
    MappingNode,
    ListNode,
    RecognizerError
  }

  import Utils
  require Logger

  defp _L(l, c) do
    Location.new(l, c)
  end

  test "values" do
    cases = [
      ["1", %Token{kind: :INTEGER, text: "1", value: 1, start: _L(1, 1), end: _L(1, 1)}],
      [
        "1.0",
        %Token{kind: :FLOAT, text: "1.0", value: 1, start: _L(1, 1), end: _L(1, 3)}
      ],
      [
        "2j",
        %Token{
          kind: :COMPLEX,
          text: "2j",
          value: Complex.new(0, 2),
          start: _L(1, 1),
          end: _L(1, 2)
        }
      ],
      [
        "true",
        %Token{
          kind: :TRUE,
          text: "true",
          value: true,
          start: _L(1, 1),
          end: _L(1, 4)
        }
      ],
      [
        "false",
        %Token{
          kind: :FALSE,
          text: "false",
          value: false,
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "null",
        %Token{kind: :NONE, text: "null", value: nil, start: _L(1, 1), end: _L(1, 4)}
      ],
      [
        "foo",
        %Token{kind: :WORD, text: "foo", value: nil, start: _L(1, 1), end: _L(1, 3)}
      ],
      [
        "`foo`",
        %Token{
          kind: :BACKTICK,
          text: "`foo`",
          value: "foo",
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "'abc'",
        %Token{
          kind: :STRING,
          text: "'abc'",
          value: "abc",
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "'abc'\"def\"",
        %Token{
          kind: :STRING,
          text: "'abc'\"def\"",
          value: "abcdef",
          start: _L(1, 1),
          end: _L(1, 10)
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      {:ok, p} = Parser.from_source(source)
      {:ok, v} = Parser.value(p)
      assert v == ev
    end)
  end

  test "atoms" do
    cases = [
      ["1", %Token{kind: :INTEGER, text: "1", value: 1, start: _L(1, 1), end: _L(1, 1)}],
      [
        "1.0",
        %Token{kind: :FLOAT, text: "1.0", value: 1, start: _L(1, 1), end: _L(1, 3)}
      ],
      [
        "2j",
        %Token{
          kind: :COMPLEX,
          text: "2j",
          value: Complex.new(0, 2),
          start: _L(1, 1),
          end: _L(1, 2)
        }
      ],
      [
        "true",
        %Token{
          kind: :TRUE,
          text: "true",
          value: true,
          start: _L(1, 1),
          end: _L(1, 4)
        }
      ],
      [
        "false",
        %Token{
          kind: :FALSE,
          text: "false",
          value: false,
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "null",
        %Token{kind: :NONE, text: "null", value: nil, start: _L(1, 1), end: _L(1, 4)}
      ],
      [
        "foo",
        %Token{kind: :WORD, text: "foo", value: nil, start: _L(1, 1), end: _L(1, 3)}
      ],
      [
        "`foo`",
        %Token{
          kind: :BACKTICK,
          text: "`foo`",
          value: "foo",
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "'abc'",
        %Token{
          kind: :STRING,
          text: "'abc'",
          value: "abc",
          start: _L(1, 1),
          end: _L(1, 5)
        }
      ],
      [
        "'abc'\"def\"",
        %Token{
          kind: :STRING,
          text: "'abc'\"def\"",
          value: "abcdef",
          start: _L(1, 1),
          end: _L(1, 10)
        }
      ],
      [
        "[a]",
        %ListNode{
          elements: [
            %Token{
              end: _L(1, 2),
              kind: :WORD,
              start: _L(1, 2),
              text: "a",
              value: nil
            }
          ],
          kind: :LBRACK,
          start: _L(1, 2)
        }
      ],
      [
        "[a, b]",
        %ListNode{
          elements: [
            %Token{
              end: _L(1, 2),
              kind: :WORD,
              start: _L(1, 2),
              text: "a",
              value: nil
            },
            %Token{
              end: _L(1, 5),
              kind: :WORD,
              start: _L(1, 5),
              text: "b",
              value: nil
            }
          ],
          kind: :LBRACK,
          start: _L(1, 2)
        }
      ],
      [
        "[a\nb]",
        %ListNode{
          elements: [
            %Token{
              end: _L(1, 2),
              kind: :WORD,
              start: _L(1, 2),
              text: "a",
              value: nil
            },
            %Token{
              end: _L(2, 1),
              kind: :WORD,
              start: _L(2, 1),
              text: "b",
              value: nil
            }
          ],
          kind: :LBRACK,
          start: _L(1, 2)
        }
      ],
      [
        "{a:1}",
        %MappingNode{
          elements: [
            {%Token{
               end: _L(1, 2),
               kind: :WORD,
               start: _L(1, 2),
               text: "a",
               value: nil
             },
             %Token{
               end: _L(1, 4),
               kind: :INTEGER,
               start: _L(1, 4),
               text: "1",
               value: 1
             }}
          ],
          kind: :LCURLY,
          start: _L(1, 2)
        }
      ],
      [
        "{a:1, b:2}",
        %MappingNode{
          elements: [
            {%Token{
               end: _L(1, 2),
               kind: :WORD,
               start: _L(1, 2),
               text: "a",
               value: nil
             },
             %Token{
               end: _L(1, 4),
               kind: :INTEGER,
               start: _L(1, 4),
               text: "1",
               value: 1
             }},
            {%Token{
               end: _L(1, 7),
               kind: :WORD,
               start: _L(1, 7),
               text: "b",
               value: nil
             },
             %Token{
               end: _L(1, 9),
               kind: :INTEGER,
               start: _L(1, 9),
               text: "2",
               value: 2
             }}
          ],
          kind: :LCURLY,
          start: _L(1, 2)
        }
      ],
      # Error cases
      [
        "[",
        %RecognizerError{
          detail: {:RBRACK, :EOF},
          location: _L(1, 2),
          reason: :unexpected_token
        }
      ],
      [
        "{",
        %RecognizerError{
          detail: {:RCURLY, :EOF},
          location: _L(1, 2),
          reason: :unexpected_token
        }
      ],
      [
        "{a",
        %RecognizerError{
          detail: :EOF,
          location: _L(1, 3),
          reason: :bad_key_value_separator
        }
      ],
      [
        "{a:",
        %RecognizerError{
          detail: :EOF,
          location: _L(1, 4),
          reason: :unexpected_token_for_expression
        }
      ],
      [
        "{a:1",
        %RecognizerError{
          detail: {:RCURLY, :EOF},
          location: _L(1, 5),
          reason: :unexpected_token
        }
      ],
      [
        "{a:1,",
        %RecognizerError{
          detail: {:RCURLY, :EOF},
          location: _L(1, 6),
          reason: :unexpected_token
        }
      ],
      [
        "{a:1,b",
        %RecognizerError{
          detail: :EOF,
          location: _L(1, 7),
          reason: :bad_key_value_separator
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      {:ok, p} = Parser.from_source(source)

      case Parser.atom(p) do
        {:ok, e} ->
          assert e == ev

        {:error, v} ->
          assert v == ev
      end
    end)
  end

  test "primaries" do
    cases = [
      [
        "a",
        %Token{
          end: _L(1, 1),
          kind: :WORD,
          start: _L(1, 1),
          text: "a",
          value: nil
        }
      ],
      [
        "a.b",
        %BinaryNode{
          kind: :DOT,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %Token{
            end: _L(1, 3),
            kind: :WORD,
            start: _L(1, 3),
            text: "b",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[0]",
        %BinaryNode{
          kind: :LBRACK,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %Token{
            end: _L(1, 3),
            kind: :INTEGER,
            start: _L(1, 3),
            text: "0",
            value: 0
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[:2]",
        %BinaryNode{
          kind: :COLON,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %SliceNode{
            start_index: nil,
            step: nil,
            stop_index: %Token{
              end: _L(1, 4),
              kind: :INTEGER,
              start: _L(1, 4),
              text: "2",
              value: 2
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[::2]",
        %BinaryNode{
          kind: :COLON,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %CFG.SliceNode{
            start_index: nil,
            step: %CFG.Token{
              end: _L(1, 5),
              kind: :INTEGER,
              start: _L(1, 5),
              text: "2",
              value: 2
            },
            stop_index: nil,
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[1:10:2]",
        %BinaryNode{
          kind: :COLON,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %CFG.SliceNode{
            start_index: %Token{
              end: _L(1, 3),
              kind: :INTEGER,
              start: _L(1, 3),
              text: "1",
              value: 1
            },
            step: %Token{
              end: _L(1, 8),
              kind: :INTEGER,
              start: _L(1, 8),
              text: "2",
              value: 2
            },
            stop_index: %Token{
              end: _L(1, 6),
              kind: :INTEGER,
              start: _L(1, 5),
              text: "10",
              value: 10
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[2:]",
        %BinaryNode{
          kind: :COLON,
          lhs: %Token{
            end: _L(1, 1),
            kind: :WORD,
            start: _L(1, 1),
            text: "a",
            value: nil
          },
          rhs: %SliceNode{
            start_index: %Token{
              end: _L(1, 3),
              kind: :INTEGER,
              start: _L(1, 3),
              text: "2",
              value: 2
            },
            step: nil,
            stop_index: nil,
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a[:-1:-1]",
        %BinaryNode{
          kind: :COLON,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %SliceNode{
            start_index: nil,
            step: %Token{
              end: _L(1, 8),
              kind: :INTEGER,
              start: _L(1, 7),
              text: "-1",
              value: -1
            },
            stop_index: %Token{
              end: _L(1, 5),
              kind: :INTEGER,
              start: _L(1, 4),
              text: "-1",
              value: -1
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      # Error cases
      [
        "a[1:10:2",
        %RecognizerError{
          detail: {:RBRACK, :EOF},
          location: _L(1, 9),
          reason: :unexpected_token
        }
      ],
      [
        "a[",
        %RecognizerError{
          detail: {:RBRACK, :EOF},
          location: _L(1, 3),
          reason: :unexpected_token
        }
      ],
      ["a[]", %RecognizerError{detail: 0, location: _L(1, 3), reason: :invalid_index}]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      {:ok, p} = Parser.from_source(source)

      case Parser.primary(p) do
        {:ok, v} ->
          assert v == ev

        {:error, e} ->
          assert e == ev
      end
    end)
  end

  test "unaries" do
    cases = [
      ["a", %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil}],
      [
        "-a",
        %UnaryNode{
          kind: :MINUS,
          operand: %Token{
            end: _L(1, 2),
            kind: :WORD,
            start: _L(1, 2),
            text: "a",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "+a",
        %UnaryNode{
          kind: :PLUS,
          operand: %Token{
            end: _L(1, 2),
            kind: :WORD,
            start: _L(1, 2),
            text: "a",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "@a",
        %UnaryNode{
          kind: :AT,
          operand: %Token{
            end: _L(1, 2),
            kind: :WORD,
            start: _L(1, 2),
            text: "a",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "~a",
        %UnaryNode{
          kind: :BITNOT,
          operand: %Token{
            end: _L(1, 2),
            kind: :WORD,
            start: _L(1, 2),
            text: "a",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "--a",
        %UnaryNode{
          kind: :MINUS,
          operand: %UnaryNode{
            kind: :MINUS,
            operand: %Token{
              end: _L(1, 3),
              kind: :WORD,
              start: _L(1, 3),
              text: "a",
              value: nil
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "-a**b",
        %UnaryNode{
          kind: :MINUS,
          operand: %BinaryNode{
            kind: :POWER,
            lhs: %Token{
              end: _L(1, 2),
              kind: :WORD,
              start: _L(1, 2),
              text: "a",
              value: nil
            },
            rhs: %Token{
              end: _L(1, 5),
              kind: :WORD,
              start: _L(1, 5),
              text: "b",
              value: nil
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a**b**c",
        %BinaryNode{
          kind: :POWER,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %BinaryNode{
            kind: :POWER,
            lhs: %Token{
              end: _L(1, 4),
              kind: :WORD,
              start: _L(1, 4),
              text: "b",
              value: nil
            },
            rhs: %Token{
              end: _L(1, 7),
              kind: :WORD,
              start: _L(1, 7),
              text: "c",
              value: nil
            },
            start: _L(1, 4)
          },
          start: _L(1, 1)
        }
      ],
      [
        "-a**b**c",
        %UnaryNode{
          kind: :MINUS,
          operand: %BinaryNode{
            kind: :POWER,
            lhs: %Token{
              end: _L(1, 2),
              kind: :WORD,
              start: _L(1, 2),
              text: "a",
              value: nil
            },
            rhs: %BinaryNode{
              kind: :POWER,
              lhs: %Token{
                end: _L(1, 5),
                kind: :WORD,
                start: _L(1, 5),
                text: "b",
                value: nil
              },
              rhs: %Token{
                end: _L(1, 8),
                kind: :WORD,
                start: _L(1, 8),
                text: "c",
                value: nil
              },
              start: _L(1, 5)
            },
            start: _L(1, 2)
          },
          start: _L(1, 1)
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      {:ok, p} = Parser.from_source(source)

      case Parser.unary_expr(p) do
        {:ok, v} ->
          assert v == ev

        {:error, e} ->
          assert e == ev
      end
    end)
  end

  test "expressions" do
    cases = [
      [
        "a + b",
        %BinaryNode{
          kind: :PLUS,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{
            end: _L(1, 5),
            kind: :WORD,
            start: _L(1, 5),
            text: "b",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "a - b",
        %BinaryNode{
          kind: :MINUS,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{
            end: _L(1, 5),
            kind: :WORD,
            start: _L(1, 5),
            text: "b",
            value: nil
          },
          start: _L(1, 1)
        }
      ],
      [
        "a + b - c",
        %BinaryNode{
          kind: :MINUS,
          lhs: %BinaryNode{
            kind: :PLUS,
            lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
            rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
            start: _L(1, 1)
          },
          rhs: %Token{end: _L(1, 9), kind: :WORD, start: _L(1, 9), text: "c", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a * b",
        %BinaryNode{
          kind: :STAR,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a / b",
        %BinaryNode{
          kind: :SLASH,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a % b",
        %BinaryNode{
          kind: :MODULO,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a // b",
        %BinaryNode{
          kind: :SLASHSLASH,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a * b + c",
        %BinaryNode{
          kind: :PLUS,
          lhs: %BinaryNode{
            kind: :STAR,
            lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
            rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
            start: _L(1, 1)
          },
          rhs: %Token{end: _L(1, 9), kind: :WORD, start: _L(1, 9), text: "c", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a < b",
        %BinaryNode{
          kind: :LT,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a > b",
        %BinaryNode{
          kind: :GT,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a <= b",
        %BinaryNode{
          kind: :LE,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a >= b",
        %BinaryNode{
          kind: :GE,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a == b",
        %BinaryNode{
          kind: :EQ,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a != b",
        %BinaryNode{
          kind: :NEQ,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a <> b",
        %BinaryNode{
          kind: :ALT_NEQ,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a is b",
        %BinaryNode{
          kind: :IS,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a in b",
        %BinaryNode{
          kind: :IN,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a is not b",
        %BinaryNode{
          kind: :ISNOT,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 10), kind: :WORD, start: _L(1, 10), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a not in b",
        %BinaryNode{
          kind: :NOTIN,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 10), kind: :WORD, start: _L(1, 10), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "not a",
        %UnaryNode{
          kind: :NOT,
          operand: %Token{end: _L(1, 5), kind: :WORD, start: _L(1, 5), text: "a", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "not not a",
        %UnaryNode{
          kind: :NOT,
          operand: %UnaryNode{
            kind: :NOT,
            operand: %Token{end: _L(1, 9), kind: :WORD, start: _L(1, 9), text: "a", value: nil},
            start: _L(1, 5)
          },
          start: _L(1, 1)
        }
      ],
      [
        "a and b",
        %BinaryNode{
          kind: :AND,
          lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
          rhs: %Token{end: _L(1, 7), kind: :WORD, start: _L(1, 7), text: "b", value: nil},
          start: _L(1, 1)
        }
      ],
      [
        "a and b or not c",
        %BinaryNode{
          kind: :OR,
          lhs: %BinaryNode{
            kind: :AND,
            lhs: %Token{end: _L(1, 1), kind: :WORD, start: _L(1, 1), text: "a", value: nil},
            rhs: %Token{end: _L(1, 7), kind: :WORD, start: _L(1, 7), text: "b", value: nil},
            start: _L(1, 1)
          },
          rhs: %UnaryNode{
            kind: :NOT,
            operand: %Token{end: _L(1, 16), kind: :WORD, start: _L(1, 16), text: "c", value: nil},
            start: _L(1, 12)
          },
          start: _L(1, 1)
        }
      ],
      [
        "(a + b) - (c + d) * (e + f)",
        %CFG.BinaryNode{
          kind: :MINUS,
          lhs: %CFG.BinaryNode{
            kind: :PLUS,
            lhs: %CFG.Token{end: _L(1, 2), kind: :WORD, start: _L(1, 2), text: "a", value: nil},
            rhs: %CFG.Token{end: _L(1, 6), kind: :WORD, start: _L(1, 6), text: "b", value: nil},
            start: _L(1, 2)
          },
          rhs: %CFG.BinaryNode{
            kind: :STAR,
            lhs: %CFG.BinaryNode{
              kind: :PLUS,
              lhs: %CFG.Token{
                end: _L(1, 12),
                kind: :WORD,
                start: _L(1, 12),
                text: "c",
                value: nil
              },
              rhs: %CFG.Token{
                end: _L(1, 16),
                kind: :WORD,
                start: _L(1, 16),
                text: "d",
                value: nil
              },
              start: _L(1, 12)
            },
            rhs: %CFG.BinaryNode{
              kind: :PLUS,
              lhs: %CFG.Token{
                end: _L(1, 22),
                kind: :WORD,
                start: _L(1, 22),
                text: "e",
                value: nil
              },
              rhs: %CFG.Token{
                end: _L(1, 26),
                kind: :WORD,
                start: _L(1, 26),
                text: "f",
                value: nil
              },
              start: _L(1, 22)
            },
            start: _L(1, 11)
          },
          start: _L(1, 1)
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      {:ok, p} = Parser.from_source(source)

      case Parser.expression(p) do
        {:ok, v} ->
          assert v == ev

        {:error, e} ->
          assert e == ev
      end
    end)
  end

  test "data" do
    {:ok, cases} = TokenizerTest.load_data("testdata.txt")

    Enum.each(cases, fn {key, source} ->
      source = String.slice(source, 0..-2)
      {:ok, p} = Parser.from_source(source)
      v = Parser.mapping_body(p)

      if key < "D01" do
        case v do
          {:error, e} -> Logger.error("Failed for #{key}: #{inspect(e)}")
          _ -> {}
        end
      else
        case v do
          {:error, _} -> {}
          _ -> Logger.error("Unexpected success for #{key}")
        end
      end
    end)
  end

  test "json" do
    p = data_path("forms.conf")
    {:ok, p} = Parser.from_file(p)
    {:ok, mn = %MappingNode{}} = Parser.mapping(p)
    expected = MapSet.new(["refs", "fieldsets", "forms", "modals", "pages"])

    Enum.each(mn.elements, fn item ->
      {k, _v} = item
      assert MapSet.member?(expected, k.value)
    end)
  end
end

defmodule ConfigTest do
  @moduledoc false
  use ExUnit.Case
  use Bitwise
  import Utils
  alias ComplexNum.Cartesian, as: Complex

  alias CFG.{
    # Parser,
    Token,
    Location,
    # BinaryNode,
    UnaryNode,
    SliceNode,
    # MappingNode,
    # ListNode,
    RecognizerError,
    Config
  }

  require Logger

  defp _L(l, c) do
    Location.new(l, c)
  end

  test "files" do
    {:ok, files} = File.ls(data_path("derived"))

    not_mappings =
      MapSet.new([
        "data.cfg",
        "incl_list.cfg",
        "pages.cfg",
        "routes.cfg"
      ])

    Enum.each(files, fn fname ->
      p = Path.join("derived", fname)
      v = Config.from_file(data_path(p))

      case v do
        {:error, e} ->
          case e do
            %RecognizerError{reason: :must_be_mapping} ->
              assert MapSet.member?(not_mappings, fname)

            %RecognizerError{reason: :duplicate_key} ->
              assert fname == "dupes.cfg"

            _ ->
              Logger.debug("Unexpected error: #{inspect(e)}")
          end

        _ ->
          assert fname != "dupes.cfg" && !MapSet.member?(not_mappings, fname)
      end
    end)
  end

  test "identifiers" do
    cases = [
      ["foo", true],
      ["\u0935\u092e\u0938", true],
      ["\u73b0\u4ee3\u6c49\u8bed\u5e38\u7528\u5b57\u8868", true],
      ["foo ", false],
      ["foo[", false],
      ["foo [", false],
      ["foo.", false],
      ["foo .", false],
      ["\u0935\u092e\u0938.", false],
      ["\u73b0\u4ee3\u6c49\u8bed\u5e38\u7528\u5b57\u8868.", false],
      ["9", false],
      ["9foo", false],
      ["hyphenated-key", false]
    ]

    Enum.each(cases, fn case ->
      [source, expected] = case
      actual = Config.is_identifier(source)

      if actual != expected do
        Logger.debug("Failed for #{source}")
      end

      assert actual == expected
    end)
  end

  test "paths" do
    cases = [
      [
        "foo[1:2]",
        [
          [:DOT, %Token{end: _L(1, 3), kind: :WORD, start: _L(1, 1), text: "foo", value: nil}],
          [
            :COLON,
            %CFG.SliceNode{
              start_index: %Token{
                end: _L(1, 5),
                kind: :INTEGER,
                start: _L(1, 5),
                text: "1",
                value: 1
              },
              step: nil,
              stop_index: %Token{
                end: _L(1, 7),
                kind: :INTEGER,
                start: _L(1, 7),
                text: "2",
                value: 2
              },
              start: _L(1, 4)
            }
          ]
        ]
      ],
      [
        "foo[-bar].baz.bozz[3].fizz[:3].futz['foo']",
        [
          [:DOT, %Token{end: _L(1, 3), kind: :WORD, start: _L(1, 1), text: "foo", value: nil}],
          [
            :LBRACK,
            %UnaryNode{
              kind: :MINUS,
              operand: %Token{
                end: _L(1, 8),
                kind: :WORD,
                start: _L(1, 6),
                text: "bar",
                value: nil
              },
              start: _L(1, 5)
            }
          ],
          [:DOT, %Token{end: _L(1, 13), kind: :WORD, start: _L(1, 11), text: "baz", value: nil}],
          [:DOT, %Token{end: _L(1, 18), kind: :WORD, start: _L(1, 15), text: "bozz", value: nil}],
          [
            :LBRACK,
            %Token{end: _L(1, 20), kind: :INTEGER, start: _L(1, 20), text: "3", value: 3}
          ],
          [:DOT, %Token{end: _L(1, 26), kind: :WORD, start: _L(1, 23), text: "fizz", value: nil}],
          [
            :COLON,
            %SliceNode{
              start_index: nil,
              step: nil,
              stop_index: %Token{
                end: _L(1, 29),
                kind: :INTEGER,
                start: _L(1, 29),
                text: "3",
                value: 3
              },
              start: _L(1, 27)
            }
          ],
          [:DOT, %Token{end: _L(1, 35), kind: :WORD, start: _L(1, 32), text: "futz", value: nil}],
          [
            :LBRACK,
            %Token{end: _L(1, 41), kind: :STRING, start: _L(1, 37), text: "'foo'", value: "foo"}
          ]
        ]
      ],
      # bad paths
      ["foo[1, 2]", %RecognizerError{detail: 2, location: _L(1, 5), reason: :invalid_index}],
      [
        "foo[1] bar",
        %RecognizerError{detail: "foo[1] bar", location: _L(1, 8), reason: :invalid_path_extra}
      ],
      [
        "foo.123",
        %RecognizerError{detail: "foo.123", location: _L(1, 4), reason: :invalid_path_extra}
      ],
      [
        "foo.",
        %RecognizerError{detail: {:WORD, :EOF}, location: _L(1, 5), reason: :unexpected_token}
      ],
      ["foo[]", %RecognizerError{detail: 0, location: _L(1, 5), reason: :invalid_index}],
      ["foo[1a]", %RecognizerError{detail: "1a", location: _L(1, 6), reason: :bad_number}],
      ["4", %RecognizerError{detail: "4", location: _L(1, 1), reason: :invalid_path}]
    ]

    Enum.each(cases, fn case ->
      [source, ev] = case
      v = Config.parse_path(source)

      case v do
        {:error, e} ->
          assert e == ev

        {:ok, node} ->
          path = Config.unpack_path(node)
          assert path == ev
      end
    end)
  end

  defp make_datetime(s, offset) do
    {:ok, ndt} = NaiveDateTime.from_iso8601(s)
    {:ok, dt} = DateTime.from_naive(ndt, "Etc/UTC")

    if offset == 0 do
      dt
    else
      %{dt | utc_offset: offset}
    end
  end

  #
  # This is here because it seems you can't do a partial match of a struct s with an expected value ev
  # e.g. ^ev = Map.from_struct(s)
  #
  defp match_error(ev, s) when is_map(ev) do
    m = Map.from_struct(s)

    Enum.each(ev, fn {k, v} ->
      assert Map.has_key?(m, k)
      assert v == Map.get(m, k)
    end)
  end

  test "main config" do
    p = data_path(Path.join("derived", "main.cfg"))
    {:ok, cfg} = Config.new()
    Config.add_include(cfg, [data_path("base")])
    {:ok, _} = Config.load_file(cfg, p)
    {:ok, lcfg} = Config.get(cfg, "logging")
    assert is_pid(lcfg)
    %Config{} = Agent.get(lcfg, fn state -> state end)
    v = Config.as_dict(lcfg)

    case v do
      {:error, e} ->
        Logger.error("#{__ENV__.line}: #{inspect(e)})")

      {:ok, d} ->
        assert d == %{
                 "formatters" => %{
                   "brief" => %{
                     "class" => "logging.Formatter",
                     "format" => "%(name)20.20s %(lineno)4d %(message)s"
                   }
                 },
                 "handlers" => %{
                   "debug" => %{
                     "class" => "logging.FileHandler",
                     "encoding" => "utf-8",
                     "filename" => "run/server-debug.log",
                     "formatter" => "brief",
                     "level" => "DEBUG",
                     "mode" => "w"
                   },
                   "error" => %{
                     "class" => "logging.FileHandler",
                     "encoding" => "utf-8",
                     "filename" => "run/server-errors.log",
                     "formatter" => "brief",
                     "level" => "ERROR",
                     "mode" => "w"
                   },
                   "file" => %{
                     "class" => "logging.FileHandler",
                     "encoding" => "utf-8",
                     "filename" => "run/server.log",
                     "formatter" => "brief",
                     "level" => "INFO",
                     "mode" => "w"
                   }
                 },
                 "loggers" => %{},
                 "root" => %{"handlers" => {"file", "error", "debug"}, "level" => "DEBUG"}
               }
    end

    v = Config.get(lcfg, ~s("handlers.file/filename))
    {:error, %RecognizerError{reason: :unterminated_string}} = v

    cases = [
      ["foo", "bar"],
      ["foo.bar", "baz"],
      ["handler.debug.levl", "bozz"]
    ]

    Enum.each(cases, fn case ->
      [key, dv] = case
      {:ok, rv} = Config.get(lcfg, key, dv)
      assert dv == rv
    end)

    cases = [
      ["handlers.file.filename", "run/server.log"],
      ["handlers.debug.filename", "run/server-debug.log"],
      ["root.handlers", {"file", "error", "debug"}],
      ["root.handlers[2]", "debug"],
      ["root.handlers[:2]", {"file", "error"}],
      ["root.handlers[::2]", {"file", "debug"}],
      ["root.handlers[:]", {"file", "error", "debug"}],
      ["root.handlers[::]", {"file", "error", "debug"}]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      {:ok, v} = Config.get(lcfg, key)
      assert v == ev
    end)

    {:ok, tcfg} = Config.get(cfg, "test")
    assert is_pid(tcfg)
    %Config{} = Agent.get(tcfg, fn state -> state end)

    cases = [
      ["float", 1.0e-7],
      ["float2", 0.3],
      ["float3", 3.0],
      ["list[1]", 2],
      ["dict.a", "b"],
      ["date_time", make_datetime("2019-03-28T23:27:04.314159", 19800)],
      ["neg_offset_time", make_datetime("2019-03-28T23:27:04.314159", -19800)],
      ["alt_date_time", make_datetime("2019-03-28 23:27:04.271828", 0)],
      ["no_ms_time", make_datetime("2019-03-28 23:27:04.000000", 0)],
      ["computed", 3.3],
      ["computed2", 2.7],
      ["computed3", 0.9],
      ["computed4", 10]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      {:ok, v} = Config.get(tcfg, key)

      if key != "computed3" do
        assert v == ev
      else
        assert_in_delta v, ev, 1.0e-7
      end
    end)

    {:ok, bcfg} = Config.get(cfg, "base")
    assert is_pid(bcfg)
    %Config{} = Agent.get(bcfg, fn state -> state end)

    v = Config.get(cfg, "combined_list")

    {:ok,
     {"derived_foo", "derived_bar", "derived_baz", "test_foo", "test_bar", "test_baz", "base_foo",
      "base_bar", "base_baz"}} = v

    v = Config.get(cfg, "combined_map_1")

    {:ok,
     %{
       "foo_key" => "base_foo",
       "bar_key" => "base_bar",
       "baz_key" => "base_baz",
       "base_foo_key" => "base_foo",
       "base_bar_key" => "base_bar",
       "base_baz_key" => "base_baz",
       "derived_foo_key" => "derived_foo",
       "derived_bar_key" => "derived_bar",
       "derived_baz_key" => "derived_baz",
       "test_foo_key" => "test_foo",
       "test_bar_key" => "test_bar",
       "test_baz_key" => "test_baz"
     }} = v

    v = Config.get(cfg, "combined_map_2")

    {:ok,
     %{
       "derived_foo_key" => "derived_foo",
       "derived_bar_key" => "derived_bar",
       "derived_baz_key" => "derived_baz"
     }} = v

    {:ok, n1} = Config.get(cfg, "number_1")
    {:ok, n2} = Config.get(cfg, "number_2")
    {:ok, n3} = Config.get(cfg, "number_3")
    {:ok, n4} = Config.get(cfg, "number_4")

    assert (n1 &&& n2) == n3
    assert bxor(n1, n2) == n4

    cases = [
      ["logging[4]", %{location: _L(1, 9), reason: :invalid_container}],
      ["logging[:4]", %{location: _L(1, 8), reason: :invalid_container}],
      [
        "no_such_key",
        %{detail: "no_such_key", location: _L(1, 1), reason: :not_found}
      ]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      v = Config.get(cfg, key)

      case v do
        {:error, e} ->
          match_error(ev, e)

        {:ok, v} ->
          assert v == ev
      end
    end)
  end

  test "example config" do
    p = data_path(Path.join("derived", "example.cfg"))
    {:ok, cfg} = Config.new()
    Config.add_include(cfg, [data_path("base")])
    {:ok, _} = Config.load_file(cfg, p)
    {:ok, v1} = Config.get(cfg, "snowman_escaped")
    {:ok, v2} = Config.get(cfg, "snowman_unescaped")
    assert v1 == v2
    assert v1 == "☃"
    {:ok, v1} = Config.get(cfg, "face_with_tears_of_joy")
    {:ok, v2} = Config.get(cfg, "unescaped_face_with_tears_of_joy")
    assert v1 == v2
    assert v1 == "😂"
    {:ok, strings} = Config.get(cfg, "strings")

    case :os.type() do
      {:unix, _} ->
        assert strings ==
                 {"Oscar Fingal O'Flahertie Wills Wilde", "size: 5\"",
                  "Triple quoted form\ncan span\n'multiple' lines",
                  "with \"either\"\nkind of 'quote' embedded within"}

      {:win32, _} ->
        assert strings ==
                 {"Oscar Fingal O'Flahertie Wills Wilde", "size: 5\"",
                  "Triple quoted form\r\ncan span\r\n'multiple' lines",
                  "with \"either\"\r\nkind of 'quote' embedded within"}
    end

    {:ok, ospid} = Config.get(cfg, "special_value_1")
    assert is_binary(ospid)
    {:ok, hd} = Config.get(cfg, "special_value_2")

    # On Windows, try to make things uniform for the assertion
    case :os.type() do
      {:unix, _} ->
        assert hd == Path.expand("~")

      {:win32, _} ->
        assert Path.absname(hd) == Path.expand("~")
    end

    {:ok, v} = Config.get(cfg, "special_value_3")
    assert v == make_datetime("2019-03-28 23:27:04.314159", 19843)
    {:ok, v} = Config.get(cfg, "special_value_4")
    assert v == "bar"
    {:ok, v} = Config.get(cfg, "special_value_5")
    dt = DateTime.utc_now()
    u1 = DateTime.to_unix(v)
    u2 = DateTime.to_unix(dt)
    assert u2 >= u1
    assert u2 - u1 <= 1

    cases = [
      # integers
      ["decimal_integer", 123],
      ["hexadecimal_integer", 0x123],
      ["octal_integer", 0o123],
      ["binary_integer", 0b000100100011],
      # floats
      ["common_or_garden", 123.456],
      ["leading_zero_not_needed", 0.123],
      ["trailing_zero_not_needed", 123.0],
      ["scientific_large", 1.0e6],
      ["scientific_small", 1.0e-7],
      ["expression_1", 3.14159],
      # complex
      ["expression_2", Complex.new(3, 2)],
      ["list_value[4]", Complex.new(1, 3)],
      # boolean
      ["boolean_value", true],
      ["opposite_boolean_value", false],
      ["computed_boolean_1", true],
      ["computed_boolean_2", false],
      ["incl_list", {"a", "b", "c"}],
      ["incl_mapping", %{"foo" => "bar", "bar" => "baz"}],
      ["incl_mapping_body", %{"baz" => "bozz", "fizz" => "buzz"}]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case

      {:ok, v} = Config.get(cfg, key)

      v =
        if Config.is_config(v) do
          {:ok, v} = Config.as_dict(v)
          v
        else
          v
        end

      assert v == ev
    end)
  end

  test "duplicates" do
    p = data_path(Path.join("derived", "dupes.cfg"))
    {:ok, cfg} = Config.new()
    v = Config.load_file(cfg, p)
    loc = _L(4, 1)
    {:error, %RecognizerError{detail: "foo", location: ^loc, reason: :duplicate_key}} = v
    Config.set_no_duplicates(cfg, false)
    {:ok, _} = Config.load_file(cfg, p)
    {:ok, "not again!"} = Config.get(cfg, "foo")
  end

  test "context" do
    p = data_path(Path.join("derived", "context.cfg"))
    {:ok, cfg} = Config.new(%{context: %{"bozz" => "bozz-bozz"}})
    {:ok, ^cfg} = Config.load_file(cfg, p)
    {:ok, "bozz-bozz"} = Config.get(cfg, "baz")
    v = Config.get(cfg, "bad")
    loc = _L(3, 7)
    {:error, %RecognizerError{detail: "not_there", location: ^loc, reason: :unknown_variable}} = v
    # Logger.debug("#{__ENV__.line}: #{inspect v}")
  end

  test "expressions" do
    p = data_path(Path.join("derived", "test.cfg"))
    {:ok, cfg} = Config.from_file(p)
    assert p == Agent.get(cfg, fn state -> state.path end)

    cases = [
      ["dicts_added", %{"a" => "b", "c" => "d"}],
      [
        "nested_dicts_added",
        %{"a" => %{"b" => "c", "w" => "x"}, "d" => %{"e" => "f", "y" => "z"}}
      ],
      ["lists_added", {"a", 1, "b", 2}],
      ["list[:2]", {1, 2}],
      ["dicts_subtracted", %{"a" => "b"}],
      ["nested_dicts_subtracted", %{}],
      [
        "dict_with_nested_stuff",
        %{"a_list" => {1, 2, %{"a" => 3}}, "a_map" => %{"k1" => {"b", "c", %{"d" => "e"}}}}
      ],
      ["dict_with_nested_stuff.a_list[:2]", {1, 2}],
      ["unary", -4],
      ["abcdefghijkl", "mno"],
      ["power", 8],
      ["computed5", 2.5],
      ["computed6", 2],
      ["c3", Complex.new(3, 1)],
      ["c4", Complex.new(5, 5)],
      ["computed8", 2],
      ["computed9", 160],
      ["computed10", 62],
      ["dict.a", "b"],
      # second call should return the same
      ["dict.a", "b"],
      ["dicts_added.a", "b"],
      ["interp", "A-4 a test_foo true 10 1.0e-7 1 b [a, c, e, g]Z"],
      ["interp2", "{a: b}"],
      [
        "bad_include",
        %RecognizerError{detail: 4, location: _L(71, 17), reason: :string_expected}
      ],
      ["computed7", %RecognizerError{detail: "float4", location: _L(72, 16), reason: :not_found}],
      [
        "bad_interp",
        %RecognizerError{
          detail: "${computed7}",
          location: _L(85, 15),
          reason: :conversion_failure
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case

      v = Config.get(cfg, key)

      case v do
        {:error, e} ->
          assert e == ev

        {:ok, v} ->
          assert v == ev
      end
    end)
  end

  test "forms" do
    p = data_path(Path.join("derived", "forms.cfg"))
    {:ok, cfg} = Config.new()
    Config.add_include(cfg, [data_path("base")])
    {:ok, _} = Config.load_file(cfg, p)

    cases = [
      ["modals.deletion.contents[0].id", "frm-deletion"],
      [
        "refs.delivery_address_field",
        %{
          "attrs" => %{"minlength" => 10},
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Postal address",
          "label_i18n" => "postal-address",
          "message" => " ",
          "name" => "postal_address",
          "ph_i18n" => "your-postal-address",
          "placeholder" => "We need this for delivering to you",
          "required" => true,
          "short_name" => "address",
          "type" => "textarea"
        }
      ],
      [
        "refs.delivery_instructions_field",
        %{
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Delivery Instructions",
          "label_i18n" => "delivery-instructions",
          "message" => " ",
          "name" => "delivery_instructions",
          "ph_i18n" => "any-special-delivery-instructions",
          "placeholder" => "Any special delivery instructions?",
          "short_name" => "notes",
          "type" => "textarea"
        }
      ],
      [
        "refs.verify_field",
        %{
          "append" => %{"classes" => "btn-primary", "label" => "Verify", "type" => "submit"},
          "attrs" => %{"autofocus" => true, "maxlength" => 6, "minlength" => 6},
          "kind" => "field",
          "label" => "Verification code",
          "label_i18n" => "verification-code",
          "message" => " ",
          "name" => "verification_code",
          "ph_i18n" => "verification-not-backup-code",
          "placeholder" => "Your verification code (NOT a backup code)",
          "required" => true,
          "short_name" => "verification code",
          "type" => "input"
        }
      ],
      [
        "refs.signup_password_field",
        %{
          "kind" => "field",
          "label" => "Password",
          "label_i18n" => "password",
          "message" => " ",
          "name" => "password",
          "ph_i18n" => "password-wanted-on-site",
          "placeholder" => "The password you want to use on this site",
          "required" => true,
          "toggle" => true,
          "type" => "password"
        }
      ],
      [
        "refs.signup_password_conf_field",
        %{
          "kind" => "field",
          "label" => "Password confirmation",
          "label_i18n" => "password-confirmation",
          "message" => " ",
          "name" => "password_conf",
          "ph_i18n" => "same-password-again",
          "placeholder" => "The same password, again, to guard against mistyping",
          "required" => true,
          "toggle" => true,
          "type" => "password"
        }
      ],
      [
        "fieldsets.signup_ident[0].contents[0]",
        %{
          "attrs" => %{"autofocus" => true},
          "data_source" => "user.display_name",
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Your name",
          "label_i18n" => "your-name",
          "message" => " ",
          "name" => "display_name",
          "ph_i18n" => "your-full-name",
          "placeholder" => "Your full name",
          "required" => true,
          "type" => "input"
        }
      ],
      [
        "fieldsets.signup_ident[0].contents[1]",
        %{
          "data_source" => "user.familiar_name",
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Familiar name",
          "label_i18n" => "familiar-name",
          "message" => " ",
          "name" => "familiar_name",
          "ph_i18n" => "if-not-first-word",
          "placeholder" => "If not just the first word in your full name",
          "type" => "input"
        }
      ],
      [
        "fieldsets.signup_ident[1].contents[0]",
        %{
          "data_source" => "user.email",
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Email address (used to sign in)",
          "label_i18n" => "email-address",
          "message" => " ",
          "name" => "email",
          "ph_i18n" => "your-email-address",
          "placeholder" => "Your email address",
          "required" => true,
          "short_name" => "email address",
          "type" => "email"
        }
      ],
      [
        "fieldsets.signup_ident[1].contents[1]",
        %{
          "attrs" => %{"maxlength" => 10},
          "classes" => "numeric",
          "data_source" => "customer.mobile_phone",
          "grpclass" => "col-md-6",
          "kind" => "field",
          "label" => "Phone number",
          "label_i18n" => "phone-number",
          "message" => " ",
          "name" => "mobile_phone",
          "ph_i18n" => "your-phone-number",
          "placeholder" => "Your phone number",
          "prepend" => %{"icon" => "phone"},
          "required" => true,
          "short_name" => "phone number",
          "type" => "input"
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      v = Config.get(cfg, key)

      case v do
        {:error, e} ->
          assert e == ev

        {:ok, v} ->
          assert v == ev
      end
    end)
  end

  test "paths across includes" do
    p = data_path(Path.join("base", "main.cfg"))
    {:ok, cfg} = Config.from_file(p)

    cases = [
      ["logging.appenders.file.filename", "run/server.log"],
      ["logging.appenders.file.append", true],
      ["logging.appenders.error.filename", "run/server-errors.log"],
      ["logging.appenders.error.append", false],
      ["redirects.freeotp.url", "https://freeotp.github.io/"],
      ["redirects.freeotp.permanent", false]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      v = Config.get(cfg, key)

      case v do
        {:error, e} ->
          assert e == ev

        {:ok, v} ->
          assert v == ev
      end
    end)
  end

  test "sources" do
    cases = [
      "foo[::2]",
      "foo[:]",
      "foo[:2]",
      "foo[2:]",
      "foo[::1]",
      "foo[::-1]",
      "foo[3]",
      "foo"
    ]

    Enum.each(cases, fn s ->
      {:ok, node} = Config.parse_path(s)
      assert s == Config.to_source(node)
    end)
  end

  test "circular references" do
    p = data_path(Path.join("derived", "test.cfg"))
    {:ok, cfg} = Config.from_file(p)

    cases = [
      [
        "circ_list[1]",
        %{detail: [{_L(46, 7), "circ_list[1]"}], location: _L(46, 7), reason: :circular_reference}
      ],
      [
        "circ_map.a",
        %{
          detail: [
            {_L(51, 10), "circ_map.b"},
            {_L(52, 10), "circ_map.c"},
            {_L(53, 10), "circ_map.a"}
          ],
          location: _L(51, 10),
          reason: :circular_reference
        }
      ]
    ]

    Enum.each(cases, fn case ->
      [key, ev] = case
      {:error, e} = Config.get(cfg, key)
      match_error(ev, e)
    end)
  end

  test "slices and indices" do
    p = data_path(Path.join("derived", "test.cfg"))
    {:ok, cfg} = Config.from_file(p)
    the_list = {"a", "b", "c", "d", "e", "f", "g"}

    cases = [
      ["test_list[:]", the_list],
      ["test_list[::]", the_list],
      ["test_list[:20]", the_list],
      ["test_list[-20:4]", {"a", "b", "c", "d"}],
      ["test_list[-20:20]", the_list],
      ["test_list[2:]", {"c", "d", "e", "f", "g"}],
      ["test_list[-3:]", {"e", "f", "g"}],
      ["test_list[-2:2:-1]", {"f", "e", "d"}],
      ["test_list[::3]", {"a", "d", "g"}],
      ["test_list[::2]", {"a", "c", "e", "g"}],
      ["test_list[::2][::3]", {"a", "g"}],
      ["test_list[2:-2:2]", {"c", "e"}],
      ["test_list[::-1]", {"g", "f", "e", "d", "c", "b", "a"}]
    ]

    # Logger.debug("*** SENTINEL ***")

    Enum.each(cases, fn case ->
      [key, ev] = case
      # Logger.debug("#{__ENV__.line}: #{key}")

      v = Config.get(cfg, key)

      case v do
        {:error, e} ->
          assert e == ev

        {:ok, v} ->
          assert v == ev
      end
    end)
  end

  test "absolute include paths" do
    p = String.replace(Path.expand(Path.join(data_path("derived"), "test.cfg")), "\\", "/")
    s = "test: @'#{p}'"
    {:ok, cfg} = Config.from_source(s)
    {:ok, v} = Config.get(cfg, "test.computed6")
    assert v == 2
  end

  test "nested include paths" do
    d1 = data_path("base")
    d2 = data_path("derived")
    d3 = data_path("another")
    p = Path.join(d1, "top.cfg")
    {:ok, cfg} = Config.from_file(p)
    Config.add_include(cfg, [d2, d3])
    {:ok, v} = Config.get(cfg, "level1.level2.final")
    assert v == 42
  end

  test "recursive configuration" do
    p = data_path(Path.join("derived", "recurse.cfg"))
    {:ok, cfg} = Config.from_file(p)
    v = Config.get(cfg, "recurse")
    # Logger.debug("#{__ENV__.line}: #{inspect v}")
    assert v ==
             {:error,
              %RecognizerError{
                reason: :cannot_include_self,
                location: _L(1, 11),
                detail: "recurse.cfg"
              }}
  end
end
