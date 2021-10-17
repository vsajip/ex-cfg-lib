#
# Copyright (C) 2021 Vinay Sajip <vinay_sajip@yahoo.co.uk>
#
# See LICENSE file for usage rights.
#
defmodule CFG do
  @moduledoc """
  This top-level namespace holds all the functionality for working with CFG.
  You'll normally interact with configurations using the Config submodule.
  """
  alias ComplexNum.Cartesian, as: Complex

  defmodule RecognizerError do
    @moduledoc """
    This is the type used to return errors.
    """
    defexception [:location, :reason, :detail]

    @type t :: %__MODULE__{
            location: term(),
            reason: atom(),
            detail: term()
          }

    def exception(reason, location, detail),
      do: %__MODULE__{reason: reason, location: location, detail: detail}

    def message(exception = %__MODULE__{}),
      do: CFG.format_error(exception)
  end

  @doc """
  Format an exception for display.
  """
  @spec format_error(%RecognizerError{}) :: String.t()
  def format_error(exception) do
    "#{inspect(exception)}"
  end

  defmodule Location do
    # """This represents a location in the CFG source."""
    @moduledoc false

    defstruct line: 1, column: 1

    @doc """
    Return a location with the specified line and column.
    """
    @spec new(integer(), non_neg_integer()) :: %Location{}
    def new(line \\ 1, column \\ 1) do
      %Location{line: line, column: column}
    end

    @doc """
    Return the location of the start of the next line.
    """
    @spec next_line(%Location{}) :: %Location{}
    def next_line(loc) do
      %Location{line: loc.line + 1, column: 1}
    end

    @doc """
    Return the location of the next column.
    """
    @spec next_col(%Location{}) :: %Location{}
    def next_col(loc) do
      %Location{line: loc.line, column: loc.column + 1}
    end

    @doc """
    Return the location of the previous column.
    """
    @spec prev_col(%Location{}) :: %Location{}
    def prev_col(loc) do
      %Location{line: loc.line, column: loc.column - 1}
    end
  end

  defimpl Inspect, for: Location do
    def inspect(loc, _opts) do
      "_L(#{loc.line}, #{loc.column})"
    end
  end

  defimpl String.Chars, for: Location do
    def to_string(loc) do
      "(#{loc.line}, #{loc.column})"
    end
  end

  defimpl String.Chars, for: ComplexNum do
    def to_string(z) do
      if z.real == 0 do
        "#{z.imaginary}j"
      else
        "#{z.real} + #{z.imaginary}j"
      end
    end
  end

  defmodule Token do
    # """This module represents a token in the CFG language."""
    @moduledoc false

    defstruct [:kind, :text, :value, :start, :end]
  end

  defimpl String.Chars, for: Token do
    def to_string(t) do
      "<Token #{t.kind} \{#{t.text}\} [#{t.value}] #{t.start}-#{t.end}>"
    end
  end

  defmodule Tokenizer do
    # """This module contains the functionality to convert CFG source code into tokens."""
    @moduledoc false
    use Agent
    require Logger
    require Map

    defstruct [
      :stream,
      :location,
      :char_location,
      :pushed_back,
      :escapes,
      :punctuation,
      :keywords,
      :keyword_values
    ]

    def new(stream) do
      {:ok, pid} =
        Agent.start(fn ->
          %Tokenizer{
            stream: stream,
            location: Location.new(),
            char_location: Location.new(),
            pushed_back: [],
            escapes: %{
              "a" => "\a",
              "b" => "\b",
              "f" => "\f",
              "n" => "\n",
              "r" => "\r",
              "t" => "\t",
              "v" => "\v",
              "\\" => "\\",
              "'" => "'",
              "\"" => "\""
            },
            punctuation: %{
              "=" => :ASSIGN,
              ":" => :COLON,
              "-" => :MINUS,
              "+" => :PLUS,
              "*" => :STAR,
              "/" => :SLASH,
              "%" => :MODULO,
              "," => :COMMA,
              "{" => :LCURLY,
              "}" => :RCURLY,
              "[" => :LBRACK,
              "]" => :RBRACK,
              "(" => :LPAREN,
              ")" => :RPAREN,
              "@" => :AT,
              "$" => :DOLLAR,
              "<" => :LT,
              ">" => :GT,
              "!" => :NOT,
              "~" => :BITNOT,
              "&" => :BITAND,
              "|" => :BITOR,
              "^" => :BITXOR
            },
            keywords: %{
              "true" => :TRUE,
              "false" => :FALSE,
              "null" => :NONE,
              "is" => :IS,
              "in" => :IN,
              "not" => :NOT,
              "and" => :AND,
              "or" => :OR
            },
            keyword_values: %{
              :TRUE => true,
              :FALSE => false,
              :NONE => nil
            }
          }
        end)

      pid
    end

    def from_source(s) do
      {:ok, stream} = StringIO.open(s)
      new(stream)
    end

    def from_file(path) do
      {:ok, stream} = File.open(path, [:read, :utf8])
      new(stream)
    end

    defp push_back(this, c) do
      if c != :eof do
        state = Agent.get(this, fn state -> state end)
        pb = [{c, state.char_location} | state.pushed_back]
        Agent.update(this, fn state -> %{state | pushed_back: pb} end)
      end
    end

    defp get_char(this) do
      state = Agent.get(this, fn state -> state end)
      pb = state.pushed_back
      loc = state.location

      {result, cloc, read_back} =
        if Enum.empty?(pb) do
          {IO.read(state.stream, 1), loc, false}
        else
          [h | t] = pb
          Agent.update(this, fn state -> %{state | pushed_back: t} end)
          {a, b} = h
          {a, b, true}
        end

      loc =
        if read_back do
          cloc
        else
          loc
        end

      loc =
        if !is_binary(result) do
          loc
        else
          if result == "\n" do
            Location.next_line(loc)
          else
            Location.next_col(loc)
          end
        end

      if result != :eof do
        Agent.update(this, fn state -> %{state | location: loc, char_location: cloc} end)
      end

      result
    end

    defp as_string(list) do
      List.to_string(list)
    end

    defp is_digit(c) do
      is_binary(c) && String.match?(c, ~r/\d/)
    end

    defp is_alnum(c) do
      is_binary(c) && String.match?(c, ~r/[_\p{L}\p{Nd}]/u)
    end

    defp adjusted_loc(c, loc) do
      if c == :eof do
        loc
      else
        Location.prev_col(loc)
      end
    end

    defp collect_ident(this, token, start) do
      c = get_char(this)

      if c != :eof && is_alnum(c) do
        collect_ident(this, token ++ [c], start)
      else
        push_back(this, c)
        s = as_string(token)
        state = Agent.get(this, fn state -> state end)
        k = Map.get(state.keywords, s, :WORD)
        v = Map.get(state.keyword_values, k)
        eloc = adjusted_loc(c, state.char_location)
        {:ok, %Token{kind: k, text: s, value: v, start: start, end: eloc}}
      end
    end

    defp char_loc(this) do
      Agent.get(this, fn state -> state.char_location end)
    end

    defp error(reason, loc, detail) do
      {:error, RecognizerError.exception(reason, loc, detail)}
    end

    defp parse_escapes(escapes, s, pos) do
      parts = String.split(s, "\\", parts: 2)

      if length(parts) == 1 do
        {:ok, s}
      else
        [first, rest] = parts
        newpos = pos + String.length(first)
        # Logger.debug("[#{first}|#{rest}]")
        c = String.first(rest)

        if Map.has_key?(escapes, c) do
          last = parse_escapes(escapes, String.slice(rest, 1..-1), newpos)

          case last do
            {:error, _} -> last
            {:ok, pv} -> {:ok, first <> Map.get(escapes, c) <> pv}
          end
        else
          c = String.first(rest)

          if String.match?(c, ~r/[ux]/i) do
            len =
              cond do
                c == "x" || c == "X" -> 2
                c == "u" -> 4
                true -> 8
              end

            if String.length(rest) < len + 1 do
              error(:invalid_escape, newpos, rest)
            else
              hex = String.slice(rest, 1, len)

              if !String.match?(hex, ~r/^[0-9a-f]+$/i) do
                error(:invalid_escape, newpos, rest)
              else
                {esc, _} = Integer.parse(hex, 16)
                {:ok, first <> List.to_string([esc]) <> String.slice(rest, (1 + len)..-1)}
              end
            end
          else
            {:ok, first <> rest}
          end
        end
      end
    end

    defp collect_backtick(this, token, start) do
      c = get_char(this)

      cond do
        c == :eof ->
          error(:unterminated_backtick, char_loc(this), as_string(token))

        c == "\n" ->
          error(:newlines_not_allowed, char_loc(this), as_string(token))

        c == "`" ->
          state = Agent.get(this, fn state -> state end)
          s = as_string(token ++ [c])
          n = 1 + length(token)
          pe = parse_escapes(state.escapes, String.slice(s, 1, n - 2), 0)

          case pe do
            {:error, %RecognizerError{reason: r, detail: d}} ->
              error(r, start, d)

            {:ok, pv} ->
              {:ok,
               %Token{kind: :BACKTICK, text: s, value: pv, start: start, end: state.char_location}}
          end

        true ->
          collect_backtick(this, token ++ [c], start)
      end
    end

    defp collect_string(this, token, quoter, multi_line, escaped, start) do
      c = get_char(this)
      # Logger.debug("[#{token}|#{c}]")
      cond do
        c == :eof ->
          error(:unterminated_string, char_loc(this), as_string(token))

        c == "\\" ->
          collect_string(this, token ++ [c], quoter, multi_line, !escaped, start)

        c == "\n" || c == "\r" ->
          if !multi_line do
            error(:newlines_not_allowed, char_loc(this), as_string(token))
          else
            addend =
              if c == "\n" do
                ["\n"]
              else
                nc = get_char(this)

                if nc == "\n" do
                  ["\r", "\n"]
                else
                  # perhaps handle unexpected \r not followed by \n
                  ["\r", nc]
                end
              end

            collect_string(this, token ++ addend, quoter, multi_line, false, start)
          end

        c == String.first(quoter) && !escaped ->
          s = as_string(token ++ [c])
          n = 1 + length(token)
          qn = String.length(quoter)
          # Logger.debug("[#{s}|#{n}|#{qn}|#{String.slice(s, -qn..-1)}|#{quoter}]")
          if n >= qn * 2 && String.slice(s, -qn..-1) == quoter do
            state = Agent.get(this, fn state -> state end)
            # Have to be careful to use a range here - on Windows, \r\n is counted
            # as one grapheme, but we've counted it as two since we've been using
            # a list rather than a string to collect the token.
            pe = parse_escapes(state.escapes, String.slice(s, qn..(-qn - 1)), 0)

            case pe do
              {:error, %RecognizerError{reason: r, detail: d}} ->
                error(r, start, d)

              {:ok, pv} ->
                r = %Token{
                  kind: :STRING,
                  text: s,
                  value: pv,
                  start: start,
                  end: state.char_location
                }

                {:ok, r}
            end
          else
            collect_string(this, token ++ [c], quoter, multi_line, false, start)
          end

        true ->
          token = token ++ [c]
          # IO.puts(as_string(token))
          collect_string(this, token, quoter, multi_line, false, start)
      end
    end

    defp collect_string(this, quote, start) do
      c = get_char(this)

      quoter =
        if c != quote do
          push_back(this, c)
          quote
        else
          c = get_char(this)

          if c != quote do
            push_back(this, c)
            push_back(this, quote)
            quote
          else
            "#{quote}#{quote}#{quote}"
          end
        end

      collect_string(
        this,
        String.split(quoter, "", trim: true),
        quoter,
        String.length(quoter) > 1,
        false,
        start
      )
    end

    defp has_exponent(token) do
      s = as_string(token)
      String.contains?(s, "E") || String.contains?(s, "e")
    end

    defp number_done(this, token, start, radix, c) do
      s = as_string(token)

      if String.match?(String.last(s), ~r/[eoxb_-]/i) do
        error(:bad_number, char_loc(this), s)
      else
        kind =
          cond do
            String.ends_with?(s, ["j", "J"]) ->
              :COMPLEX

            String.contains?(s, ".") || has_exponent(token) ->
              :FLOAT

            true ->
              :INTEGER
          end

        ss =
          cond do
            String.slice(s, 0, 2) == "-." ->
              "-0#{String.slice(s, 1..-1)}"

            String.slice(s, 0, 1) == "." ->
              "0#{s}"

            true ->
              s
          end

        ss =
          cond do
            kind == :COMPLEX ->
              String.slice(ss, 0..-2)

            radix != 10 ->
              String.slice(ss, 2..-1)

            true ->
              ss
          end

        ss = String.replace(ss, ~r/_/, "")
        # At this point, we could have a number with a leading zero,
        # which should be treated as an octal constant, even though
        # the radix would be 10 here as we didn't start with 0[xob].
        # We check for that specifically before the final parse to
        # a number.
        if kind == :INTEGER and radix == 10 && String.match?(ss, ~r/^0[0-9]+$/) &&
             !String.match?(ss, ~r/^0[0-7]+$/) do
          error(:bad_octal_constant, char_loc(this), ss)
        else
          radix =
            if radix == 10 && String.match?(ss, ~r/^0[0-7]+$/) do
              8
            else
              radix
            end

          {v, _} =
            if kind == :INTEGER do
              Integer.parse(ss, radix)
            else
              Float.parse(ss)
            end

          state = Agent.get(this, fn state -> state end)
          eloc = adjusted_loc(c, state.char_location)
          # IO.puts("#{kind}, #{v}")
          v =
            if kind != :COMPLEX do
              v
            else
              Complex.new(0, v)
            end

          {:ok, %Token{kind: kind, text: s, value: v, start: start, end: eloc}}
        end
      end
    end

    defp last_index(haystack, needle) do
      result = Enum.find_index(Enum.reverse(haystack), fn x -> x == needle end)

      result =
        if result == nil do
          nil
        else
          length(haystack) - result - 1
        end

      # IO.puts("last_index(#{haystack}, #{needle}) -> #{result}")
      result
    end

    defp is_valid_digit(c, radix) do
      cond do
        radix == 16 ->
          String.match?(c, ~r/[0-9a-f]/i)

        radix == 8 ->
          c >= "0" && c <= "7"

        radix == 2 ->
          c == "0" || c == "1"

        true ->
          is_digit(c)
      end
    end

    defp collect_number(this, token, radix, start) do
      c = get_char(this)

      cond do
        c == :eof ->
          number_done(this, token, start, radix, c)

        is_valid_digit(c, radix) ->
          collect_number(this, token ++ [c], radix, start)

        c == "_" ->
          if is_valid_digit(List.last(token), radix) do
            collect_number(this, token ++ [c], radix, start)
          else
            error(:bad_number, char_loc(this), "#{token}#{c}")
          end

        c == "." ->
          if radix != 10 || String.contains?(as_string(token), ".") do
            error(:bad_number, char_loc(this), "#{token}#{c}")
          else
            collect_number(this, token ++ [c], radix, start)
          end

        c == "e" || c == "E" ->
          if has_exponent(token) || radix != 10 do
            error(:bad_number, char_loc(this), "#{token}#{c}")
          else
            collect_number(this, token ++ [c], radix, start)
          end

        c == "-" ->
          lv = last_index(token, "-")
          # existing minus after exponent
          if !has_exponent(token) || (lv != nil && lv > 0) do
            error(:bad_number, char_loc(this), "#{token}#{c}")
          else
            collect_number(this, token ++ [c], radix, start)
          end

        c == "j" || c == "J" ->
          if radix != 10 do
            error(:bad_number, char_loc(this), "#{token}#{c}")
          else
            nc = get_char(this)

            if nc != :eof && is_alnum(nc) do
              error(:bad_number, char_loc(this), "#{token}#{c}#{nc}")
            else
              push_back(this, nc)
              number_done(this, token ++ [c], start, radix, nc)
            end
          end

        # We flag up an alphanumeric char adjacent to a number
        is_alnum(c) ->
          error(:bad_number, char_loc(this), "#{token}#{c}")

        true ->
          push_back(this, c)
          number_done(this, token, start, radix, c)
      end
    end

    defp punctuation(kind, s, start) do
      eloc = %{start | column: start.column + String.length(s) - 1}
      {:ok, %Token{kind: kind, text: s, value: nil, start: start, end: eloc}}
    end

    defp get_radix(c) do
      cond do
        c == "x" || c == "X" ->
          16

        c == "o" || c == "O" ->
          8

        c == "b" || c == "B" ->
          2

        true ->
          10
      end
    end

    def collect_punct(this, c, kind, start) do
      case c do
        "=" ->
          nc = get_char(this)

          if nc == "=" do
            punctuation(:EQ, "==", start)
          else
            push_back(this, nc)
            punctuation(kind, c, start)
          end

        "!" ->
          nc = get_char(this)

          if nc == "=" do
            punctuation(:NEQ, "!=", start)
          else
            push_back(this, nc)
            punctuation(kind, c, start)
          end

        "*" ->
          nc = get_char(this)

          if nc == "*" do
            punctuation(:POWER, "**", start)
          else
            push_back(this, nc)
            punctuation(kind, c, start)
          end

        "/" ->
          nc = get_char(this)

          if nc == "/" do
            punctuation(:SLASHSLASH, "//", start)
          else
            push_back(this, nc)
            punctuation(kind, c, start)
          end

        "<" ->
          nc = get_char(this)

          case nc do
            "=" ->
              punctuation(:LE, "<=", start)

            ">" ->
              punctuation(:ALT_NEQ, "<>", start)

            "<" ->
              punctuation(:LSHIFT, "<<", start)

            _ ->
              push_back(this, nc)
              punctuation(kind, c, start)
          end

        ">" ->
          nc = get_char(this)

          case nc do
            "=" ->
              punctuation(:GE, ">=", start)

            ">" ->
              punctuation(:RSHIFT, ">>", start)

            _ ->
              push_back(this, nc)
              punctuation(kind, c, start)
          end

        _ ->
          punctuation(kind, c, start)
      end
    end

    defp collect_newline(s, start) do
      {:ok,
       %Token{
         kind: :NEWLINE,
         text: s,
         value: nil,
         start: start,
         end: %Location{line: start.line + 1, column: 0}
       }}
    end

    def get_token(this) do
      c = get_char(this)
      state = Agent.get(this, fn state -> state end)
      start_location = state.char_location
      # end_location = state.char_location

      cond do
        c == :eof ->
          {:ok,
           %Token{kind: :EOF, text: "", value: nil, start: state.location, end: state.location}}

        String.match?(c, ~r/[ \t]/) ->
          get_token(this)

        c == "\r" || c == "\n" ->
          if c == "\r" do
            c = get_char(this)

            if c != "\n" do
              push_back(this, c)
            end
          end

          collect_newline("\n", start_location)

        c == "#" ->
          s = IO.read(state.stream, :line)
          loc = Location.new(start_location.line + 1, 1)
          Agent.update(this, fn state -> %{state | location: loc} end)
          collect_newline("\##{String.trim_trailing(s)}", start_location)

        c == "\\" ->
          c = get_char(this)

          if c != "\r" && c != "\n" do
            error(:unexpected_char, state.char_location, "\\")
          else
            if c == "\r" do
              c = get_char(this)

              if c != "\n" do
                push_back(this, c)
              end
            end

            get_token(this)
          end

        c == "'" || c == "\"" ->
          collect_string(this, c, start_location)

        c == "`" ->
          collect_backtick(this, [c], start_location)

        String.match?(c, ~r/[_\p{L}]/u) ->
          collect_ident(this, [c], start_location)

        c == "0" ->
          c = get_char(this)
          radix = get_radix(c)

          token =
            if radix == 10 do
              push_back(this, c)
              ["0"]
            else
              ["0", c]
            end

          collect_number(this, token, radix, start_location)

        is_digit(c) ->
          collect_number(this, [c], 10, start_location)

        c == "." ->
          c = get_char(this)

          if is_digit(c) do
            collect_number(this, [".", c], 10, start_location)
          else
            push_back(this, c)
            punctuation(:DOT, ".", start_location)
          end

        c == "-" ->
          c = get_char(this)

          cond do
            c == "0" ->
              c = get_char(this)
              radix = get_radix(c)

              token =
                if radix == 10 do
                  ["-", "0"]
                else
                  ["-", "0", c]
                end

              collect_number(this, token, radix, start_location)

            is_digit(c) || c == "." ->
              collect_number(this, ["-", c], 10, start_location)

            true ->
              push_back(this, c)
              punctuation(:MINUS, "-", start_location)
          end

        Map.has_key?(state.punctuation, c) ->
          collect_punct(this, c, Map.get(state.punctuation, c), start_location)

        true ->
          error(:unexpected_char, char_loc(this), c)
      end
    end
  end

  defmodule UnaryNode do
    # """    This module represents an AST node for a unary expression."""
    @moduledoc false

    defstruct [:kind, :operand, :start]

    @doc """
    Return a new unary node.
    """
    @spec new(atom(), struct(), %Location{}) :: %UnaryNode{}
    def new(kind, operand, start) do
      %UnaryNode{kind: kind, operand: operand, start: start}
    end
  end

  defmodule BinaryNode do
    # """This module represents an AST node for a binary expression."""
    @moduledoc false

    defstruct [:kind, :lhs, :rhs, :start]

    @doc """
    Return a new binary node.
    """
    @spec new(atom(), struct(), struct(), %Location{}) :: %BinaryNode{}
    def new(kind, lhs, rhs, start) do
      %BinaryNode{kind: kind, lhs: lhs, rhs: rhs, start: start}
    end
  end

  defimpl Inspect, for: Token do
    def inspect(tok, _opts) do
      v =
        if tok.kind == :WORD do
          tok.text
        else
          tok.value
        end

      "_T[#{inspect(tok.kind)}|#{v}|#{tok.start}]"
    end
  end

  defimpl Inspect, for: UnaryNode do
    def inspect(un, _opts) do
      "_U[#{inspect(un.kind)}|#{inspect(un.operand)}]"
    end
  end

  defimpl Inspect, for: BinaryNode do
    def inspect(bn, _opts) do
      "_B[#{inspect(bn.kind)}|#{inspect(bn.lhs)}|#{inspect(bn.rhs)}]"
    end
  end

  defmodule SliceNode do
    # """This module represents an AST node for a slice expression (start, stop, step)."""
    @moduledoc false

    defstruct [:start_index, :stop_index, :step, :start]

    @doc """
    Return a new slice node.
    """
    @spec new(any(), any(), any(), %Location{}) :: %SliceNode{}
    def new(start, stop, step, loc) do
      %SliceNode{start_index: start, stop_index: stop, step: step, start: loc}
    end
  end

  defmodule ListNode do
    # """This module represents an AST node for a list."""
    @moduledoc false

    defstruct kind: :LBRACK, elements: [], start: nil

    def new(elements, start) do
      %ListNode{elements: elements, start: start}
    end
  end

  defmodule MappingNode do
    # """This module represents an AST node for a mapping."""
    @moduledoc false

    defstruct kind: :LCURLY, elements: [], start: nil

    def new(elements, start) do
      %MappingNode{elements: elements, start: start}
    end
  end

  defmodule Parser do
    # """This module contains the functionality to convert CFG source code into an AST (abstract syntax tree)."""
    @moduledoc false
    use Agent
    require Logger

    defstruct [
      :tokenizer,
      :next_token,
      :expression_starters,
      :value_starters,
      :comparison_operators
    ]

    def new(stream) do
      tokenizer = Tokenizer.new(stream)
      v = Tokenizer.get_token(tokenizer)

      case v do
        {:error, _} ->
          v

        {:ok, t} ->
          Agent.start(fn ->
            %Parser{
              tokenizer: tokenizer,
              next_token: t,
              expression_starters:
                MapSet.new([
                  :LCURLY,
                  :LBRACK,
                  :LPAREN,
                  :AT,
                  :DOLLAR,
                  :BACKTICK,
                  :PLUS,
                  :MINUS,
                  :BITNOT,
                  :INTEGER,
                  :FLOAT,
                  :COMPLEX,
                  :TRUE,
                  :FALSE,
                  :NONE,
                  :NOT,
                  :STRING,
                  :WORD
                ]),
              value_starters:
                MapSet.new([
                  :BACKTICK,
                  :INTEGER,
                  :FLOAT,
                  :COMPLEX,
                  :TRUE,
                  :FALSE,
                  :NONE,
                  :STRING,
                  :WORD
                ]),
              comparison_operators:
                MapSet.new([
                  :LT,
                  :LE,
                  :GT,
                  :GE,
                  :EQ,
                  :NEQ,
                  :ALT_NEQ,
                  :IS,
                  :IN,
                  :NOT
                ])
            }
          end)
      end
    end

    def from_source(s) do
      {:ok, stream} = StringIO.open(s)
      new(stream)
    end

    def from_file(path) do
      {:ok, stream} = File.open(path, [:read, :utf8])
      new(stream)
    end

    def at_end(this) do
      Agent.get(this, fn state -> state.next_token.kind == :EOF end)
    end

    defp advance(this) do
      tokenizer = Agent.get(this, fn state -> state.tokenizer end)
      v = Tokenizer.get_token(tokenizer)

      case v do
        {:error, _} ->
          v

        {:ok, nt} ->
          Agent.update(this, fn state -> %{state | next_token: nt} end)
          {:ok, nt.kind}
      end
    end

    # public for debugging
    def next_token(this) do
      Agent.get(this, fn state -> state.next_token end)
    end

    defp next_token_start(this) do
      Agent.get(this, fn state -> state.next_token.start end)
    end

    defp error(reason, loc, detail) do
      {:error, RecognizerError.exception(reason, loc, detail)}
    end

    defp expect(this, kind) do
      nt = next_token(this)

      if nt.kind != kind do
        error(:unexpected_token, nt.start, {kind, nt.kind})
      else
        advance(this)
        {:ok, nt}
      end
    end

    defp consume_newlines(this) do
      nt = next_token(this)

      case nt.kind do
        :NEWLINE ->
          v = advance(this)

          case v do
            {:ok, :NEWLINE} ->
              consume_newlines(this)

            _ ->
              v
          end

        _ ->
          {:ok, nt.kind}
      end
    end

    defp collect_strings(this, pid) do
      nt = next_token(this)

      Agent.update(pid, fn state ->
        %{
          state
          | texts: state.texts ++ [nt.text],
            values: state.values ++ [nt.value],
            end: nt.end
        }
      end)

      v = advance(this)

      case v do
        {:error, _} ->
          v

        {:ok, :STRING} ->
          collect_strings(this, pid)

        {:ok, _} ->
          state = Agent.get(pid, fn state -> state end)

          merged = %Token{
            kind: :STRING,
            text: Enum.join(state.texts),
            value: Enum.join(state.values),
            start: state.start,
            end: state.end
          }

          {:ok, merged}
      end
    end

    def strings(this) do
      result = next_token(this)
      v = advance(this)

      case v do
        {:error, _} ->
          v

        {:ok, :STRING} ->
          {:ok, pid} =
            Agent.start(fn ->
              %{
                texts: [result.text],
                values: [result.value],
                start: result.start,
                end: result.end
              }
            end)

          v = collect_strings(this, pid)
          Agent.stop(pid)
          v

        {:ok, _} ->
          {:ok, result}
      end
    end

    def value(this) do
      state = Agent.get(this, fn state -> state end)
      kind = state.next_token.kind

      if !MapSet.member?(state.value_starters, kind) do
        error(:unexpected_at_value, state.next_token.start, kind)
      else
        if kind == :STRING do
          strings(this)
        else
          result = state.next_token
          v = advance(this)

          case v do
            {:error, _} ->
              v

            _ ->
              {:ok, result}
          end
        end
      end
    end

    def atom(this) do
      state = Agent.get(this, fn state -> state end)
      nt = state.next_token

      case nt.kind do
        :LCURLY ->
          mapping(this)

        :LBRACK ->
          list(this)

        :LPAREN ->
          v = advance(this)

          case v do
            {:error, _} ->
              v

            _ ->
              expr = expression(this)

              case expr do
                {:error, _} ->
                  expr

                _ ->
                  v = expect(this, :RPAREN)

                  case v do
                    {:error, _} ->
                      v

                    _ ->
                      expr
                  end
              end
          end

        :DOLLAR ->
          v = advance(this)

          case v do
            {:error, _} ->
              v

            _ ->
              v = expect(this, :LCURLY)

              case v do
                {:error, _} ->
                  v

                _ ->
                  start = Agent.get(this, fn state -> state.next_token.start end)
                  expr = primary(this)

                  case expr do
                    {:error, _} ->
                      v

                    {:ok, expr} ->
                      v = expect(this, :RCURLY)

                      case v do
                        {:error, _} ->
                          v

                        _ ->
                          {:ok, UnaryNode.new(:DOLLAR, expr, start)}
                      end
                  end
              end
          end

        # Can't use MapSet.member?(state.value_starters, k) :-(
        k
        when k in [
               :BACKTICK,
               :INTEGER,
               :FLOAT,
               :COMPLEX,
               :TRUE,
               :FALSE,
               :NONE,
               :STRING,
               :WORD
             ] ->
          value(this)

        _ ->
          error(:unexpected_token_for_atom, nt.start, nt.kind)
      end
    end

    defp mapping_key(this, nt) do
      if nt.kind == :STRING do
        strings(this)
      else
        v = advance(this)

        case v do
          {:error, _} -> v
          _ -> {:ok, nt}
        end
      end
    end

    defp advance_and_consume_newlines(this) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          consume_newlines(this)
      end
    end

    defp collect_mapping_elements(this, nt, elements) do
      v = mapping_key(this, nt)

      case v do
        {:error, _} ->
          v

        {:ok, key} ->
          nt = next_token(this)

          if nt.kind != :COLON && nt.kind != :ASSIGN do
            error(:bad_key_value_separator, nt.start, nt.kind)
          else
            v = advance_and_consume_newlines(this)

            case v do
              {:error, _} ->
                v

              _ ->
                v = expression(this)

                case v do
                  {:error, _} ->
                    v

                  {:ok, expr} ->
                    kvp = {key, expr}
                    nt = next_token(this)

                    case nt.kind do
                      k when k in [:NEWLINE, :COMMA] ->
                        v = advance_and_consume_newlines(this)

                        case v do
                          {:error, _} ->
                            v

                          {:ok, kind} ->
                            if kind in [:EOF, :RCURLY] do
                              {:ok, elements ++ [kvp]}
                            else
                              nt = next_token(this)

                              if kind != :WORD && kind != :STRING do
                                error(:unexpected_for_key, nt.start, kind)
                              else
                                collect_mapping_elements(this, nt, elements ++ [kvp])
                              end
                            end
                        end

                      k when k in [:EOF, :RCURLY] ->
                        {:ok, elements ++ [kvp]}

                      k when k in [:WORD, :STRING] ->
                        collect_mapping_elements(this, nt, elements ++ [kvp])

                      _ ->
                        error(:unexpected_for_key, nt.start, nt.kind)
                    end
                end
            end
          end
      end
    end

    def mapping_body(this) do
      v = consume_newlines(this)

      case v do
        {:error, _} ->
          v

        _ ->
          nt = next_token(this)
          kind = nt.kind
          start = nt.start

          if kind == :EOF || kind == :RCURLY do
            {:ok, MappingNode.new([], start)}
          else
            if kind != :WORD && kind != :STRING do
              error(:unexpected_for_key, start, kind)
            else
              v = collect_mapping_elements(this, nt, [])

              case v do
                {:error, _} ->
                  v

                {:ok, elements} ->
                  {:ok, MappingNode.new(elements, start)}
              end
            end
          end
      end
    end

    def mapping(this) do
      v = expect(this, :LCURLY)

      case v do
        {:error, _} ->
          v

        _ ->
          v = mapping_body(this)

          case v do
            {:error, _} ->
              v

            _ ->
              ev = expect(this, :RCURLY)

              case ev do
                {:error, _} -> ev
                _ -> v
              end
          end
      end
    end

    defp collect_list_elements(this, elements) do
      v = expression(this)

      case v do
        {:error, _} ->
          v

        {:ok, expr} ->
          {es, kind} =
            Agent.get(this, fn state -> {state.expression_starters, state.next_token.kind} end)

          if kind in [:NEWLINE, :COMMA] do
            v = advance_and_consume_newlines(this)

            case v do
              {:error, _} ->
                v

              {:ok, kind} ->
                if !MapSet.member?(es, kind) do
                  {:ok, elements ++ [expr]}
                else
                  collect_list_elements(this, elements ++ [expr])
                end
            end
          else
            {:ok, elements ++ [expr]}
          end
      end
    end

    def list_body(this) do
      v = consume_newlines(this)

      case v do
        {:error, _} ->
          v

        _ ->
          {es, nt} =
            Agent.get(this, fn state -> {state.expression_starters, state.next_token} end)

          kind = nt.kind
          start = nt.start
          elements = []

          if MapSet.member?(es, kind) do
            v = collect_list_elements(this, elements)

            case v do
              {:error, _} ->
                v

              {:ok, elements} ->
                {:ok, ListNode.new(elements, start)}
            end
          else
            {:ok, ListNode.new(elements, start)}
          end
      end
    end

    def list(this) do
      v = expect(this, :LBRACK)

      case v do
        {:error, _} ->
          v

        _ ->
          v = list_body(this)

          case v do
            {:error, _} ->
              v

            _ ->
              ev = expect(this, :RBRACK)

              case ev do
                {:error, _} -> ev
                _ -> v
              end
          end
      end
    end

    def container(this, check_end \\ true) do
      v = consume_newlines(this)

      result =
        case v do
          {:error, _} ->
            v

          {:ok, :LCURLY} ->
            mapping(this)

          {:ok, :LBRACK} ->
            list(this)

          {:ok, k} ->
            if k in [:WORD, :STRING, :EOF] do
              mapping_body(this)
            else
              nt = next_token(this)
              error(:unexpected_token_for_container, nt.start, k)
            end
        end

      case result do
        {:error, _} ->
          result

        _ ->
          v = consume_newlines(this)

          case v do
            {:error, _} ->
              v

            _ ->
              if !check_end || at_end(this) do
                result
              else
                nt = next_token(this)
                error(:text_after_container, nt.start, nt)
              end
          end
      end
    end

    defp get_slice_element(this) do
      v = list_body(this)

      case v do
        {:error, _} ->
          v

        {:ok, lb} ->
          n = length(lb.elements)

          if n != 1 do
            error(:invalid_index, lb.start, n)
          else
            {:ok, List.first(lb.elements)}
          end
      end
    end

    defp trailer(this, kind) do
      if kind == :DOT do
        v = advance(this)

        case v do
          {:error, _} ->
            v

          _ ->
            v = expect(this, :WORD)

            case v do
              {:error, _} -> v
              {:ok, w} -> {:ok, :DOT, w}
            end
        end
      else
        start = next_token_start(this)
        v = advance(this)

        case v do
          {:error, _} ->
            v

          _ ->
            {:ok, pid} =
              Agent.start(fn ->
                %{is_slice: false, start_index: nil, stop_index: nil, step: nil, result: nil}
              end)

            # After this point we just store errors in the state.result, and
            # decide what to do at the end
            kind = Agent.get(this, fn state -> state.next_token.kind end)

            if kind == :COLON do
              Agent.update(pid, fn state -> %{state | is_slice: true} end)
            else
              v = get_slice_element(this)

              case v do
                {:error, _} ->
                  Agent.update(pid, fn state -> %{state | result: v} end)

                {:ok, elem} ->
                  kind = Agent.get(this, fn state -> state.next_token.kind end)

                  if kind != :COLON do
                    Agent.update(pid, fn state -> %{state | result: elem} end)
                  else
                    Agent.update(pid, fn state -> %{state | start_index: elem, is_slice: true} end)
                  end
              end
            end

            state = Agent.get(pid, fn state -> state end)

            if state.is_slice do
              # at this point start_index is either nil (if foo[:xyz]) or a
              # value representing the start. We are pointing at the COLON
              # after the start value
              v = advance(this)

              case v do
                {:error, _} ->
                  Agent.update(pid, fn state -> %{state | result: v} end)

                {:ok, kind} ->
                  case kind do
                    # no stop, but there might be a step
                    :COLON ->
                      v = advance(this)

                      case v do
                        {:error, _} ->
                          Agent.update(pid, fn state -> %{state | result: v} end)

                        {:ok, kind} ->
                          if kind != :RBRACK do
                            v = get_slice_element(this)

                            case v do
                              {:error, _} ->
                                Agent.update(pid, fn state -> %{state | result: v} end)

                              {:ok, elem} ->
                                Agent.update(pid, fn state -> %{state | step: elem} end)
                            end
                          end
                      end

                    :RBRACK ->
                      {}

                    _ ->
                      v = get_slice_element(this)

                      case v do
                        {:error, _} ->
                          Agent.update(pid, fn state -> %{state | result: v} end)

                        {:ok, elem} ->
                          Agent.update(pid, fn state -> %{state | stop_index: elem} end)
                          kind = Agent.get(this, fn state -> state.next_token.kind end)

                          if kind == :COLON do
                            v = advance(this)

                            case v do
                              {:error, _} ->
                                Agent.update(pid, fn state -> %{state | result: v} end)

                              {:ok, kind} ->
                                if kind != :RBRACK do
                                  v = get_slice_element(this)

                                  case v do
                                    {:error, _} ->
                                      Agent.update(pid, fn state -> %{state | result: v} end)

                                    {:ok, elem} ->
                                      Agent.update(pid, fn state -> %{state | step: elem} end)
                                  end
                                end
                            end
                          end
                      end
                  end
              end
            end

            v = expect(this, :RBRACK)

            case v do
              {:error, _} -> Agent.update(pid, fn state -> %{state | result: v} end)
              _ -> {}
            end

            state = Agent.get(pid, fn state -> state end)
            Agent.stop(pid)

            case state.result do
              {:error, _} ->
                state.result

              _ ->
                if state.is_slice do
                  {:ok, :COLON,
                   SliceNode.new(state.start_index, state.stop_index, state.step, start)}
                else
                  {:ok, :LBRACK, state.result}
                end
            end
        end
      end
    end

    defp collect_trailers(this, kind, lhs, start) do
      t = trailer(this, kind)

      case t do
        {:error, _} ->
          t

        {:ok, op, v} ->
          new_lhs = BinaryNode.new(op, lhs, v, start)
          kind = Agent.get(this, fn state -> state.next_token.kind end)

          if kind == :DOT || kind == :LBRACK do
            collect_trailers(this, kind, new_lhs, start)
          else
            {:ok, new_lhs}
          end
      end
    end

    def primary(this) do
      start = next_token_start(this)
      result = atom(this)

      case result do
        {:error, _} ->
          result

        {:ok, lhs} ->
          kind = Agent.get(this, fn state -> state.next_token.kind end)

          if kind == :DOT || kind == :LBRACK do
            collect_trailers(this, kind, lhs, start)
          else
            result
          end
      end
    end

    defp collect_power(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = unary_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:POWER, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :POWER do
                collect_power(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def power(this) do
      start = next_token_start(this)
      result = primary(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :POWER do
            result
          else
            collect_power(this, expr, start)
          end
      end
    end

    def unary_expr(this) do
      nt = next_token(this)

      if nt.kind not in [:PLUS, :MINUS, :BITNOT, :AT] do
        power(this)
      else
        v = advance(this)

        case v do
          {:error, _} ->
            v

          _ ->
            v = unary_expr(this)

            case v do
              {:error, _} ->
                v

              {:ok, expr} ->
                {:ok, UnaryNode.new(nt.kind, expr, nt.start)}
            end
        end
      end
    end

    def collect_mul_expr(this, kind, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = unary_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(kind, lhs, expr, start)
              nt = next_token(this)

              if nt.kind in [:STAR, :SLASH, :SLASHSLASH, :MODULO] do
                collect_mul_expr(this, nt.kind, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def mul_expr(this) do
      start = next_token_start(this)
      result = unary_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind not in [:STAR, :SLASH, :SLASHSLASH, :MODULO] do
            result
          else
            collect_mul_expr(this, nt.kind, expr, start)
          end
      end
    end

    def collect_add_expr(this, kind, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = mul_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(kind, lhs, expr, start)
              nt = next_token(this)

              if nt.kind in [:PLUS, :MINUS] do
                collect_add_expr(this, nt.kind, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def add_expr(this) do
      start = next_token_start(this)
      result = mul_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind not in [:PLUS, :MINUS] do
            result
          else
            collect_add_expr(this, nt.kind, expr, start)
          end
      end
    end

    def collect_shift_expr(this, kind, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = add_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(kind, lhs, expr, start)
              nt = next_token(this)

              if nt.kind in [:LSHIFT, :RSHIFT] do
                collect_shift_expr(this, nt.kind, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def shift_expr(this) do
      start = next_token_start(this)
      result = add_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind not in [:LSHIFT, :RSHIFT] do
            result
          else
            collect_shift_expr(this, nt.kind, expr, start)
          end
      end
    end

    def collect_bitand_expr(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = shift_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:BITAND, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :BITAND do
                collect_bitand_expr(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def bitand_expr(this) do
      start = next_token_start(this)
      result = shift_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :BITAND do
            result
          else
            collect_bitand_expr(this, expr, start)
          end
      end
    end

    def collect_bitxor_expr(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = bitand_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:BITXOR, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :BITXOR do
                collect_bitxor_expr(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def bitxor_expr(this) do
      start = next_token_start(this)
      result = bitand_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :BITXOR do
            result
          else
            collect_bitxor_expr(this, expr, start)
          end
      end
    end

    def collect_bitor_expr(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = bitxor_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:BITOR, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :BITOR do
                collect_bitor_expr(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def bitor_expr(this) do
      start = next_token_start(this)
      result = bitxor_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :BITOR do
            result
          else
            collect_bitor_expr(this, expr, start)
          end
      end
    end

    def comparison_op(this) do
      nt = next_token(this)
      v = advance(this)

      case v do
        {:error, _} ->
          v

        {:ok, kind} ->
          {kind, should_advance} =
            cond do
              nt.kind == :IS && kind == :NOT ->
                {:ISNOT, true}

              nt.kind == :NOT && kind == :IN ->
                {:NOTIN, true}

              true ->
                {nt.kind, false}
            end

          if !should_advance do
            {:ok, kind}
          else
            v = advance(this)

            case v do
              {:error, _} -> v
              _ -> {:ok, kind}
            end
          end
      end
    end

    def comparison(this) do
      start = next_token_start(this)
      result = bitor_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          {co, nt} =
            Agent.get(this, fn state -> {state.comparison_operators, state.next_token} end)

          if !MapSet.member?(co, nt.kind) do
            result
          else
            v = comparison_op(this)

            case v do
              {:error, _} ->
                v

              {:ok, kind} ->
                rhs = bitor_expr(this)

                case rhs do
                  {:error, _} -> rhs
                  {:ok, erhs} -> {:ok, BinaryNode.new(kind, expr, erhs, start)}
                end
            end
          end
      end
    end

    def not_expr(this) do
      nt = next_token(this)

      if nt.kind != :NOT do
        comparison(this)
      else
        v = advance(this)

        case v do
          {:error, _} ->
            v

          _ ->
            v = not_expr(this)

            case v do
              {:error, _} ->
                v

              {:ok, expr} ->
                {:ok, UnaryNode.new(:NOT, expr, nt.start)}
            end
        end
      end
    end

    def collect_and_expr(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = not_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:AND, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :AND do
                collect_and_expr(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def and_expr(this) do
      start = next_token_start(this)
      result = not_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :AND do
            result
          else
            collect_and_expr(this, expr, start)
          end
      end
    end

    def collect_or_expr(this, lhs, start) do
      v = advance(this)

      case v do
        {:error, _} ->
          v

        _ ->
          v = and_expr(this)

          case v do
            {:error, _} ->
              v

            {:ok, expr} ->
              result = BinaryNode.new(:OR, lhs, expr, start)
              nt = next_token(this)

              if nt.kind == :OR do
                collect_or_expr(this, result, start)
              else
                {:ok, result}
              end
          end
      end
    end

    def or_expr(this) do
      start = next_token_start(this)
      result = and_expr(this)

      case result do
        {:error, _} ->
          result

        {:ok, expr} ->
          nt = next_token(this)

          if nt.kind != :OR do
            result
          else
            collect_or_expr(this, expr, start)
          end
      end
    end

    def expression(this) do
      {es, nt} = Agent.get(this, fn state -> {state.expression_starters, state.next_token} end)

      if !MapSet.member?(es, nt.kind) do
        error(:unexpected_token_for_expression, nt.start, nt.kind)
      else
        or_expr(this)
      end
    end
  end

  defmodule MapUtils do
    # """A utility module to merge maps recursively."""
    @moduledoc false
    def deep_merge(left, right) do
      Map.merge(left, right, &deep_resolve/3)
    end

    # Key exists in both maps, and both values are maps as well.
    # These can be merged recursively.
    defp deep_resolve(_key, left = %{}, right = %{}) do
      deep_merge(left, right)
    end

    # Key exists in both maps, but at least one of the values is
    # NOT a map. We fall back to standard merge behavior, preferring
    # the value on the right.
    defp deep_resolve(_key, _left, right) do
      right
    end
  end

  defmodule Pow do
    # """A utility module to raise integers to powers."""
    @moduledoc false
    require Integer

    def pow(_, 0), do: 1
    def pow(x, n) when Integer.is_odd(n), do: x * pow(x, n - 1)

    def pow(x, n) do
      result = pow(x, div(n, 2))
      result * result
    end
  end

  defmodule Config do
    @moduledoc """
    This module contains top-level functionality for working with CFG. Client code will usually just interact with this module.
    """
    use Agent
    use Bitwise
    require Logger

    defstruct data: nil,
              no_duplicates: true,
              strict_conversions: true,
              context: nil,
              include_path: [],
              path: nil,
              root_dir: nil,
              parent: nil,
              cache: nil,
              error: nil,
              scalar_tokens:
                MapSet.new([
                  :STRING,
                  :INTEGER,
                  :FLOAT,
                  :COMPLEX,
                  :FALSE,
                  :TRUE,
                  :NONE
                ]),
              refs_seen: nil,
              string_converter: nil

    @type t :: %__MODULE__{
            data: nil | map(),
            no_duplicates: boolean(),
            strict_conversions: boolean(),
            context: nil | map(),
            include_path: list(),
            path: nil | binary(),
            root_dir: nil | binary(),
            parent: nil | pid(),
            cache: nil | map(),
            string_converter: function()
          }

    defp error(reason, loc, detail) do
      {:error, RecognizerError.exception(reason, loc, detail)}
    end

    # Public for testing only
    @doc false
    def is_identifier(s) do
      Regex.match?(~r/^(?!\d)(\w+)$/u, s)
    end

    defp tuple_to_string(t) do
      parts =
        Enum.reduce(Tuple.to_list(t), [], fn item, parts ->
          s =
            case item do
              nt when is_tuple(nt) -> tuple_to_string(nt)
              m when is_map(m) -> map_to_string(m)
              _ -> to_string(item)
            end

          parts ++ [s]
        end)

      "[#{Enum.join(parts, ", ")}]"
    end

    defp map_to_string(m) do
      parts =
        Enum.reduce(m, [], fn {k, v}, parts ->
          s =
            case v do
              nt when is_tuple(nt) -> tuple_to_string(nt)
              m when is_map(m) -> map_to_string(m)
              _ -> to_string(v)
            end

          parts ++ ["#{k}: #{s}"]
        end)

      "{#{Enum.join(parts, ", ")}}"
    end

    defp default_string_converter(s, cfg) do
      iso_datetime_pattern =
        ~r/^(\d{4})-(\d{2})-(\d{2})(([ T])(((\d{2}):(\d{2}):(\d{2}))(\.\d{1,6})?(([+-])(\d{2}):(\d{2})(:(\d{2})(\.\d{1,6})?)?)?))?$/

      env_value_pattern = ~r/^\$(\w+)(\|(.*))?$/
      colon_object_pattern = ~r/^([A-Za-z_]\w*(\.[A-Za-z_]\w*)*)(:([A-Za-z_]\w*))?$/
      interpolation_pattern = ~r/\$\{([^}]+)\}/

      m = Regex.run(iso_datetime_pattern, s)

      if !is_nil(m) do
        mlen = length(m)
        m = List.to_tuple(m)
        has_time = mlen > 4
        {year, _} = Integer.parse(elem(m, 1))
        {month, _} = Integer.parse(elem(m, 2))
        {day, _} = Integer.parse(elem(m, 3))
        {:ok, date} = Date.new(year, month, day)

        if !has_time do
          date
        else
          {hour, _} = Integer.parse(elem(m, 8))
          {minute, _} = Integer.parse(elem(m, 9))
          {second, _} = Integer.parse(elem(m, 10))

          microsecond =
            if mlen < 12 || elem(m, 11) == "" do
              0
            else
              {f, _} = Float.parse("0" <> elem(m, 11))
              round(f * 1.0e6)
            end

          has_offset = mlen > 13
          {:ok, time} = Time.new(hour, minute, second, microsecond)
          {:ok, ndt} = NaiveDateTime.new(date, time)
          {:ok, date_time} = DateTime.from_naive(ndt, "Etc/UTC")

          if !has_offset do
            date_time
          else
            sign =
              if elem(m, 13) == "-" do
                -1
              else
                1
              end

            {ohour, _} = Integer.parse(elem(m, 14))
            {ominute, _} = Integer.parse(elem(m, 15))

            osecond =
              if mlen < 17 do
                0
              else
                {os, _} = Integer.parse(elem(m, 17))
                os
              end

            offset = osecond + 60 * ominute + 3600 * ohour
            %{date_time | utc_offset: sign * offset}
          end
        end
      else
        m = Regex.run(env_value_pattern, s)

        if !is_nil(m) do
          mlen = length(m)
          m = List.to_tuple(m)
          has_pipe = mlen > 2

          dv =
            if !has_pipe do
              nil
            else
              elem(m, 3)
            end

          System.get_env(elem(m, 1), dv)
        else
          m = Regex.run(colon_object_pattern, s)
          # Logger.debug("tested colon object pattern: #{inspect m}")
          if !is_nil(m) do
            m = List.to_tuple(m)

            try do
              mod = String.to_existing_atom(elem(m, 1))
              func = String.to_existing_atom(elem(m, 4))
              apply(mod, func, [])
            rescue
              _ -> s
            end
          else
            m = Regex.match?(interpolation_pattern, s)

            if !m do
              s
            else
              m = Regex.scan(interpolation_pattern, s, return: :index)
              # Logger.debug("#{__ENV__.line}: #{inspect m}")
              {pos, parts, failed} =
                Enum.reduce_while(m, {0, [], false}, fn m, {pos, parts, _} ->
                  [{ostart, olen}, {istart, ilen}] = m

                  parts =
                    if pos < ostart do
                      parts ++ [String.slice(s, pos, ostart - pos)]
                    else
                      parts
                    end

                  expr = String.slice(s, istart, ilen)
                  v = Config.get(cfg, expr)

                  case v do
                    {:error, _e} ->
                      # Logger.debug("#{__ENV__.line}: #{expr}: #{inspect(_e)}")
                      {:halt, {pos, parts, true}}

                    {:ok, v} ->
                      sv =
                        case v do
                          t when is_tuple(t) ->
                            tuple_to_string(t)

                          m when is_map(m) ->
                            map_to_string(m)

                          _ ->
                            to_string(v)
                        end

                      {:cont, {ostart + olen, parts ++ [sv], false}}
                  end
                end)

              parts =
                if !failed && pos < String.length(s) do
                  parts ++ [String.slice(s, pos..-1)]
                else
                  parts
                end

              # Logger.debug("#{__ENV__.line}: #{inspect({pos, parts, failed})}")

              if failed do
                s
              else
                Enum.join(parts)
              end
            end
          end
        end
      end
    end

    @doc "Return a new, empty configuration with specified options."
    @spec new(map()) :: {atom(), pid()}
    def new(options \\ %{}) do
      Agent.start(fn ->
        %Config{
          no_duplicates: Map.get(options, :no_duplicates, true),
          strict_conversions: Map.get(options, :strict_conversions, true),
          context: Map.get(options, :context),
          include_path: Map.get(options, :include_path, []),
          string_converter: Map.get(options, :string_converter, &default_string_converter/2)
        }
      end)
    end

    # defp name(this) do
    # Path.basename(Agent.get(this, fn state -> state.path end))
    # end

    defp wrap_mapping(this, mn) do
      no_dupes = Agent.get(this, fn state -> state.no_duplicates end)
      {:ok, pid} = Agent.start(fn -> %{error: nil, data: %{}} end)

      _ =
        Enum.reduce_while(mn.elements, pid, fn elem, pid ->
          {key, value} = elem
          data = Agent.get(pid, fn state -> state.data end)

          kv =
            if key.kind == :WORD do
              key.text
            else
              key.value
            end

          if no_dupes && Map.has_key?(data, kv) do
            e = RecognizerError.exception(:duplicate_key, key.start, kv)
            Agent.update(pid, fn state -> %{state | error: e} end)
            {:halt, pid}
          else
            data = Map.put(data, kv, value)
            Agent.update(pid, fn state -> %{state | data: data} end)
            {:cont, pid}
          end
        end)

      state = Agent.get(pid, fn state -> state end)
      Agent.stop(pid)
      # Logger.debug("#{__ENV__.line} #{inspect(state)}")

      if is_nil(state.error) do
        {:ok, state.data}
      else
        {:error, state.error}
      end
    end

    defp load(this, stream) do
      {:ok, p} = Parser.new(stream)
      v = Parser.container(p)

      case v do
        {:error, _} ->
          v

        {:ok, mn = %MappingNode{}} ->
          v = wrap_mapping(this, mn)

          case v do
            {:error, _} ->
              v

            {:ok, data} ->
              Agent.update(this, fn state -> %{state | data: data} end)
              {:ok, this}
          end

        {:ok, other} ->
          error(:must_be_mapping, other.start, other)
      end
    end

    @doc "Load this configuration from a file, given its path."
    @spec load_file(pid(), binary()) :: {atom(), any()}
    def load_file(this, path) do
      {:ok, stream} = File.open(path, [:read, :utf8])
      v = load(this, stream)

      case v do
        {:error, _} ->
          v

        {:ok, this} ->
          Config.set_path(this, path)
          v
      end
    end

    defp from_stream(stream) do
      {:ok, this} = new()
      load(this, stream)
    end

    @doc "Return a configuration from its source."
    @spec from_source(binary()) :: {atom(), any()}
    def from_source(s) do
      {:ok, stream} = StringIO.open(s)
      from_stream(stream)
    end

    @doc "Return a configuration from a file, given its path."
    @spec from_file(binary()) :: {atom(), any()}
    def from_file(path) do
      # Logger.debug("About to load: #{path}")
      {:ok, stream} = File.open(path, [:read, :utf8])
      v = from_stream(stream)
      # Logger.debug("#{__ENV__.line}: #{inspect v}")
      case v do
        {:error, _} ->
          v

        {:ok, this} ->
          Config.set_path(this, path)
          v
      end
    end

    @doc """
    Set the path from which this configuration was loaded.
    This is also used to determine the directory searched for included configurations.
    """
    @spec set_path(pid(), binary()) :: atom()
    def set_path(this, p) do
      rd = Path.dirname(p)
      Agent.update(this, fn state -> %{state | path: p, root_dir: rd} end)
    end

    @doc "See whether this configuration allows duplicates."
    @spec get_no_duplicates(pid()) :: boolean()
    def get_no_duplicates(this) do
      Agent.get(this, fn state -> state.no_duplicates end)
    end

    @doc "Set whether this configuration allows duplicates."
    @spec set_no_duplicates(pid(), boolean()) :: atom()
    def set_no_duplicates(this, no_dupes) do
      Agent.update(this, fn state -> %{state | no_duplicates: no_dupes} end)
    end

    @doc "Append or prepend a list of directories to the include path of this configuration."
    @spec add_include(pid(), [binary()], boolean()) :: atom()
    def add_include(this, path, append \\ true) when is_list(path) do
      ip = Agent.get(this, fn state -> state.include_path end)

      new_ip =
        if append do
          ip ++ path
        else
          path ++ ip
        end

      Agent.update(this, fn state -> %{state | include_path: new_ip} end)
    end

    @doc "Get the include path of this configuration."
    @spec get_include(pid()) :: [binary()]
    def get_include(this) do
      Agent.get(this, fn state -> state.include_path end)
    end

    @doc "Set the include path of this configuration to the specified list of directories."
    @spec set_include(pid(), [binary()]) :: atom()
    def set_include(this, path) when is_list(path) do
      Agent.update(this, fn state -> %{state | include_path: path} end)
    end

    # public for tests only
    @doc false
    def parse_path(s) do
      v = Parser.from_source(s)

      case v do
        {:error, _} ->
          v

        {:ok, p} ->
          t = Parser.next_token(p)

          if t.kind != :WORD do
            error(:invalid_path, t.start, s)
          else
            v = Parser.primary(p)

            case v do
              {:error, _} ->
                v

              _ ->
                if Parser.at_end(p) do
                  v
                else
                  t = Parser.next_token(p)
                  error(:invalid_path_extra, t.start, s)
                end
            end
          end
      end
    end

    # public for tests only
    @doc false
    def to_source(node) do
      case node do
        %Token{} ->
          to_string(node.value)

        %BinaryNode{} ->
          path = unpack_path(node)
          first = List.first(path)
          parts = [List.last(first).text]

          parts =
            if length(path) == 1 do
              parts
            else
              [_ | rest] = path

              Enum.reduce(rest, parts, fn item, parts ->
                case item do
                  [:DOT, t = %Token{kind: :WORD}] ->
                    parts ++ [".", t.text]

                  [:LBRACK, indexpr] ->
                    parts ++ ["[", to_source(indexpr), "]"]

                  [:COLON, sn = %SliceNode{}] ->
                    addend =
                      ["["] ++
                        if is_nil(sn.start_index) do
                          [":"]
                        else
                          [to_source(sn.start_index), ":"]
                        end

                    addend =
                      addend ++
                        if is_nil(sn.stop_index) do
                          []
                        else
                          [to_source(sn.stop_index)]
                        end

                    addend =
                      addend ++
                        if is_nil(sn.step) do
                          []
                        else
                          [":", to_source(sn.step)]
                        end

                    parts ++ addend ++ ["]"]
                end
              end)
            end

          Enum.join(parts)

        _ ->
          {:ok, to_string(node)}
      end
    end

    defp visit(pid, node) do
      case node do
        t = %Token{} ->
          Agent.update(pid, fn state -> state ++ [[:DOT, t]] end)

        %UnaryNode{operand: o} ->
          visit(pid, o)

        %BinaryNode{kind: k, lhs: lhs, rhs: rhs} ->
          visit(pid, lhs)
          Agent.update(pid, fn state -> state ++ [[k, rhs]] end)
      end
    end

    # public for tests only
    @doc false
    def unpack_path(node) do
      {:ok, pid} = Agent.start(fn -> [] end)
      visit(pid, node)
      result = Agent.get(pid, fn state -> state end)
      Agent.stop(pid)
      result
    end

    @doc "See whether this configuration uses a cache."
    @spec is_cached(pid()) :: boolean()
    def is_cached(this) do
      Agent.get(this, fn state -> !is_nil(state.cache) end)
    end

    @doc "Set whether this configuration uses a cache."
    @spec set_cached(pid(), boolean()) :: no_return()
    def set_cached(this, cached) do
      state = Agent.get(this, fn state -> state end)

      cond do
        cached && is_nil(state.cache) ->
          Agent.update(this, fn state -> %{state | cache: Map.new()} end)

        !cached && !is_nil(state.cache) ->
          Agent.update(this, fn state -> %{state | cache: nil} end)
      end
    end

    @doc """
    Get a value from this configuration by key or path, with an optional default value if not found.
    If no default is specified and a value isn't found, an error will be returned.
    """
    @spec get(pid(), binary(), any()) :: tuple()
    def get(this, key, default \\ :MISSING) do
      state = Agent.get_and_update(this, fn state -> {state, %{state | refs_seen: %{}}} end)

      result =
        cond do
          !is_nil(state.cache) && Map.has_key?(state.cache, key) ->
            {:ok, Map.get(state.cache, key)}

          is_nil(state.data) ->
            error(:no_configuration, %Location{}, this)

          Map.has_key?(state.data, key) ->
            evaluated(this, Map.get(state.data, key))

          is_identifier(key) ->
            if default == :MISSING do
              # Logger.debug("#{__ENV__.line}: #{key} not in #{inspect(Map.keys(state.data))}")
              error(:not_found, %Location{}, key)
            else
              {:ok, default}
            end

          true ->
            v = get_from_path(this, key)

            case v do
              {:ok, _} ->
                v

              {:error, e} ->
                if default == :MISSING do
                  v
                else
                  case e do
                    %RecognizerError{reason: :not_found} -> {:ok, default}
                    _ -> v
                  end
                end
            end
        end

      # Logger.debug("#{__ENV__.line}: get: #{name(this)}: #{key} -> #{inspect(result)}")
      result
    end

    defp shallow_unwrap(this, v) do
      result =
        case v do
          %MappingNode{} = mn ->
            wrap_mapping(this, mn)

          %ListNode{} = ln ->
            {:ok, List.to_tuple(ln.elements)}

          %Token{} = t ->
            evaluate(this, t)

          %UnaryNode{} ->
            evaluate(this, v)

          %BinaryNode{} ->
            evaluate(this, v)

          _ ->
            {:ok, v}
        end

      # Logger.debug("#{__ENV__.line}: s_u: #{name(this)}: #{inspect(result)}")
      result
    end

    defp map_access(this, map, key, start) do
      result =
        if !Map.has_key?(map, key) do
          # Logger.debug("#{__ENV__.line}: #{key} not in #{inspect(Map.keys(map))}")
          error(:not_found, start, key)
        else
          shallow_unwrap(this, Map.get(map, key))
        end

      # Logger.debug("#{__ENV__.line}: m_a: #{name(this)}: #{key} -> #{inspect(result)}")
      result
    end

    defp flag_error(pid, e) do
      Agent.update(pid, fn state -> %{state | error: e} end)
      {:halt, pid}
    end

    defp flag_error(pid, reason, loc, detail) do
      flag_error(pid, RecognizerError.exception(reason, loc, detail))
    end

    defp evaluate_optional(this, expr, default) do
      if is_nil(expr) do
        {:ok, default}
      else
        evaluate(this, expr)
      end
    end

    defp collect_slice(container, start, stop, step, result) do
      done =
        if step > 0 do
          start >= stop
        else
          start <= stop
        end

      # Logger.debug("#{__ENV__.line}: #{start}, #{stop}, #{step}, #{inspect result}, #{done}")
      if done do
        result
      else
        collect_slice(container, start + step, stop, step, result ++ [elem(container, start)])
      end
    end

    defp get_slice(this, container, sn) when is_tuple(container) do
      start_index = evaluate_optional(this, sn.start_index, 0)

      case start_index do
        {:error, _} ->
          start_index

        {:ok, start} ->
          size = tuple_size(container)

          start =
            if start < 0 do
              if start >= -size do
                start + size
              else
                0
              end
            else
              start
            end

          stop_index = evaluate_optional(this, sn.stop_index, size)

          case stop_index do
            {:error, _} ->
              stop_index

            {:ok, stop} ->
              stop =
                if stop > size do
                  size
                else
                  stop
                end

              stop =
                if stop < 0 do
                  if stop >= -size do
                    stop + size
                  else
                    0
                  end
                else
                  stop
                end

              step = evaluate_optional(this, sn.step, 1)

              case step do
                {:error, _} ->
                  step

                {:ok, 0} ->
                  error(:invalid_step, sn.step.start, 0)

                {:ok, step} ->
                  {start, stop} =
                    if step < 0 && start < stop do
                      stop =
                        if stop >= size do
                          size - 1
                        else
                          stop
                        end

                      start =
                        if start == 0 do
                          -1
                        else
                          start
                        end

                      {stop, start}
                    else
                      {start, stop}
                    end

                  {:ok, List.to_tuple(collect_slice(container, start, stop, step, []))}
              end
          end
      end
    end

    @doc "See if the specified value is a configuration."
    @spec is_config(any()) :: boolean()
    def is_config(v) do
      if !is_pid(v) do
        false
      else
        state = Agent.get(v, fn state -> state end)

        case state do
          %Config{} -> true
          _ -> false
        end
      end
    end

    defp get_from_path(this, key) when is_binary(key) do
      v = parse_path(key)

      case v do
        {:error, _} ->
          v

        {:ok, node} ->
          get_from_path(this, node)
      end
    end

    defp get_from_path(this, node) do
      path = unpack_path(node)
      first = List.first(path)
      kind = List.first(first)

      if kind != :DOT do
        error(:unexpected_path_start, node.start, first)
      else
        data = Agent.get(this, fn state -> state.data end)
        {:ok, pid} = Agent.start(fn -> %{error: nil, current: data, config: this} end)
        # iterate over the path elements
        _ =
          Enum.reduce_while(path, pid, fn item, pid ->
            {current, config} = Agent.get(pid, fn state -> {state.current, state.config} end)

            # Logger.debug(
            #   "#{__ENV__.line}: pit: #{name(config)}: item = #{inspect(item)}, current = #{
            #     inspect(current)
            #   }"
            # )

            case item do
              [:DOT, t = %Token{kind: :WORD}] ->
                # attribute access
                {key, start} = {t.text, t.start}

                case current do
                  %{} ->
                    v = map_access(config, current, key, start)

                    case v do
                      {:error, e} ->
                        flag_error(pid, e)

                      {:ok, v} ->
                        cfg =
                          if is_config(v) do
                            v
                          else
                            config
                          end

                        Agent.update(pid, fn state -> %{state | current: v, config: cfg} end)
                        {:cont, pid}
                    end

                  _ ->
                    if !is_config(current) do
                      flag_error(pid, :invalid_container, start, {key, current})
                    else
                      v = Config.get(current, key)

                      case v do
                        {:error, e} ->
                          flag_error(pid, e)

                        {:ok, v} ->
                          cfg =
                            if is_config(v) do
                              v
                            else
                              config
                            end

                          Agent.update(pid, fn state -> %{state | current: v, config: cfg} end)
                          {:cont, pid}
                      end
                    end
                end

              [:LBRACK, indexpr] ->
                # indexed access
                v = evaluate(config, indexpr)

                case v do
                  {:error, e} ->
                    flag_error(pid, e)

                  {:ok, iv} ->
                    cond do
                      is_binary(iv) ->
                        case current do
                          %{} ->
                            v = map_access(config, current, iv, indexpr.start)

                            case v do
                              {:error, e} ->
                                flag_error(pid, e)

                              {:ok, v} ->
                                Agent.update(pid, fn state -> %{state | current: v} end)
                                {:cont, pid}
                            end

                          _ ->
                            if !is_config(current) do
                              flag_error(pid, :invalid_container, indexpr.start, {iv, current})
                            else
                              v = Config.get(current, iv)

                              case v do
                                {:error, e} ->
                                  flag_error(pid, e)

                                {:ok, v} ->
                                  Agent.update(pid, fn state -> %{state | current: v} end)
                                  {:cont, pid}
                              end
                            end
                        end

                      is_integer(iv) ->
                        case current do
                          t when is_tuple(t) ->
                            size = tuple_size(t)

                            index =
                              if iv < 0 do
                                size + iv
                              else
                                iv
                              end

                            if index < 0 || index >= size do
                              flag_error(pid, :invalid_index, indexpr.start, {index, current})
                            else
                              v = shallow_unwrap(config, elem(t, index))

                              case v do
                                {:error, e} ->
                                  flag_error(pid, e)

                                {:ok, v} ->
                                  Agent.update(pid, fn state -> %{state | current: v} end)
                                  {:cont, pid}
                              end
                            end

                          _ ->
                            # Logger.debug("#{__ENV__.line}: #{inspect(current)}")
                            flag_error(pid, :invalid_container, indexpr.start, {iv, current})
                        end

                      true ->
                        flag_error(pid, :invalid_index, indexpr.start, v)
                    end
                end

              [:COLON, sn = %SliceNode{}] ->
                # slice access
                case current do
                  t when is_tuple(t) ->
                    v = get_slice(this, t, sn)

                    case v do
                      {:error, e} ->
                        flag_error(pid, e)

                      {:ok, v} ->
                        Agent.update(pid, fn state -> %{state | current: v} end)
                        {:cont, pid}
                    end

                  _ ->
                    flag_error(pid, :invalid_container, sn.start, {nil, current})
                end

              _ ->
                flag_error(pid, :not_implemented, __ENV__.line, item)
            end
          end)

        state = Agent.get(pid, fn state -> state end)
        Agent.stop(pid)

        if is_nil(state.error) do
          evaluated(state.config, state.current)
        else
          {:error, state.error}
        end
      end
    end

    defp evaluated(this, node) do
      v =
        case node do
          %Token{} ->
            evaluate(this, node)

          %UnaryNode{} ->
            v = evaluate(this, node)
            # Logger.debug("#{__ENV__.line}: #{inspect v}")
            v

          %BinaryNode{} ->
            evaluate(this, node)

          %ComplexNum{} ->
            {:ok, node}

          %MappingNode{} ->
            wrap_mapping(this, node)

          %ListNode{} ->
            as_list(this, node.elements)

          t when is_list(t) ->
            as_list(this, t)

          _ ->
            if is_integer(node) || is_float(node) || is_binary(node) || is_tuple(node) ||
                 is_map(node) || is_boolean(node) do
              {:ok, node}
            else
              error(:cannot_evaluate, %Location{}, node)
            end
        end

      result =
        case v do
          {:error, _} -> v
          {:ok, v} -> unwrap(this, v)
        end

      result
    end

    defp find_include(this, fname) do
      state = Agent.get(this, fn state -> state end)

      {found, path} =
        if Path.type(fname) == :absolute do
          {File.exists?(fname), fname}
        else
          d =
            if is_nil(state.root_dir) do
              File.cwd()
            else
              state.root_dir
            end

          p = Path.join(d, fname)

          if File.exists?(p) do
            {true, p}
          else
            # Logger.debug("#{__ENV__.line}: not found: #{p}")
            Enum.reduce_while(state.include_path, {false, fname}, fn d, acc ->
              p = Path.join(d, fname)

              if File.exists?(p) do
                {:halt, {true, p}}
              else
                # Logger.debug("#{__ENV__.line}: not found: #{p}")
                {:cont, acc}
              end
            end)
          end
        end

      if !found do
        {:error, nil}
      else
        v = Parser.from_file(path)

        case v do
          {:error, _} ->
            v

          {:ok, p} ->
            v = Parser.container(p)

            case v do
              {:error, _} ->
                v

              {:ok, node} ->
                case node do
                  mn = %MappingNode{} ->
                    # Create a new child config
                    {:ok, child} = Config.new(state)
                    # Logger.debug("#{__ENV__.line}: created child #{inspect child}")
                    set_path(child, path)

                    if is_cached(this) do
                      set_cached(child, true)
                    end

                    v = wrap_mapping(child, mn)
                    # Logger.debug("#{__ENV__.line}: #{inspect v}")

                    case v do
                      {:error, _} ->
                        v

                      {:ok, data} ->
                        Agent.update(child, fn state -> %{state | parent: this, data: data} end)
                        {:ok, child}
                    end

                  _ ->
                    v
                end
            end
        end
      end
    end

    defp eval_at(this, node) do
      v = evaluate(this, node)

      case v do
        {:error, _} ->
          v

        {:ok, key} ->
          if !is_binary(key) do
            error(:string_expected, node.start, key)
          else
            v = find_include(this, key)
            # Logger.debug("#{__ENV__.line}: #{inspect v}")
            case v do
              {:error, nil} ->
                error(:include_not_found, node.start, key)

              _ ->
                # Logger.debug("#{__ENV__.line}: #{inspect v}")
                v
            end
          end
      end
    end

    defp is_complex(v) do
      case v do
        %ComplexNum{} -> true
        _ -> false
      end
    end

    defp eval_add(this, node = %UnaryNode{}) do
      evaluate(this, node.operand)
    end

    defp eval_add(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          # Logger.debug("#{__ENV__.line}: #{inspect node.rhs}")
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_number(lhs) && is_number(rhs) ->
                  {:ok, lhs + rhs}

                is_binary(lhs) && is_binary(rhs) ->
                  {:ok, lhs <> rhs}

                is_complex(lhs) || is_complex(rhs) ->
                  cond do
                    is_complex(lhs) && is_complex(rhs) ->
                      {:ok, Complex.add(lhs, rhs)}

                    is_number(lhs) ->
                      {:ok, Complex.add(Complex.new(lhs, 0), rhs)}

                    is_number(rhs) ->
                      {:ok, Complex.add(lhs, Complex.new(rhs, 0))}

                    true ->
                      error(:cannot_add, node.start, {lhs, rhs})
                  end

                is_tuple(lhs) && is_tuple(rhs) ->
                  {:ok, List.to_tuple(Tuple.to_list(lhs) ++ Tuple.to_list(rhs))}

                is_map(lhs) && is_map(rhs) ->
                  {:ok, MapUtils.deep_merge(lhs, rhs)}

                true ->
                  error(:cannot_add, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_subtract(this, node = %UnaryNode{}) do
      v = evaluate(this, node.operand)

      case v do
        {:error, _} ->
          v

        {:ok, operand} ->
          cond do
            is_number(operand) -> {:ok, -operand}
            is_complex(operand) -> {:ok, Complex.minus(operand)}
            true -> error(:cannot_negate, node.start, operand)
          end
      end
    end

    defp eval_subtract(this, node = %BinaryNode{}) do
      # Logger.debug("#{__ENV__.line}: #{inspect node}")
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              # Logger.debug("#{__ENV__.line}: #{inspect lhs} - #{inspect rhs}")
              cond do
                is_number(lhs) && is_number(rhs) ->
                  {:ok, lhs - rhs}

                is_complex(lhs) || is_complex(rhs) ->
                  cond do
                    is_complex(lhs) && is_complex(rhs) ->
                      # Logger.debug("#{__ENV__.line}: #{inspect lhs} - #{inspect rhs}")
                      {:ok, Complex.sub(lhs, rhs)}

                    is_number(lhs) ->
                      {:ok, Complex.sub(Complex.new(lhs, 0), rhs)}

                    is_number(rhs) ->
                      {:ok, Complex.sub(lhs, Complex.new(rhs, 0))}

                    true ->
                      error(:cannot_subtract, node.start, {lhs, rhs})
                  end

                is_map(lhs) && is_map(rhs) ->
                  {:ok, Map.drop(lhs, Map.keys(rhs))}

                true ->
                  error(:cannot_subtract, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_multiply(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_number(lhs) && is_number(rhs) ->
                  {:ok, lhs * rhs}

                is_complex(lhs) || is_complex(rhs) ->
                  cond do
                    is_complex(lhs) && is_complex(rhs) ->
                      {:ok, Complex.mult(lhs, rhs)}

                    is_number(lhs) ->
                      {:ok, Complex.mult(Complex.new(lhs, 0), rhs)}

                    is_number(rhs) ->
                      {:ok, Complex.mult(lhs, Complex.new(rhs, 0))}

                    true ->
                      error(:cannot_multiply, __ENV__.line, {lhs, rhs})
                  end

                true ->
                  error(:not_implemented, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_divide(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_number(lhs) && is_number(rhs) ->
                  {:ok, lhs / rhs}

                is_complex(lhs) || is_complex(rhs) ->
                  cond do
                    is_complex(lhs) && is_complex(rhs) ->
                      {:ok, Complex.div(lhs, rhs)}

                    is_number(lhs) ->
                      {:ok, Complex.div(Complex.new(lhs, 0), rhs)}

                    is_number(rhs) ->
                      {:ok, Complex.div(lhs, Complex.new(rhs, 0))}

                    true ->
                      error(:cannot_divide, __ENV__.line, {lhs, rhs})
                  end

                true ->
                  error(:not_implemented, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_integer_divide(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              if is_integer(lhs) && is_integer(rhs) do
                {:ok, div(lhs, rhs)}
              else
                error(:cannot_integer_divide, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_modulo(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              if is_integer(lhs) && is_integer(rhs) do
                {:ok, Integer.mod(lhs, rhs)}
              else
                error(:cannot_compute_modulo, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_left_shift(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              if is_integer(lhs) && is_integer(rhs) do
                {:ok, lhs <<< rhs}
              else
                error(:cannot_left_shift, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_right_shift(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              if is_integer(lhs) && is_integer(rhs) do
                {:ok, lhs >>> rhs}
              else
                error(:cannot_right_shift, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_power(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_number(lhs) && is_number(rhs) ->
                  {:ok, Pow.pow(lhs, rhs)}

                is_complex(lhs) && is_integer(rhs) ->
                  {:ok, Complex.pow(lhs, rhs)}

                true ->
                  error(:cannot_raise_to_power, node.start, {lhs, rhs})
              end
          end
      end
    end

    defp eval_bitor(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_integer(lhs) && is_integer(rhs) ->
                  {:ok, lhs ||| rhs}

                is_map(lhs) && is_map(rhs) ->
                  {:ok, MapUtils.deep_merge(lhs, rhs)}

                true ->
                  error(:cannot_bitwise_or, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_bitand(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_integer(lhs) && is_integer(rhs) ->
                  {:ok, lhs &&& rhs}

                is_map(lhs) && is_map(rhs) ->
                  {:ok, Map.drop(lhs, Map.keys(rhs))}

                true ->
                  error(:cannot_bitwise_and, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_bitxor(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          v = evaluate(this, node.rhs)

          case v do
            {:error, _} ->
              v

            {:ok, rhs} ->
              cond do
                is_integer(lhs) && is_integer(rhs) ->
                  {:ok, bxor(lhs, rhs)}

                is_map(lhs) && is_map(rhs) ->
                  {:ok, Map.drop(lhs, Map.keys(rhs))}

                true ->
                  error(:cannot_bitwise_and, __ENV__.line, {lhs, rhs})
              end
          end
      end
    end

    defp eval_logical_or(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          if lhs do
            {:ok, true}
          else
            v = evaluate(this, node.rhs)

            case v do
              {:error, _} ->
                v

              {:ok, rhs} ->
                {:ok, !!rhs}
            end
          end
      end
    end

    defp eval_logical_and(this, node = %BinaryNode{}) do
      v = evaluate(this, node.lhs)

      case v do
        {:error, _} ->
          v

        {:ok, lhs} ->
          if !lhs do
            {:ok, false}
          else
            v = evaluate(this, node.rhs)

            case v do
              {:error, _} ->
                v

              {:ok, rhs} ->
                {:ok, !!rhs}
            end
          end
      end
    end

    defp eval_reference(this, node) do
      get_from_path(this, node.operand)
    end

    defp evaluate(this, node) do
      # Logger.debug("#{__ENV__.line}: eva? #{name(this)}: #{inspect(node)}")
      state = Agent.get(this, fn state -> state end)

      result =
        case node do
          %Token{} ->
            if MapSet.member?(state.scalar_tokens, node.kind) do
              {:ok, node.value}
            else
              case node.kind do
                :WORD ->
                  key = node.text

                  if !is_nil(state.context) && Map.has_key?(state.context, key) do
                    {:ok, Map.get(state.context, key)}
                  else
                    error(:unknown_variable, node.start, key)
                  end

                :BACKTICK ->
                  convert_string(this, node)
              end
            end

          %MappingNode{} ->
            v = wrap_mapping(this, node)

            case v do
              {:error, _} -> v
              {:ok, v} -> as_dict(this, v)
            end

          %ListNode{} ->
            as_list(this, node.elements)

          _ ->
            case node.kind do
              :AT ->
                eval_at(this, node.operand)

              :PLUS ->
                eval_add(this, node)

              :MINUS ->
                eval_subtract(this, node)

              :STAR ->
                eval_multiply(this, node)

              :SLASH ->
                eval_divide(this, node)

              :SLASHSLASH ->
                eval_integer_divide(this, node)

              :MODULO ->
                eval_modulo(this, node)

              :POWER ->
                eval_power(this, node)

              :DOLLAR ->
                eval_reference(this, node)

              :BITOR ->
                eval_bitor(this, node)

              :BITAND ->
                eval_bitand(this, node)

              :BITXOR ->
                eval_bitxor(this, node)

              :LSHIFT ->
                eval_left_shift(this, node)

              :RSHIFT ->
                eval_right_shift(this, node)

              :AND ->
                eval_logical_and(this, node)

              :OR ->
                eval_logical_or(this, node)

              _ ->
                error(:not_implemented, __ENV__.line, node)
            end
        end

      # Logger.debug("#{__ENV__.line}: eva! #{name(this)}: #{inspect(result)}")
      result
    end

    defp convert_string(this, node) do
      state = Agent.get(this, fn state -> state end)
      s = node.value
      v = state.string_converter.(s, this)
      # Logger.debug("convert_string: #{s} -> #{inspect(v)}")

      if s == v && state.strict_conversions do
        error(:conversion_failure, node.start, s)
      else
        {:ok, v}
      end
    end

    defp unwrap(this, v) do
      # Logger.debug("#{__ENV__.line}: unw? #{name(this)}: #{inspect(v, structs: false)}")

      result =
        case v do
          s when is_binary(s) ->
            {:ok, s}

          i when is_integer(i) ->
            {:ok, i}

          f when is_float(f) ->
            {:ok, f}

          b when is_boolean(b) ->
            {:ok, b}

          %DateTime{} ->
            {:ok, v}

          c = %ComplexNum{} ->
            {:ok, c}

          mn = %MappingNode{} ->
            v = wrap_mapping(this, mn)

            case v do
              {:error, _} -> v
              {:ok, v} -> as_dict(this, v)
            end

          ln = %ListNode{} ->
            as_list(this, ln.elements)

          %{} ->
            as_dict(this, v)

          t when is_tuple(t) ->
            as_list(this, t)

          t when is_list(t) ->
            as_list(this, t)

          _ ->
            if is_config(v) do
              {:ok, v}
            else
              error(:not_implemented, __ENV__.line, v)
            end
        end

      # Logger.debug("#{__ENV__.line}: unw! #{name(this)}: #{inspect result}")
      result
    end

    defp as_list(this, elements) when is_list(elements) do
      {:ok, pid} = Agent.start(fn -> %{data: [], error: nil} end)

      _ =
        Enum.reduce_while(elements, pid, fn value, pid ->
          ev = evaluated(this, value)

          case ev do
            {:error, e} ->
              Agent.update(pid, fn state -> %{state | error: e} end)
              {:halt, pid}

            {:ok, v} ->
              rv = unwrap(this, v)

              case rv do
                {:error, e} ->
                  Agent.update(pid, fn state -> %{state | error: e} end)
                  {:halt, pid}

                {:ok, result} ->
                  data = Agent.get(pid, fn state -> state.data end)
                  new_data = data ++ [result]
                  Agent.update(pid, fn state -> %{state | data: new_data} end)
                  {:cont, pid}
              end
          end
        end)

      state = Agent.get(pid, fn state -> state end)
      Agent.stop(pid)

      if is_nil(state.error) do
        {:ok, List.to_tuple(state.data)}
      else
        {:error, state.error}
      end
    end

    defp as_list(this, elements) when is_tuple(elements) do
      as_list(this, Tuple.to_list(elements))
    end

    defp as_dict(this, d) when is_map(d) do
      {:ok, pid} = Agent.start(fn -> %{data: %{}, error: nil} end)

      _ =
        Enum.reduce_while(d, pid, fn {key, value}, pid ->
          ev = evaluated(this, value)
          # Logger.debug("#{__ENV__.line}: #{key} -> #{inspect ev}")
          case ev do
            {:error, e} ->
              Agent.update(pid, fn state -> %{state | error: e} end)
              {:halt, pid}

            {:ok, v} ->
              rv = unwrap(this, v)

              case rv do
                {:error, e} ->
                  Agent.update(pid, fn state -> %{state | error: e} end)
                  {:halt, pid}

                {:ok, result} ->
                  data = Agent.get(pid, fn state -> state.data end)
                  new_data = Map.put(data, key, result)
                  Agent.update(pid, fn state -> %{state | data: new_data} end)
                  {:cont, pid}
              end
          end

          {:cont, pid}
        end)

      state = Agent.get(pid, fn state -> state end)
      Agent.stop(pid)

      if is_nil(state.error) do
        {:ok, state.data}
      else
        {:error, state.error}
      end
    end

    @doc "Return this configuration as a map, recursing into included configurations."
    @spec as_dict(pid()) :: {atom(), any()}
    def as_dict(this) do
      state = Agent.get(this, fn state -> state end)
      as_dict(this, state.data)
    end
  end
end
