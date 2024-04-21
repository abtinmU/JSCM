module Parser

using ..LambdaCalculus

using Base: readline, match

# EOF object representation
const eof_object = Symbol2("#<eof-object>")

# Tokenizer pattern
tokenizer = r"""\s*(,@|[('`,)]|"(?:[\\].|[^\\"])*"|;.*|[^\s('"`,;)]*)(.*)"""

# InPort class equivalent in Julia
mutable struct InPort
    file::IO
    line::String
    InPort(file::IO) = new(file, "")
end

function next_token(inport::InPort)
    while true
        if isempty(inport.line)
            inport.line = readline(inport.file, keep=true)
        end

        if isempty(inport.line)
            return eof_object
        end

        match_result = Base.match(tokenizer, inport.line)  # Use Base.match explicitly
        if match_result !== nothing
            token, inport.line = match_result.captures
            if !isempty(token) && !startswith(token, ";")
                return token
            end
        end
    end
end

# Quotes mapping
quotes = Dict("'" => _quote, "`" => _quasiquote, "," => _unquote, ",@" => _unquotesplicing)

function read2(inport::InPort)
    function read_ahead(token)
        if token == "("
            L = []
            while true
                token = next_token(inport)
                if token == ")"
                    return L
                else
                    push!(L, read_ahead(token))
                end
            end
        elseif token == ")"
            error("unexpected )")
        elseif haskey(quotes, token)
            return [quotes[token], read2(inport)]
        elseif token === eof_object
            error("unexpected EOF in list")
        else
            return atom(String(token))
        end
    end
    token1 = next_token(inport)
    return token1 === eof_object ? eof_object : read_ahead(token1)
end

function parse2(inport::Union{String, InPort})

    if isa(inport, String)
        fix_for_macro(inport)
        inport = InPort(IOBuffer(inport))
    end
    return prep(read2(inport), toplevel=true)
end

function fix_for_macro(x)
    if isa(x, String)
      if contains(String(x), "define-macro")
        global_env["cons"] = (x, y) -> y==[] ? x : isa(x, Symbol2) ? [x, y...] : [x, y]
        global_env["car"] = x -> [first(x)]
      end
    end
end

function atom(token::String)
    if token == "#t"
        return true
    elseif token == "#f"
        return false
    elseif startswith(token, "\"")
        return unescape_string(token[2:end-1])
    else
        try
            return parse(Int, token)
        catch
            try
                return parse(Float64, token)
            catch
                try
                    return parse(Complex{Float64}, replace(token, "i" => "im"))
                catch
                    return Sym(token, symbol_table=symbol_table)
                end
            end
        end
    end
end

# tests = [
#     "(quote (testing 1 (2.0) -3.14e159))",
#     "(+ 2 2)",
#     "(+ (* 2 100) (* 1 10))",
#     "(if (> 6 5) (+ 1 1) (+ 2 2))",
#     "(if (< 6 5) (+ 1 1) (+ 2 2))",
#     "(define x 3)",
#     "x",
#     "(+ x x) => 6",
#     "(begin (define x 1) (set! x (+ x 1)) (+ x 1))",
#     "((lambda (x) (+ x x)) 5) => 10",
#     "(define twice (lambda (x) (* 2 x)))",
#     "(twice 5)",
#     "(define compose (lambda (f g) (lambda (x) (f (g x)))))",
#     "((compose list twice) 5) => (10)",
#     "(define repeat (lambda (f) (compose f f)))",
#     "((repeat twice) 5) => 20",
#     "((repeat (repeat twice)) 5) => 80",
#     "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))",
#     "(fact 3)",
#     "(fact 10)",
#     "(define abs (lambda (n) ((if (> n 0) + -) 0 n)))",
#     "(list (abs -3) (abs 0) (abs 3))",
#     "(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y)))))))",
#     "(define zip (combine cons))",
#     "(zip (list 1 2 3 4) (list 5 6 7 8))",
#     "(define riff-shuffle (lambda (deck) (begin (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq))))) (define mid (lambda (seq) (/ (length seq) 2))) ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))",
#     "(riff-shuffle (list 1 2 3 4 5 6 7 8))",
#     "((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))",
#     "(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))"
# ]
# 
# # Assuming the 'parse' function and 'tests' list are already defined as per previous discussions
# 
# println("Parser Tests")
# for test in tests
#     println("Parse>>> ", test)
#     println(parse2(test))
#     println("")
# end

end # module Parser