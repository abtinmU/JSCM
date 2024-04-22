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

end # module Parser
