module Utils

struct Symbol2
    name::String
end

# Define the Sym function to find or create unique Symbol2 entries in a symbol table
function Sym(s; symbol_table=Dict{String,Symbol2}())
    if !haskey(symbol_table, s)
        symbol_table[s] = Symbol2(s)
    end
    return symbol_table[s]
end

# Initialize an empty dictionary to serve as the symbol table
symbol_table = Dict{String, Symbol2}()

# List of predefined symbol names
symbols = ["quote", "if", "set!", "define", "lambda", "begin", "define-macro",
           "quasiquote", "unquote", "unquote-splicing"]

# Populate the symbol table with predefined symbols
for sym in symbols
    Sym(sym, symbol_table=symbol_table)
end

# Access specific symbols from the symbol table
_quote = symbol_table["quote"]
_if = symbol_table["if"]
_set = symbol_table["set!"]
_define = symbol_table["define"]
_lambda = symbol_table["lambda"]
_begin = symbol_table["begin"]
_definemacro = symbol_table["define-macro"]
_quasiquote = symbol_table["quasiquote"]
_unquote = symbol_table["unquote"]
_unquotesplicing = symbol_table["unquote-splicing"]

end # module Utils