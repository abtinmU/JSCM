module env

import ..Utils: Symbol2, Sym, symbol_table, _quote,_if,_set,_define,_lambda,_begin,_definemacro,_quasiquote,_unquote,_unquotesplicing
import ..Parser: eof_object, tokenizer ,InPort, next_token, quotes, read2, parse2, fix_for_macro, atom

mutable struct Env
    mappings::Dict{Any, Any}
    outer::Union{Env, Nothing}

    function Env(parms=(), args=(), outer=nothing)
        new_env = new(Dict{Any, Any}(), outer)

        if isa(parms, Symbol2)
            new_env.mappings[parms.name] = args
        else
            # Assume parms is a collection of parameters
            if length(args) != length(parms)
                throw(TypeError("Expected $(length(parms_array)), given $(length(args_array))"))
            end
            # Bind each parameter to its corresponding argument
            for (parm, arg) in zip(parms, args)
                new_env.mappings[parm.name] = arg
            end
        end
        return new_env
    end
end

Base.getindex(env::Env, key) = get(env.mappings, key, nothing)
Base.setindex!(env::Env, value, key) = setindex!(env.mappings, value, key)

# Define a mutable struct to use for our custom continuation exception
mutable struct ContinuationException <: Exception
    retval::Any  # Store the return value here
end

# This function will be passed to the user-defined procedure to allow escaping
function throw_continuation(retval)
    throw(ContinuationException(retval))  # Throw the exception with the retval
end

# The callcc function, which simulates call-with-current-continuation
function callcc(proc)
    try
        return proc(throw_continuation)  # Pass the throw function to the user procedure
    catch e
        if isa(e, ContinuationException)
            return e.retval  # Return the stored retval when the specific exception is caught
        else
            rethrow(e)  # If any other exception occurs, rethrow it
        end
    end
end

struct LookupError <: Exception
    msg::String
end

function find(env::Env, var)
    # Check if the variable is in the current environment
    if haskey(env.mappings, var.name)
        return env
    elseif env.outer === nothing
        # If there's no outer environment, throw an error indicating the variable was not found
        throw(KeyError("Variable $var not found"))
    else
        # Recursively search in the outer environment
        return find(env.outer, var)
    end
end

islist(x) = isa(x, Array)
isbool(x) = isa(x, Bool)
issymbol(x) = isa(x, Symbol)
ispair(x) = isa(x, Array) && length(x) == 2
isnumber(x) = isa(x, Number)
isstring(x) = isa(x, AbstractString)
isio(x) = isa(x, IO)
cons(x, y) = [x; y]

function add_globals(env::Env)
    # Math and logical operators
    env["+"] = +
    env["-"] = -
    env["*"] = *
    env["/"] = /

    env[">"] = >
    env["<"] = <
    env[">="] = >=
    env["<="] = <=
    env["="] = ==

    # Type-checking functions
    env["list?"] = islist
    env["boolean?"] = isbool
    env["symbol?"] = issymbol
    env["pair?"] = ispair
    env["number?"] = isnumber
    env["string?"] = isstring
    env["port?"] = isio

    # List operations
    env["not"] = (x) -> !x
    env["cons"] = cons
    env["car"] = x -> first(x)
    env["cdr"] = x -> x[2:end]
    env["append"] = (x, y...) -> [x;y...]
    env["list"] = (x...) -> [x...]
    env["null?"] = isempty
    env["sqrt"] = (x) -> if x>=0 sqrt(x) else sqrt(Complex(x)) end

    # File operations (simplified examples)
    env["open-input-file"] = open
    env["close-input-port"] = close
    env["open-output-file"] = f -> open(f, "w")
    env["close-output-port"] = close

    # I/O operations
    env["read"] = read
    env["write"] = write
    env["display"] = println

    # Other functionalities (apply, eval, load, call/cc) need custom implementation
    env["apply"] = (proc, args) -> proc(args...)
    env["eval"] = (x) -> eval2(prep(x))
    env["load"] = (fn) -> load(fn)
    env["call/cc"] = callcc
    env["length"] = length

    return env
end

global_env = add_globals(Env())

end # module env