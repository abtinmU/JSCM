module Prep

using ..Utils

function prep(x; toplevel=false)

    # Walk tree of x, making optimizations/fixes, and signaling SyntaxError.
    require(x, x != [])  # () => Error

    if !isa(x, Array)  # constant => unchanged
        return x

    elseif x[1] === _quote
        require(x, length(x) == 2)
        return x


    elseif x[1] === _if
        if length(x) == 3
            x = [x..., nothing]  # (if t c) => (if t c nothing)
        end
        require(x, length(x) == 4, "If expression error")
        return [_if, prep(x[2], toplevel=toplevel), prep(x[3], toplevel=toplevel), prep(x[4], toplevel=toplevel)]


    elseif x[1] === _set
        require(x, length(x) == 3)
        var = x[2]  # (set! non-var exp) => Error
        require(x, isa(var, Symbol2), "can set! only a symbol")
        return [_set, var, prep(x[3])]


    elseif x[1] === _define || x[1] === _definemacro
        require(x, length(x) >= 3, "Define form requires at least three parts")
        def_type, variable, body = x[1], x[2], x[3:end]

        if isa(variable, Array) && !isempty(variable)
            func_name, args = variable[1], variable[2:end]
            return prep([def_type, func_name, [_lambda, args, body...]])
        else
            require(x, isa(variable, Symbol2), "Can only define symbols")
            expr = prep(body[1])
            if def_type === _definemacro
                proc = eval2(expr)  # Evaluate to get the macro procedure
                require(x, Callable(proc), "Macro must be a procedure")
                setindex!(macro_table, proc, variable)
                return nothing
            end
            return [_define, variable, expr]
        end


    elseif x[1] === _let
        require(x, length(x) >= 3, "let expression requires at least bindings and one body expression")
        bindings, body = x[2], x[3:end]

        new_bindings = map(b -> begin
            require(b, isa(b, Vector) && length(b) == 2 && isa(b[1], Symbol2), "Invalid let binding format")
            return (b[1], prep(b[2], toplevel=toplevel))
        end, bindings)

        prep_body = map(b -> prep(b, toplevel=toplevel), [body])
        return let2(new_bindings, prep_body...)


    elseif x[1] === _begin
        if length(x) == 1
            return nothing  # (begin) => nothing
        else
            return [prep(xi) for xi in x]
        end


    elseif x[1] === _lambda  # (lambda (x) e1 e2) => (lambda (x) (begin e1 e2))
        require(x, length(x) >= 3)
        vars, body = x[2], x[3:end]
        require(x, (isa(vars, Array) && all(v -> isa(v, Symbol2), vars)) || isa(vars, Symbol2), "illegal lambda argument list")

        exp = length(body) == 1 ? body[1] : vcat([_begin], body)
        return [_lambda, vars, prep(exp)]


    elseif x[1] === _quasiquote  # `x => prep_quasiquote(x)
        require(x, length(x) == 2)
        return prep_quasiquote(x[2])


    elseif isa(x[1], Symbol2) && (x[1] ∈ keys(macro_table))
        return prep((macro_table[x[1]])(x[2:end]...))


    else  # (f arg...) => expand each
        return map(prep, x)
    end
end



function require(x, predicate::Bool, msg="wrong length")
    if !predicate
        error(to_string(x) * ": " * msg)
    end
end

_append, _cons, _let = Symbol2("append"), Symbol2("cons"), Symbol2("let")

function is_pair(x)
    isa(x, Array) && !isempty(x)
end


function prep_quasiquote(x)
    """Expand `x => 'x; `,x => x; `(,@x y) => (append x y) """
    if !is_pair(x)
        return [_quote, x]
    end
    require(x, x[1] !== _unquotesplicing, "can't splice here")
    if x[1] === _unquote
        require(x, length(x) == 2)
        return x[2]
    elseif is_pair(x[1]) && x[1][1] === _unquotesplicing
        require(x[1], length(x[1]) == 2)
        return [_append, x[1][2], prep_quasiquote(x[2:end])]
    else
        return [_cons, prep_quasiquote(x[1]), prep_quasiquote(x[2:end])]
    end
end


function to_string(x)
    if x === true
        return "#t"
    elseif x === false
        return "#f"
    elseif isa(x, Symbol2)  # Assuming Symbol2 is used for custom symbols
        return x.name
    elseif isa(x, String)
        # Use Julia's escape_string to handle special characters
        return "\"" * escape_string(x) * "\""
    elseif isa(x, Array)
        # Recursively convert array elements to strings and join with spaces
        return "(" * join(map(to_string, x), " ") * ")"
    elseif isa(x, Complex)
        # Replace Julia's 'im' with 'i' for complex numbers
        return replace(string(x), "im" => "i")
    else
        return string(x)
    end
end


function let2(bindings, body)
    require(bindings, all(b -> isa(b, Tuple) && length(b) == 2 && isa(b[1], Symbol2), bindings), "let2 expects bindings as tuples of (Symbol2, expression)")

    # Unpack the variables and expressions from bindings
    vars = [b[1] for b in bindings]
    vals = [prep(b[2]) for b in bindings]  # Ensure values are expanded

    # Prepare the body: if there are multiple expressions, wrap them in a 'begin'
    prep_body = length(body) > 1 ? map(prep, body) : prep(body[1])

    # Construct the lambda expression
    lambda_expr = [_lambda, vars, prep_body]

    return [lambda_expr, vals...]
end

macro_table = Dict{Any, Any}(Sym("let") => let2)

function unzip(bindings)
    vars = [b[1] for b in bindings]
    vals = [b[2] for b in bindings]
    return vars, vals
end


function Callable(x)
    # Check if the typeof(x) has any methods defined for call syntax
    return !isempty(methods(x))
end


# using Test
#
#  @testset "Scheme Interpreter Tests" begin
#      @testset "prep function tests" begin
#          @test begin
#              result = prep(42)
#              println("prep(42) gives the result: $result \n")
#              result == 42
#          end
#  
#          @test begin
#              result = prep([_quote, Sym("exp")])
#              println("prep([_quote, 'exp']) gives the result: $result \n")
#              result == [_quote, Sym("exp")]
#          end
#  
#          @test begin
#              result = prep([_if, Sym("test"), Sym("consequence"), Sym("alternative")])
#              println("prep([_if, 'test', 'consequence', 'alternative']) gives the result: $result \n")
#              result == [_if, Sym("test"), Sym("consequence"), Sym("alternative")]
#          end
#  
#          @test begin
#              result = prep([_if, Sym("test"), Sym("consequence")])
#              println("prep([_if, 'test', 'consequence']) gives the result: $result \n")
#              result == [_if, Sym("test"), Sym("consequence"), nothing]
#          end
#  
#          @test begin
#              result = prep([_set, Sym("var"), 42])
#              println("prep([_set, Sym('var'), 42]) gives the result: $result \n")
#              result == [_set, Sym("var"), 42]
#          end
#  
#          @test begin
#              result = prep([_define, Sym("var"), 42])
#              println("prep([_define, Sym('var'), 42]) gives the result: $result \n")
#              result == [_define, Sym("var"), 42]
#          end
#  
#          @test begin
#              result = prep([_define, [Sym("f"), Sym("arg")], "body"])
#              println("prep([_define, [Sym('f'), Sym('arg')], 'body']) gives the result: $result \n")
#              result == [_define, Sym("f"), [_lambda, [Sym("arg")], "body"]]
#          end
#  
#          @test begin
#              result = prep([_begin, 42, Sym("exp")])
#              println("prep([_begin, 42, 'exp']) gives the result: $result \n")
#              result == [_begin, 42, Sym("exp")]
#          end
#  
#          @test begin
#              result = prep([_lambda, [Sym("arg")], Sym("body")])
#              println("prep([_lambda, [Sym('arg')], 'body']) gives the result: $result \n")
#              result == [_lambda, [Sym("arg")], Sym("body")]
#          end
#  
#          @test begin
#              result = prep_quasiquote([_unquote, Sym("exp")])
#              println("prep_quasiquote([_unquote, 'exp']) gives the result: $result \n")
#              result == Sym("exp")
#          end
#  
#  
#          @test begin
#              result = let2([(Sym("x"), 42), (Sym("y"), Sym("exp"))], "body")
#              println("let2([[Sym('x'), 42], [Sym('y'), 'exp']], 'body') gives the result: $result \n")
#              result == [[_lambda, [Sym("x"), Sym("y")], "body"], 42, Sym("exp")]
#          end
#      end
#  end

end # module prep