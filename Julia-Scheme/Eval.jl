module Eval

using ..env

function eval2(x, env=global_env)

    while true
        if isa(x, Symbol2)  # Variable reference or built-in operator
            var_name = x.name
            # Attempt to find the variable in the environment chain
            found_env = find(env, x)
            if found_env !== nothing
                return found_env.mappings[var_name]
            elseif haskey(global_env.mappings, var_name)  # Check the global environment for built-in functions/operators
                return global_env.mappings[var_name]
            else
                throw(KeyError("Variable or operator $var_name not found"))
            end

        elseif !isa(x, Vector)  # Constant literal
            if x==1
              return true
            else
              return x
            end

        elseif x[1] === _quote  # (quote exp)
            return x[2]

        elseif x[1] === _if  # (if test conseq alt)
            test, conseq, alt = x[2], x[3], x[4]
            x = eval2(test, env) ? conseq : alt

        elseif x[1] === _set  # (set! var exp)
            var, exp = x[2], x[3]
            find(env, var)[var.name] = eval2(exp, env)
            return nothing

        elseif x[1] === _define  # (define var exp)
            var, exp = x[2], x[3]
            env.mappings[var.name] = eval2(exp, env)
            return nothing

        elseif x[1] === _lambda  # (lambda (var*) exp)
            vars, exp = x[2], x[3]
            return Procedure(vars, exp, env)

        elseif x[1] === _begin  # (begin exp+)
            for exp in x[2:end-1]
                eval2(exp, env)
            end
            x = x[end]

        else  # (proc exp*)z
            proc = eval2(x[1], env)
            exps = [eval2(exp, env) for exp in x[2:end]]
            if isa(proc, Procedure)
                x = proc.exp
                env = Env(proc.parms, exps, proc.env)
            else
                return proc(exps...)
            end
        end

    end
end

# Represent a Scheme procedure (lambda)
struct Procedure
    parms::Any  # Parameters of the lambda
    exp::Any  # Body of the lambda
    env::Env  # Environment where the lambda was defined
end

# Make Procedure instances callable
function (proc::Procedure)(args...)
    # Create a new environment that extends proc.env with bindings from parms to args
    lambda_env = Env(proc.parms, args, proc.env)
    return eval2(proc.exp, lambda_env)  # Evaluate the procedure's body in the new environment
end

end # module Eval
