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

#  tests = [
#      "(quote (testing 1 (2.0) -3.14e159)) => (testing 1 (2.0) -3.14e+159)",
#      "(+ 2 2) => 4",
#      "(+ (* 2 100) (* 1 10)) => 210",
#      "(if (> 6 5) (+ 1 1) (+ 2 2)) => 2",
#      "(if (< 6 5) (+ 1 1) (+ 2 2)) => 4",
#      "(define x 3) => nothing",
#      "x => 3",
#      "(+ x x) => 6",
#      "(begin (define x 1) (set! x (+ x 1)) (+ x 1)) => 3",
#      "((lambda (x) (+ x x)) 5) => 10",
#      "(define twice (lambda (x) (* 2 x))) => nothing",
#      "(twice 5) => 10",
#      "(define compose (lambda (f g) (lambda (x) (f (g x))))) => nothing",
#      "((compose list twice) 5) => (10)",
#      "(define repeat (lambda (f) (compose f f))) => nothing",
#      "((repeat twice) 5) => 20",
#      "((repeat (repeat twice)) 5) => 80",
#      "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) => nothing",
#      "(fact 3) => 6",
#      "(fact 10) => 3628800",
#      "(define abs (lambda (n) ((if (> n 0) + -) 0 n))) => nothing",
#      "(list (abs -3) (abs 0) (abs 3)) => (3 0 3)",
#      "(define combine (lambda (f) (lambda (x y) (if (null? x) (quote ()) (f (list (car x) (car y)) ((combine f) (cdr x) (cdr y))))))) => nothing",
#      "(define zip (combine cons)) => nothing",
#      "(zip (list 1 2 3 4) (list 5 6 7 8)) => ((1 5) (2 6) (3 7) (4 8))",
#      "(define riff-shuffle (lambda (deck) (begin (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq)))))) (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq))))) (define mid (lambda (seq) (/ (length seq) 2))) ((combine append) (take (mid deck) deck) (drop (mid deck) deck))))) => None",
#      "(riff-shuffle (list 1 2 3 4 5 6 7 8)) => (1 5 2 6 3 7 4 8)",
#      "((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8)) => (1 3 5 7 2 4 6 8)",
#      "(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8)))) => (1 2 3 4 5 6 7 8)",
#      "(define (twice x) (* 2 x)) => nothing",
#      "(twice 2) => 4",
#      "(define lyst (lambda items items)) => nothing",
#      "(lyst 1 2 3 (+ 2 2)) => (1 2 3 4)",
#      "(if 1 2) => 2",
#      "(if (= 3 4) 2) => nothing",
#      "(define ((account bal) amt) (set! bal (+ bal amt)) bal) => nothing",
#      "(define a1 (account 100)) => nothing",
#      "(a1 0) => 100",
#      "(a1 10) => 110",
#      "(a1 10) => 120",
#      "(define (newton guess function derivative epsilon) (define guess2 (- guess (/ (function guess) (derivative guess)))) (if (< (abs (- guess guess2)) epsilon) guess2 (newton guess2 function derivative epsilon))) => None",
#      "(define (square-root a) (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8)) => nothing",
#      "(> (square-root 200.) 14.14213) => #t",
#      "(< (square-root 200.) 14.14215) => #t",
#      "(= (square-root 200.) (sqrt 200.)) => #t",
#      "(define (sum-squares-range start end) (define (sumsq-acc start end acc) (if (> start end) acc (sumsq-acc (+ start 1) end (+ (* start start) acc)))) (sumsq-acc start end 0)) => None",
#      "(sum-squares-range 1 3000) => 9004500500",
#      "(call/cc (lambda (throw) (+ 5 (* 10 (throw 1))))) ;; throw => 1",
#      "(call/cc (lambda (throw) (+ 5 (* 10 1)))) ;; do not throw => 15",
#      "(call/cc (lambda (throw) (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3)))))))) ; 1 level => 35",
#      "(call/cc (lambda (throw) (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3)))))))) ; 2 levels => 3",
#      "(call/cc (lambda (throw) (+ 5 (* 10 (call/cc (lambda (escape) (* 100 1))))))) ; 0 levels => 1005",
#      "(* 1i 1i) => (-1+0i)",
#      "(sqrt -1) => 1i",
#      "(let ((a 1) (b 2)) (+ a b)) => 3",
#  #    "(and 1 2 3) => 3",
#  #    "(and (> 2 1) 2 3) => 3",
#  #    "(and) => #t",
#  #    "(and (> 2 1) (> 2 3)) => #f",
#      "(define-macro unless (lambda args `(if (not ,(car args)) (begin ,@(cdr args))))) ; test ` => nothing",
#      "(unless (= 2 (+ 1 1)) (display 2) 3 4) => nothing",
#      "(unless (= 4 (+ 1 1)) 4) => 4",
#      "(quote x) => x",
#      "(quote (1 2 three)) => (1 2 three)"
#  ]
#  
#  println("scm simulation: Tests")
#  for test in tests
#      # Splitting each test case into the input part and the expected output part
#      parts = split(test, " => ")
#      input_part = String(parts[1])
#      output_part = String(parts[2])
#  
#      println("jscm>>> ", input_part, "   ; scheme output: ", output_part)
#      println(eval2(parse2(input_part)))
#      println("")
#  end

# errorTests = [
#     "() => SyntaxError (): wrong length",
#     "(set! x) => SyntaxError (set! x): wrong length",
#     "(define 3 4) => SyntaxError (define 3 4): can define only a symbol",
#     "(quote 1 2) => SyntaxError (quote 1 2): wrong length",
#     "(if 1 2 3 4) => SyntaxError (if 1 2 3 4): wrong length",
#     "(lambda 3 3) => SyntaxError (lambda 3 3): illegal lambda argument list",
#     "(lambda (x)) => SyntaxError (lambda (x)): wrong length",
#     "(if (= 1 2) (define-macro a 'a) (define-macro a 'b)) => SyntaxError (define-macro a (quote a)): define-macro only allowed at top level",
#     "(twice 2 2) => TypeError expected (x), given (2 2)",
#     "(let ((a 1) (b 2 3)) (+ a b)) => SyntaxError (let ((a 1) (b 2 3)) (+ a b)): illegal binding list"
# ]

end # module Eval