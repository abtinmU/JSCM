module Repl

import ..Parser: eof_object, tokenizer ,InPort, next_token, quotes, read2, parse2, fix_for_macro, atom
import ..Eval: eval2, Procedure

function load(filename)
    inport = InPort(open(filename, "r"))
    try
        repl(nothing, inport, stdout)
    finally
        close(inport.file)
    end
end

function repl(prompt::Union{Nothing, String}="jscm>>> ", inport::InPort=InPort(stdin), out::IO=stdout)
    println(stderr, "Basic Julia Scheme Interpreter")
    while true
        try
            if !isnothing(prompt)
                write(stderr, prompt)
            end
            x = parse2(inport)
            if x === eof_object
                return
            end
            val = eval2(x)
            if !isnothing(val)
                println(out, to_string(val))
            end
        catch e
            println(stderr, "$(typeof(e).__name__): $e")
        end
    end
end

end # module Repl