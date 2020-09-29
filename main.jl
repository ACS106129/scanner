include("scan.jl")
include("id_error.jl")
include("token_type.jl")

using Printf
using .Scan: Scanner
using .TokenType: Token
using .IDError: UnIdentifierError

# start analysis stream
function analysis!(sc::Scanner)::Array{Tuple{Token,Union{Any,Nothing}},1}
    tokens = Array{Tuple{Token,Union{Any,Nothing}},1}(undef, 0)
    errBuf, c = "", Scan.next(sc)
    while c !== nothing
        try
            if !isempty(errBuf)
                push!(tokens, (TokenType.UNIDENTIFIER, errBuf))
                errBuf = ""
            end
            if sc.state == Scan.START
                start!(sc, c, tokens)
            # deprecated
            # elseif sc.state == Scan.PREPROCESS
            #     preprocess!(sc, c, tokens)
            elseif sc.state == Scan.IDENTIFIER
                identifier!(sc, c, tokens)
            elseif sc.state == Scan.CONSTANT
                constant!(sc, c, tokens)
            elseif sc.state == Scan.SLASH_MODE
                slash!(sc, c, tokens)
            end
        catch ex
            # if catch unidentifier, reset the sign, specifier and the state of scan mode
            if isa(ex, UnIdentifierError)
                sc.sign = nothing
                sc.specifier = nothing
                sc.state = Scan.START
                while c != '\r' && c != '\n' && c !== nothing
                    c = Scan.next(sc)
                end
                errBuf *= sprint(showerror, ex)
            else 
                for (exc, bt) in Base.catch_stack()
                    showerror(stdout, exc, bt)
                    println()
                end
            end
        end
        c = Scan.next(sc, true)
    end
    # end of stream tokens
    push!(tokens, (TokenType.EOS_TOKEN, nothing))
    return tokens
end

function list_tokens(io::IO, tokens::Array{Tuple{Token,Union{Any,Nothing}},1})
    rws = filter(x->Int(x[1]) >= 0 && Int(x[1]) <= 17, tokens)
    rwTable = [(typeToString(u), count(x->x[1] == u, rws)) for u in unique(rw[1] for rw in rws)]
    lns = filter(x->Int(x[1]) == 30, tokens)
    lnTable = [(u, count(x->x[2] == u, lns)) for u in unique(ln[2] for ln in lns)]
    coms = filter(x->Int(x[1]) >= 50 && Int(x[1]) <= 51, tokens)
    comTable = [(typeToString(u), count(x->x == u, coms)) for u in unique(com for com in coms)]
    ids = filter(x->Int(x[1]) == 60, tokens)
    idTable = [(u, count(x->x[2] == u, ids)) for u in unique(id[2] for id in ids)]
    cons = filter(x->Int(x[1]) >= 80 && Int(x[1]) <= 83, tokens)
    conTable = [(u, count(x->x[2] == u, cons)) for u in unique(con[2] for con in cons)]
    ops = filter(x->Int(x[1]) >= 100 && Int(x[1]) <= 126, tokens)
    opTable = [(typeToString(u), count(x->x[1] == u, ops)) for u in unique(op[1] for op in ops)]
    comps = filter(x->Int(x[1]) >= 150 && Int(x[1]) <= 156, tokens)
    compTable = [(typeToString(u), count(x->x[1] == u, comps)) for u in unique(comp[1] for comp in comps)]
    bks = filter(x->Int(x[1]) >= 170 && Int(x[1]) <= 175, tokens)
    bkTable = [(typeToString(u), count(x->x[1] == u, bks)) for u in unique(bk[1] for bk in bks)]
    fsTokens = []
    for seq in filter(x->Int(x[1]) == 71, tokens)
        push!(fsTokens, seq[2]...)
    end
    fsTable = [(typeToString(u), count(x->x == u, fsTokens)) for u in unique(fs for fs in filter(x->begin
        if isa(x, TokenType.Token)
            return Int(x) >= 180 && Int(x) <= 213
        else
            return Int(x[1]) >= 180 && Int(x[1]) <= 213
        end
    end, fsTokens))]
    pTable = [(typeToString(u), count(x->x == u, tokens)) for u in unique(token for token in filter(x->Int(x[1]) == 61, tokens))]
    aTable = [(typeToString(u), count(x->x == u, tokens)) for u in unique(token for token in filter(x->Int(x[1]) == 62, tokens))]
    puncs = filter(x->Int(x[1]) >= 250 && Int(x[1]) <= 255, tokens)
    puncTable = [(typeToString(u), count(x->x[1] == u, puncs)) for u in unique(punc[1] for punc in puncs)]
    push!(puncTable, [(typeToString(u), count(x->x == u, fsTokens)) for u in unique(fs for fs in filter(x->isa(x, TokenType.Token) && Int(x) >= 250 && Int(x) <= 255, fsTokens))]...)
    printTokens = []
    for seq in filter(x->isa(x, Tuple) && Int(x[1]) == 70, fsTokens)
        push!(printTokens, filter(x->!isempty(x), [seq[2]...])...)
    end
    printTable = [(u, count(x->x == u, printTokens)) for u in unique(printTokens)]
    unids = filter(x->Int(x[1]) == 69, tokens)
    unidTable = [(u, count(x->x[2] == u, unids)) for u in unique(unid[2] for unid in unids)]

    println(io, @sprintf("Total: %d tokens\r\n", sum(x->x[2], [rwTable..., lnTable..., comTable..., idTable..., conTable..., opTable..., compTable..., bkTable..., fsTable..., pTable..., aTable..., puncTable..., printTable..., unidTable...])))
    printType(io, "Reserved name: ", rwTable)
    printType(io, "Library name: ", lnTable)
    printType(io, "Comment: ", comTable)
    printType(io, "Identifier: ", idTable)
    printType(io, "Constant: ", conTable)
    printType(io, "Operator: ", opTable)
    printType(io, "Comparator: ", compTable)
    printType(io, "Bracket: ", bkTable)
    printType(io, "Format specifier: ", fsTable)
    printType(io, "Pointer: ", pTable)
    printType(io, "Address: ", aTable)
    printType(io, "Punctuation: ", puncTable)
    printType(io, "Printed token: ", printTable)
    printType(io, "Unidentifier: ", unidTable)
end

function printType(io::IO, description::String, table::Array)
    if !isempty(table)
        println(io, description * @sprintf("%d\r\n%s\r\n", sum(x->x[2], table), join([string(t[1]) * "\t(x" * string(t[2]) * ')' for t in table], '\n')))
    end
end

function typeToString(type::Union{Tuple{TokenType.Token,Any},TokenType.Token})::String
    if isa(type, TokenType.Token)
        if Int(type) >= 0 && Int(type) <= 17
            return lowercase(string(type))
        elseif Int(type) == 100
            return "+"
        elseif Int(type) == 101
            return "-"
        elseif Int(type) == 102
            return "*"
        elseif Int(type) == 103
            return "/"
        elseif Int(type) == 104
            return "%"
        elseif Int(type) == 105
            return "="        
        elseif Int(type) == 106
            return "!"        
        elseif Int(type) == 107
            return "&"        
        elseif Int(type) == 108
            return "|"        
        elseif Int(type) == 109
            return "^"       
        elseif Int(type) == 110
            return "~"        
        elseif Int(type) == 111
            return "++"        
        elseif Int(type) == 112
            return "--"        
        elseif Int(type) == 113
            return "+="        
        elseif Int(type) == 114
            return "-="       
        elseif Int(type) == 115
            return "*="
        elseif Int(type) == 116
            return "/="
        elseif Int(type) == 117
            return "%="
        elseif Int(type) == 118
            return "&="
        elseif Int(type) == 119
            return "|="
        elseif Int(type) == 120
            return "^="
        elseif Int(type) == 121
            return "<<"
        elseif Int(type) == 122
            return ">>"
        elseif Int(type) == 123
            return "<<="
        elseif Int(type) == 124
            return ">>="
        elseif Int(type) == 125
            return "&&"
        elseif Int(type) == 126
            return "||"
        elseif Int(type) == 150
            return "=="
        elseif Int(type) == 151
            return "==="
        elseif Int(type) == 152
            return ">"
        elseif Int(type) == 153
            return "<"
        elseif Int(type) == 154
            return ">="
        elseif Int(type) == 155
            return "<="
        elseif Int(type) == 156
            return "!="
        elseif Int(type) == 170
            return "("
        elseif Int(type) == 171
            return ")"
        elseif Int(type) == 172
            return "{"
        elseif Int(type) == 173
            return "}"
        elseif Int(type) == 174
            return "["
        elseif Int(type) == 175
            return "]"
        elseif Int(type) == 200
            return "\\a"
        elseif Int(type) == 201
            return "\\b"
        elseif Int(type) == 202
            return "\\e"
        elseif Int(type) == 203
            return "\\f"
        elseif Int(type) == 204
            return "\\n"
        elseif Int(type) == 205
            return "\\r"
        elseif Int(type) == 206
            return "\\t"
        elseif Int(type) == 207
            return "\\v"
        elseif Int(type) == 208
            return "\\\\"
        elseif Int(type) == 209
            return "\\'"
        elseif Int(type) == 210
            return "\\\""
        elseif Int(type) == 211
            return "\\?"
        elseif Int(type) == 250
            return ","
        elseif Int(type) == 251
            return ";"
        elseif Int(type) == 252
            return ":"
        elseif Int(type) == 253
            return "#"
        elseif Int(type) == 254
            return "'"
        elseif Int(type) == 255
            return "\""
        end
    elseif isa(type, Tuple)
        if Int(type[1]) == 50
            return "//" * type[2]
        elseif Int(type[1]) == 51
            return "/*" * type[2] * "*/"
        elseif Int(type[1]) == 61
            return "*" * type[2]
        elseif Int(type[1]) == 62
            return "&" * type[2]
        elseif Int(type[1]) == 180
            return "%" * type[2] * "d"
        elseif Int(type[1]) == 181
            return "%" * type[2] * "l"
        elseif Int(type[1]) == 182
            return "%" * type[2] * "f"
        elseif Int(type[1]) == 183
            return "%" * type[2] * "lf"
        elseif Int(type[1]) == 184
            return "%" * type[2] * "c"
        elseif Int(type[1]) == 185
            return "%" * type[2] * "s"
        elseif Int(type[1]) == 212
            return "\\" * type[2]
        elseif Int(type[1]) == 213
            return "\\u" * type[2]
        end
    end
end

@timev for filename in ARGS
    if !isfile(filename)
        continue
    end
    println(@sprintf("Start scanning file %s...", filename))
    str = open(filename) do file
        read(file, String)
    end
    io = open("scanner_" * replace(filename, r"\.(txt|c)$" => s".txt"), "w+")
    tokens = analysis!(Scanner(str))
    for token in tokens
        if token[1] in [TokenType.LN_TOKEN, TokenType.EOS_TOKEN]
            println(io, token)
        else
            print(io, token)
        end
    end
    println(io, @sprintf("### LN_TOKEN is new line token; EOS_TOKEN is end of stream token ###"))
    println(io, @sprintf("%s\r\n%60s\r\n%s", '-'^100, "Result of file \"" * filename * "\"'s tokens", '-'^100))
    list_tokens(io, tokens)
    println(@sprintf("Finish file %s scan.", filename))
end
