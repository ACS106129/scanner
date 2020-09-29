module Scan
include("token_type.jl")

using Printf
using .TokenType: Token, toPreversedType, toPunctuationType


@enum ScannerState begin
    START
    # PREPROCESS # deprecated
    IDENTIFIER
    CONSTANT
    SLASH_MODE
end

mutable struct Scanner
    lc::Int                             # line count
    state::ScannerState                 # state of scanner
    sign::Union{Char,Nothing}           # sign record token
    specifier::Union{Char,Nothing}      # specifier record token
    __stream::String                    # stream from file
    __index::Int                        # index in stream
    Scanner(stream::String) = new(1, START, nothing, nothing, stream, 0)
end

function next(sc::Scanner, ignoreSpace::Bool = false)::Union{Char,Nothing}
    sc.__index += 1
    result = sc.__index > lastindex(sc.__stream) ? nothing : sc.__stream[sc.__index]
    return (result in [' ', '\t'] && ignoreSpace) ? next(sc, true) : result
end

function retract(sc::Scanner, n::Int = 1)::Bool
    isSuccess = ((sc.__index - n) > 0)
    sc.__index = isSuccess ? sc.__index - n : 1
    return isSuccess
end 
end

using .Scan: Scanner

function start!(sc::Scanner, c::Char, tokens::Array)
    buf = string(c)
    # new line
    if c == '\r'
        if Scan.next(sc) != '\n'
            Scan.retract(sc)
        end
        sc.lc += 1
        push!(tokens, (TokenType.LN_TOKEN, nothing))
    elseif c == '\n'
        sc.lc += 1
        push!(tokens, (TokenType.LN_TOKEN, nothing))
    elseif c == '#'
        push!(tokens, (TokenType.SHARP, nothing))
        # sc.state = Scan.PREPROCESS # deprecated
    elseif c == ','
        push!(tokens, (TokenType.COMMA, nothing))
    elseif c == ':'
        push!(tokens, (TokenType.COLON, nothing))
    elseif c == ';'
        push!(tokens, (TokenType.SEMICOLON, nothing))
    elseif c == '('
        push!(tokens, (TokenType.LEFT_PARENTHESE, nothing))
    elseif c == ')'
        push!(tokens, (TokenType.RIGHT_PARENTHESE, nothing))
    elseif c == '['
        push!(tokens, (TokenType.LEFT_BRACKET, nothing))
    elseif c == ']'
        push!(tokens, (TokenType.RIGHT_BRACKET, nothing))
    elseif c == '{'
        push!(tokens, (TokenType.LEFT_BRACE, nothing))
    elseif c == '}'
        push!(tokens, (TokenType.RIGHT_BRACE, nothing))
    elseif c == '"'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        if lastTokenType == TokenType.INCLUDE
            c = Scan.next(sc)
            while c != '\r' && c != '\n' && c !== nothing
                buf *= c
                if c == '>'
                    push!(tokens, (TokenType.LIBRARY_NAME, buf))
                    buf = ""
                    break
                end
                c = Scan.next(sc)
            end
            if !isempty(buf)
                throw(UnIdentifierError(buf))  
            end
        else
            push!(tokens, __string_seq_stream!(sc, c, '"', true))
        end
    elseif c == '\''
        push!(tokens, __string_seq_stream!(sc, c, '"', true))
    elseif c == '+'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++|--) +(=|+)
        if lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # +=
            if c == '='
                push!(tokens, (TokenType.ACC_PLUS, nothing))
            # ++
            elseif c == '+'
                push!(tokens, (TokenType.DPLUS, nothing))
            # only + need retract
            else
                push!(tokens, (TokenType.PLUS, nothing))
                Scan.retract(sc)
            end
        # operator constant +
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.PLUS, nothing))
        # positive-sign (+123), += +123, -= +123, + +2, - +2, = +2
        # elseif lastTokenType in [TokenType.LEFT_PARENTHESE, TokenType.ACC_PLUS, TokenType.ACC_MINUS, TokenType.PLUS, TokenType.MINUS, TokenType.ASSIGN]
        #     sc.sign = c
        elseif lastTokenType !== nothing
            sc.sign = c
        else
            throw(UnIdentifierError(buf))
        end
    elseif c == '-'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++|--) -(=|-)
        if lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # -=
            if c == '='
                push!(tokens, (TokenType.ACC_MINUS, nothing))
            # --
            elseif c == '-'
                push!(tokens, (TokenType.DMINUS, nothing))
            # only - need retract
            else
                push!(tokens, (TokenType.MINUS, nothing))
                Scan.retract(sc)
            end
        # operator constant -
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.MINUS, nothing))
        # negative-sign (-123), += -123, -= -123, + -2, - -2, = -2
        # elseif lastTokenType in [TokenType.LEFT_PARENTHESE, TokenType.ACC_PLUS, TokenType.ACC_MINUS, TokenType.PLUS, TokenType.MINUS, TokenType.ASSIGN, TokenType.]
        #     sc.sign = c
        elseif lastTokenType !== nothing
            sc.sign = c
        else
            throw(UnIdentifierError(buf))
        end
    elseif c == '*'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++)/(--) *(=)
        if lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # *=
            if c == '='
                push!(tokens, (TokenType.ACC_MULTIPLY, nothing))
            # only * need retract
            else
                push!(tokens, (TokenType.MULTIPLY, nothing))
                Scan.retract(sc)
            end
        # operator constant *
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.MULTIPLY, nothing))
        # if target is pointer (allow format *id with space between)
        else
            sc.specifier = c
            c = Scan.next(sc, true)
            if c == '\r' || c == '\n'
                c = nothing
                Scan.retract(sc)
            end
            # if pointer name is valid
            if c !== nothing && (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_')
                sc.state = Scan.IDENTIFIER
                Scan.retract(sc)
            else
                throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * buf * string(c === nothing ? "" : c)))
            end   
        end
    elseif c == '/'
        sc.state = Scan.SLASH_MODE
    elseif c == '%'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++)/(--) %(=)
        if lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # %=
            if c == '='
                push!(tokens, (TokenType.ACC_MODULO, nothing))
            # only % need retract
            else
                push!(tokens, (TokenType.MODULO, nothing))
                Scan.retract(sc)
            end
        # operator constant %
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.MODULO, nothing))
        else
            throw(UnIdentifierError(buf))
        end
    elseif c == '='
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        c = Scan.next(sc)
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # compare
        if c == '=' && lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # === identifier equal
            if c == '='
                push!(tokens, (TokenType.IDENTIFIER_EQUAL, nothing))
            # == equal
            else
                push!(tokens, (TokenType.EQUAL, nothing))
                Scan.retract(sc)
            end
        # = assign
        elseif lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            push!(tokens, (TokenType.ASSIGN, nothing))
            Scan.retract(sc)
        else
            throw(UnIdentifierError(buf * c))
        end
    elseif c == '!'
        c = Scan.next(sc)
        # != compare
        if c == '=' && !isempty(tokens)
            if last(tokens)[1] in [TokenType.IDENTIFIER, TokenType.POINTER_IDENTIFIER, TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON, TokenType.DPLUS, TokenType.DMINUS]
                push!(tokens, (TokenType.NOT_EQUAL, nothing))
            else
                throw(UnIdentifierError(buf * c))
            end
        # ! not
        else
            push!(tokens, (TokenType.NOT, nothing))
            Scan.retract(sc)
        end
    elseif c == '&'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++)/(--) &(=)
        if lastTokenType in [TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # &=
            if c == '='
                push!(tokens, (TokenType.ACC_BIT_AND, nothing))
            # &&
            elseif c == '&'
                push!(tokens, (TokenType.AND, nothing))
            # only & need retract
            else
                push!(tokens, (TokenType.BIT_AND, nothing))
                Scan.retract(sc)
            end
        # operator constant &
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.BIT_AND, nothing))
        # if target is address (allow format &id with space between)
        else
            sc.specifier = c
            c = Scan.next(sc, true)
            if c == '\r' || c == '\n'
                c = nothing
                Scan.retract(sc)
            end
            # if address name is valid
            if c !== nothing && (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_')
                sc.state = Scan.IDENTIFIER
                Scan.retract(sc)
            else
                throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * buf * (c === nothing ? "" : c)))
            end
        end
    elseif c == '|'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++)/(--) |(=)
        if lastTokenType in [TokenType.IDENTIFIER, TokenType.POINTER_IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # |=
            if c == '='
                push!(tokens, (TokenType.ACC_BIT_OR, nothing))
            # ||
            elseif c == '|'
                push!(tokens, (TokenType.OR, nothing))
            # only | need retract
            else
                push!(tokens, (TokenType.BIT_OR, nothing))
                Scan.retract(sc)
            end
        # operator constant |
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.BIT_OR, nothing))
        else
            throw(UnIdentifierError(buf))
        end
    elseif c == '^'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        # operator id(++)/(--) ^(=)
        if lastTokenType in [TokenType.IDENTIFIER, TokenType.POINTER_IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            c = Scan.next(sc)
            # ^=
            if c == '='
                push!(tokens, (TokenType.ACC_BIT_XOR, nothing))
            # only ^ need retract
            else
                push!(tokens, (TokenType.BIT_XOR, nothing))
                Scan.retract(sc)
            end
        # operator constant ^
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.BIT_XOR, nothing))
        else
            throw(UnIdentifierError(buf))
        end
    elseif c == '~'
        push!(tokens, (TokenType.BIT_NOT, nothing))
    elseif c == '<'
        if isempty(tokens)
            c = Scan.next(sc)
            while c != '\r' && c != '\n' && c !== nothing
                buf *= c
                if c == '>'
                    push!(tokens, (TokenType.LIBRARY_NAME, buf))
                    buf = ""
                    break
                end
                c = Scan.next(sc)
            end
            if !isempty(buf)
                throw(UnIdentifierError(buf))  
            end
        else
            lastTokenType = nothing
            for type in reverse(tokens)
                if type[1] != TokenType.RIGHT_PARENTHESE
                    lastTokenType = type[1]
                    break
                end
            end
            if lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON, TokenType.IDENTIFIER, TokenType.POINTER_IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]   
                c = Scan.next(sc)
                buf *= c
                if c == '<'
                    c = Scan.next(sc)
                    # <<=
                    if c == '=' 
                        if !(lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON])
                            push!(tokens, (TokenType.ACC_LEFT_SHIFT, nothing))
                        else
                            throw(UnIdentifierError(buf * c))  
                        end
                    # << without = need retract
                    else
                        push!(tokens, (TokenType.LEFT_SHIFT, nothing))
                        Scan.retract(sc)
                    end
                # <=
                elseif c == '='
                    push!(tokens, (TokenType.LESS_EQUAL, nothing))
                # only < need retract
                else
                    push!(tokens, (TokenType.LESS, nothing))
                    Scan.retract(sc)
                end
            elseif lastTokenType == TokenType.INCLUDE
                c = Scan.next(sc)
                while c != '\r' && c != '\n' && c !== nothing
                    buf *= c
                    if c == '>'
                        push!(tokens, (TokenType.LIBRARY_NAME, buf))
                        buf = ""
                        break
                    end
                    c = Scan.next(sc)
                end
                if !isempty(buf)
                    throw(UnIdentifierError(buf))  
                end
            else
                throw(UnIdentifierError(buf))  
            end
        end
    elseif c == '>'
        if isempty(tokens)
            throw(UnIdentifierError(buf))
        end
        lastTokenType = nothing
        for type in reverse(tokens)
            if type[1] != TokenType.RIGHT_PARENTHESE
                lastTokenType = type[1]
                break
            end
        end
        if lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON, TokenType.POINTER_IDENTIFIER, TokenType.IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]   
            c = Scan.next(sc)
            buf *= c
            if c == '>'
                c = Scan.next(sc)
                # >>=
                if c == '=' 
                    if !(lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON])
                        push!(tokens, (TokenType.ACC_RIGHT_SHIFT, nothing))
                    else
                        throw(UnIdentifierError(buf * c))  
                    end
                # >> without = need retract
                else
                    push!(tokens, (TokenType.RIGHT_SHIFT, nothing))
                    Scan.retract(sc)
                end
            # >=
            elseif c == '='
                push!(tokens, (TokenType.GREATER_EQUAL, nothing))
            # only > need retract
            else
                push!(tokens, (TokenType.GREATER, nothing))
                Scan.retract(sc)
            end
        else
            throw(UnIdentifierError(buf))  
        end
    elseif c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'
        sc.state = Scan.IDENTIFIER
        Scan.retract(sc)
    elseif c >= '0' && c <= '9'
        sc.state = Scan.CONSTANT
        Scan.retract(sc)
    elseif c == '.'
        c = Scan.next(sc)
        Scan.retract(sc)
        if c >= '0' && c <= '9'
            sc.state = Scan.CONSTANT
            Scan.retract(sc)
        else
            throw(UnIdentifierError(buf))  
        end
    else
        throw(UnIdentifierError(buf))  
    end
end

# deprecated
# function preprocess!(sc::Scanner, c::Char, tokens::Array)
#     buffer, keyword, isValid = "", "", true
#     isUserLib = nothing
#     while c !== nothing && c != '\r' && c != '\n'
#         if isempty(keyword)
#             if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
#                 if !isValid
#                     push!(tokens, (TokenType.UNIDENTIFIER, rstrip(buffer)))
#                     buffer, isValid = "", true
#                 end
#                 buffer *= c
#                 if lowercase(buffer) in ["include", "define"]
#                     keyword, buffer = buffer, ""
#                 end
#             elseif c != ' ' && c != '\t' || !isempty(buffer)
#                 buffer *= c
#                 isValid = false
#             end
#         elseif lowercase(keyword) == "include"
#             if isUserLib === nothing
#                 if c == '"'
#                     isUserLib = true
#                     Scan.retract(sc)
#                 elseif c == '<'
#                     isUserLib = false
#                     Scan.retract(sc)
#                 elseif c != ' ' && c != '\t' || !isempty(buffer)
#                     buffer *= c
#                 end
#             elseif isUserLib
#                 buffer, isValid = __preinclude(buffer, c, tokens, isValid, '"', '"')
#             else
#                 buffer, isValid = __preinclude(buffer, c, tokens, isValid, '<', '>')
#             end
#         elseif lowercase(keyword) == "define"
#         elseif keyword == "finish"
#             if c != ' ' && c != '\t' || !isempty(buffer)
#                 buffer *= c
#             end
#         end
#         c = Scan.next(sc)
#     end
#     buffer = strip(buffer)
#     Scan.retract(sc)
#     if !isempty(buffer)
#         single = findfirst("//", buffer)
#         multiple = findfirst("/*", buffer)
#         if single !== nothing || multiple !== nothing
#             single = (single === nothing ? typemax(Int) : single[1])
#             multiple = (multiple === nothing ? typemax(Int) : multiple[1])
#             minVal = min(single, multiple) - 1
#             push!(tokens, (TokenType.UNIDENTIFIER, rstrip(buffer[1:minVal])))
#             Scan.retract(sc, lastindex(buffer) - minVal)
#         else
#             push!(tokens, (TokenType.UNIDENTIFIER, buffer))
#         end
#     end
#     sc.state = Scan.START
# end

function identifier!(sc::Scanner, c::Char, tokens::Array)
    id, c = string(c), Scan.next(sc)
    while c !== nothing && c != '\r' && c != '\n'
        if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9'
            id *= c
        # allow multiple pointer
        elseif sc.specifier == '*' && c == '*' && last(id) == '*'
            while c == '*'
                id *= c
                c = Scan.next(sc, true)
            end
            Scan.retract(sc)
        else
            Scan.retract(sc)
            break
        end
        c = Scan.next(sc)
    end
    if c === nothing || c == '\r' || c == '\n'
        Scan.retract(sc)
    end
    pType = TokenType.toPreversedType(id)
    if pType !== nothing
        if sc.specifier !== nothing || sc.sign !== nothing
            throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * (sc.specifier === nothing ? "" : sc.specifier) * id))
        end
        push!(tokens, (pType, nothing))
    else
        tempArr = []
        if sc.specifier == '*'
            if isempty(tokens)
                throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * sc.specifier * id))
            end
            if isempty(findall(x -> x[1] in [TokenType.POINTER_IDENTIFIER] && x[2] == id, tokens))
                for type in reverse(tokens)
                    if type[1] == TokenType.LN_TOKEN
                        throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * sc.specifier * id))
                    elseif type[1] in [TokenType.INT, TokenType.CHAR, TokenType.DOUBLE, TokenType.FLOAT]
                        break
                    end
                end
            end
            push!(tempArr, (TokenType.POINTER_IDENTIFIER, id))
        elseif sc.specifier == '&'
            # if variable exists
            if !isempty(findall(x -> x[1] == TokenType.IDENTIFIER && x[2] == id, tokens))
                push!(tempArr, (TokenType.ADDRESS_IDENTIFIER, id))
            else
                throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * sc.specifier * id))
            end
        # if normal variable
        else
            if isempty(tokens)
                throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * id))
            end
            if isempty(findall(x -> x[1] == TokenType.IDENTIFIER && x[2] == id, tokens))
                for type in reverse(tokens)
                    if type[1] == TokenType.LN_TOKEN
                        throw(UnIdentifierError((sc.sign === nothing ? "" : sc.sign) * id))
                    elseif type[1] in [TokenType.INT, TokenType.CHAR, TokenType.DOUBLE, TokenType.FLOAT]
                        break
                    end
                end
            end
            push!(tempArr, (TokenType.IDENTIFIER, id))
        end
        push!(tokens, tempArr...)
    end
    sc.sign = nothing
    sc.specifier = nothing
    sc.state = Scan.START
end

function constant!(sc::Scanner, c::Char, tokens::Array)
    con, c, conType = string(c), Scan.next(sc), nothing
    # determine whether is decimal or not
    if con == "0"
        if c == 'x' || c == 'X'
            conType = TokenType.HEX_CON
        elseif c >= '0' && c <= '7'
            conType = TokenType.OCT_CON
        elseif c == '.'
            conType = TokenType.FLOAT_CON
        end
        if conType === nothing
            # if constant is zero
            conType = TokenType.DEC_CON
        else
            con *= c
            c = Scan.next(sc)
        end
    # float number
    elseif con == "."
        con *= c
        if c >= '0' && c <= '9'
            conType = TokenType.FLOAT_CON
            c = Scan.next(sc)
        else
            throw(UnIdentifierError(con))
        end
    else
        conType = TokenType.DEC_CON
    end
    while c !== nothing && c != '\r' && c != '\n'
        if conType == TokenType.HEX_CON
            if !occursin(r"^[0-9A-Fa-f]$", string(c))
                break
            end
        elseif conType == TokenType.OCT_CON
            if c < '0' || c > '7'
                break
            end
        elseif conType == TokenType.DEC_CON
            if c == '.'
                conType = TokenType.FLOAT_CON
            elseif c < '0' || c > '9'
                break
            end
        elseif conType == TokenType.FLOAT_CON
            if c < '0' || c > '9'
                break
            end
        end
        con *= c
        c = Scan.next(sc)
    end
    Scan.retract(sc)
    if sc.sign == '+' || sc.sign == '-'
        con = sc.sign * con
    end
    push!(tokens, (conType, con))
    sc.sign = nothing
    sc.specifier = nothing
    sc.state = Scan.START
end

function slash!(sc::Scanner, c::Char, tokens::Array)
    buffer = ""
    # // comment
    if c == '/'
        c = Scan.next(sc)
        while c !== nothing && c != '\r' && c != '\n'
            buffer *= c
            c = Scan.next(sc)
        end
        push!(tokens, (TokenType.SINGLE_COMMENT, buffer))
        Scan.retract(sc)
        buffer = ""
    # /* multiple comment
    elseif c == '*'
        c = Scan.next(sc)
        while c !== nothing && !occursin("*/", buffer)
            buffer *= c
            c = Scan.next(sc)
        end
        push!(tokens, (TokenType.MULTIPLE_COMMENT, buffer[1:(end - 2)]))
        Scan.retract(sc)
        buffer = ""
    elseif !isempty(tokens) 
        lastTokenType = last(tokens)[1]
        # operator id(++)/(--) /(=)
        if lastTokenType in [TokenType.IDENTIFIER, TokenType.POINTER_IDENTIFIER, TokenType.DPLUS, TokenType.DMINUS]
            # /=
            if c == '='
                push!(tokens, (TokenType.ACC_DIVIDE, nothing))
            # only / need retract
            else
                push!(tokens, (TokenType.DIVIDE, nothing))
                Scan.retract(sc)
            end
        # operator constant /
        elseif lastTokenType in [TokenType.DEC_CON, TokenType.HEX_CON, TokenType.OCT_CON, TokenType.FLOAT_CON]
            push!(tokens, (TokenType.DIVIDE, nothing))
        else
            throw(UnIdentifierError(string(c)))
        end
    else
        throw(UnIdentifierError(string(c)))
    end
    if !isempty(buffer)
        push!(tokens, (TokenType.UNIDENTIFIER, buffer))
    end
    sc.state = Scan.START
end

# deprecated
# function __preinclude(buffer::String, c::Char, tokens::Array, isValid::Bool, prefix::Char, postfix::Char)::Tuple{String, Bool}
#     if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '.' || c == '_') && startswith(buffer, string(prefix))
#         # < (invalid)
#         if !isValid
#             push!(tokens, (TokenType.UNIDENTIFIER, rstrip(buffer[2:end])))
#             buffer, isValid = string(prefix), true
#         end
#         buffer *= c
#     elseif c == postfix && startswith(buffer, string(prefix))
#         # <xx(invalid) >
#         matchString = match(Regex("^" * prefix * "[a-zA-Z._]*"), buffer)
#         matchString = matchString === nothing ? "" : matchString.match
#         remain = strip(buffer[(lastindex(matchString) + 1):end])
#         if !isValid && !isempty(remain)
#             push!(tokens, (TokenType.UNIDENTIFIER, remain))
#             isValid = true
#         end
#         push!(tokens, (TokenType.INCLUDE, nothing))
#         push!(tokens, (TokenType.LIBRARY_NAME, matchString * c))
#         buffer, keyword = "", "finish"
#     elseif c == prefix && !(c in buffer)
#         # (invalid) <
#         if !isValid
#             push!(tokens, (TokenType.UNIDENTIFIER, rstrip(buffer)))
#             buffer, isValid = "", true
#         end
#         buffer *= c
#     elseif c != ' ' && c != '\t' || !isempty(buffer[2:end])
#         buffer *= c
#         isValid = false
#     end
#     return buffer, isValid
# end

function __string_seq_stream!(sc::Scanner, startSymbol::Char, endSymbol::Char, isLimitOneLine::Bool)::Tuple{TokenType.Token, Union{Any,Nothing}}
    startType, endType = TokenType.toPunctuationType(startSymbol), TokenType.toPunctuationType(endSymbol)
    strSeq, charSeq, buf, c = [], [], "", Scan.next(sc)
    while c !== nothing && (c != '\r' && c != '\n' || !isLimitOneLine)
        if c == '\\' || c == '%'
            if !isempty(buf)
                push!(charSeq, split(buf, ' ')...)
                push!(strSeq, (TokenType.CHAR_SEQ, tuple(charSeq...)))
                charSeq = []
                buf = ""
            end
            # \ escape char
            if c == '\\'
                c = Scan.next(sc)
                if c == 'a'
                    push!(strSeq, TokenType.ALERT_ESC)
                elseif c == 'b'
                    push!(strSeq, TokenType.BACKSPACE_ESC)
                elseif c == 'e'
                    push!(strSeq, TokenType.ESCAPE_ESC)
                elseif c == 'f'
                    push!(strSeq, TokenType.FORMFEED_ESC)
                elseif c == 'n'
                    push!(strSeq, TokenType.NEWLINE_ESC)
                elseif c == 'r'
                    push!(strSeq, TokenType.RETURN_ESC)
                elseif c == 't'
                    push!(strSeq, TokenType.HTAB_ESC)
                elseif c == 'v'
                    push!(strSeq, TokenType.VTAB_ESC)
                elseif c == '\\'
                    push!(strSeq, TokenType.BACKSLASH_ESC)
                elseif c == '\''
                    push!(strSeq, TokenType.QUOTE_ESC)
                elseif c == '"'
                    push!(strSeq, TokenType.DQUOTE_ESC)
                elseif c == '?'
                    push!(strSeq, TokenType.QUESTION_ESC)
                elseif c == 'u'
                    for i in 1:4
                        c = Scan.next(sc)
                        if c === nothing || !occursin(r"^[0-9A-Fa-f]$", string(c))
                            Scan.retract(sc)
                            break
                        end
                        buf *= c
                    end
                    if occursin(r"^[0-9A-Fa-f]{4}$", buf)
                        push!(strSeq, (TokenType.HEX_ESC, buf))
                    # hexadecimal format error
                    else
                        push!(strSeq, (TokenType.UNIDENTIFIER, "\\u" * buf))
                    end
                elseif c >= '0' && c <= '7'
                    buf *= c
                    times = (c >= '0' && c <= '3' ? 2 : 1)
                    for i in 1:times
                        c = Scan.next(sc)
                        if c === nothing || c < '0' || c > '9'
                            Scan.retract(sc)
                            break
                        end
                        buf *= c
                    end
                    if occursin(r"^[0-3]?[0-9]{1,2}$", buf)
                        push!(strSeq, (TokenType.OCT_ESC, buf))
                    # octal format error
                    else
                        push!(strSeq, (TokenType.UNIDENTIFIER, "\\" * buf))
                    end
                # unknown escape symbol
                else
                    push!(strSeq, (TokenType.UNIDENTIFIER, "\\" * c))
                end
            # % format char
            elseif c == '%'
                c = Scan.next(sc)
                # fetch the format size and align
                while c !== nothing && c != '\r' && c != '\n'
                    buf *= c
                    if c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
                        if !occursin(r"^[dDfFcCsS]$", string(c))
                            buf *= Scan.next(sc)
                        end
                        break
                    end
                    c = Scan.next(sc)
                end
                if c === nothing || c == '\r' || c == '\n'
                    Scan.retract(sc)
                end
                # %x.x
                format = match(r"^[+-]?(([0-9]+\.?[0-9]*)|([0-9]*\.?[0-9]+))?", buf)
                if format !== nothing
                    format = format.match
                    type = lowercase(buf[(lastindex(format) + 1):end])
                    # format type (ignore case)
                    if type == "d"
                        push!(strSeq, (TokenType.DECIMAL_FORMAT, format))
                    elseif type == "l"
                        push!(strSeq, (TokenType.LONG_FORMAT, format))
                    elseif type == "f"
                        push!(strSeq, (TokenType.FLOAT_FORMAT, format))
                    elseif type == "lf"
                        push!(strSeq, (TokenType.DOUBLE_FORMAT, format))
                    elseif type == "c"
                        push!(strSeq, (TokenType.CHAR_FORMAT, format))
                    elseif type == "s"
                        push!(strSeq, (TokenType.STRING_FORMAT, format))
                    else
                        push!(strSeq, (TokenType.UNIDENTIFIER, '%' * buf))
                    end
                else
                    push!(strSeq, (TokenType.UNIDENTIFIER, '%' * buf))
                end
            end
            buf = ""
        elseif c == endSymbol
            if !isempty(buf)
                push!(charSeq, split(rstrip(buf))...)
                push!(strSeq, (TokenType.CHAR_SEQ, tuple(charSeq...)))
            end
            return TokenType.STRING_SEQ, tuple((startType === nothing ? startSymbol : startType), strSeq..., (endType === nothing ? endSymbol : endType))
        else
            buf *= c
        end
        c = Scan.next(sc)
    end
    Scan.retract(sc)
    if !isempty(buf)
        push!(strSeq, buf)
    end
    return TokenType.UNIDENTIFIER, tuple(startSymbol, strSeq..., endSymbol)
end