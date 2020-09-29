module TokenType
@enum Token begin
    # Reserved word
    INCLUDE = 0                         # include
    DEFINE = 1                          # define
    MAIN = 2                            # main
    CHAR = 3                            # char
    INT = 4                             # int
    FLOAT = 5                           # float
    DOUBLE = 6                          # double
    IF = 7                              # if
    ELSE = 8                            # else
    ELSEIF = 9                          # elseif
    FOR = 10                            # for
    WHILE = 11                          # while
    DO = 12                             # do
    RETURN = 13                         # return
    SWITCH = 14                         # switch
    CASE = 15                           # case
    PRINTF = 16                         # printf
    SCANF = 17                          # scanf
    # Library name
    LIBRARY_NAME = 30                   # <libname.h>, "libname.h"
    # Comment
    SINGLE_COMMENT = 50                 # //
    MULTIPLE_COMMENT = 51               # /**/
    # Identifier
    IDENTIFIER = 60                     # a b c
    # Pointer
    POINTER_IDENTIFIER = 61             # *a *b *c
    # Address
    ADDRESS_IDENTIFIER = 62             # declared &a &b &c
    # Unidentifier
    UNIDENTIFIER = 69                   # undefine
    # Sequence token
    CHAR_SEQ = 70                       # "a b c" -> "a", "b", "c"
    STRING_SEQ = 71                     # "abc efg \n" -> DQUOTE, CHAR_SEQ("abc", "efg "), NEWLINE_ESC, DQUOTE
                                        # 'b' -> QUOTE, CHAR_SEQ("b"), QUOTE
                                        # “123” -> LDQUOTE, CHAR_SEQ("123"), RDQUOTE
    # Constant
    DEC_CON = 80                        # 1 2 3 -1 (-2) -3
    OCT_CON = 81                        # 0101 0234
    HEX_CON = 82                        # 0x12
    FLOAT_CON = 83                      # 1.2f 1.f .5f 1.2
    # Operator
    PLUS = 100                          # +
    MINUS = 101                         # -
    MULTIPLY = 102                      # *
    DIVIDE = 103                        # /
    MODULO = 104                        # %
    ASSIGN = 105                        # =
    NOT = 106                           # !
    BIT_AND = 107                       # &
    BIT_OR = 108                        # |
    BIT_XOR = 109                       # ^
    BIT_NOT = 110                       # ~
    DPLUS = 111                         # ++
    DMINUS = 112                        # --
    ACC_PLUS = 113                      # +=
    ACC_MINUS = 114                     # -=
    ACC_MULTIPLY = 115                  # *=
    ACC_DIVIDE = 116                    # /=
    ACC_MODULO = 117                    # %=
    ACC_BIT_AND = 118                   # &=
    ACC_BIT_OR = 119                    # |=
    ACC_BIT_XOR = 120                   # ^=
    LEFT_SHIFT = 121                    # <<
    RIGHT_SHIFT = 122                   # >>
    ACC_LEFT_SHIFT = 123                # <<=
    ACC_RIGHT_SHIFT = 124               # >>=
    AND = 125                           # &&
    OR = 126                            # ||
    # Comparator
    EQUAL = 150                         # ==
    IDENTIFIER_EQUAL = 151              # ===
    GREATER = 152                       # >
    LESS = 153                          # <
    GREATER_EQUAL = 154                 # >=
    LESS_EQUAL = 155                    # <=
    NOT_EQUAL = 156                     # !=
    # Bracket
    LEFT_PARENTHESE = 170               # (
    RIGHT_PARENTHESE = 171              # )
    LEFT_BRACE = 172                    # {
    RIGHT_BRACE = 173                   # }
    LEFT_BRACKET = 174                  # [
    RIGHT_BRACKET = 175                 # ]
    # Format specifier
    DECIMAL_FORMAT = 180                # %x.xd
    LONG_FORMAT = 181                   # %x.xl
    FLOAT_FORMAT = 182                  # %x.xf
    DOUBLE_FORMAT = 183                 # %x.xlf
    CHAR_FORMAT = 184                   # %c
    STRING_FORMAT = 185                 # %x.xs
    ALERT_ESC = 200                     # \a
    BACKSPACE_ESC = 201                 # \b
    ESCAPE_ESC = 202                    # \e
    FORMFEED_ESC = 203                  # \f
    NEWLINE_ESC = 204                   # \n
    RETURN_ESC = 205                    # \r
    HTAB_ESC = 206                      # \t
    VTAB_ESC = 207                      # \v
    BACKSLASH_ESC = 208                 # \\
    QUOTE_ESC = 209                     # \'
    DQUOTE_ESC = 210                    # \"
    QUESTION_ESC = 211                  # \?
    OCT_ESC = 212                       # \123 \2 \32
    HEX_ESC = 213                       # \u0014
    # Punctuation
    COMMA = 250                         # ,
    SEMICOLON = 251                     # ;
    COLON = 252                         # :
    SHARP = 253                         # #
    QUOTE = 254                         # '
    DQUOTE = 255                        # "
    # New line of stream
    LN_TOKEN = -1
    # End of stream
    EOS_TOKEN = -2
end

function toPunctuationType(symbol::Char)::Union{Token,Nothing}
    if symbol == ','
        return COMMA
    elseif symbol == ';'
        return SEMICOLON
    elseif symbol == ':'
        return COLON
    elseif symbol == '#'
        return SHARP
    elseif symbol == '\''
        return QUOTE
    elseif symbol == '"'
        return DQUOTE
    end
    return nothing
end

function toPreversedType(str::String)::Union{Token,Nothing}
    str = lowercase(str)
    if str == "include"
        return INCLUDE
    elseif str == "define"
        return DEFINE
    elseif str == "main"
        return MAIN
    elseif str == "char"
        return CHAR
    elseif str == "int"
        return INT
    elseif str == "float"
        return FLOAT
    elseif str == "double"
        return DOUBLE
    elseif str == "if"
        return IF
    elseif str == "elseif"
        return ELSEIF
    elseif str == "else"
        return ELSE
    elseif str == "for"
        return FOR
    elseif str == "while"
        return WHILE
    elseif str == "do"
        return DO
    elseif str == "return"
        return RETURN
    elseif str == "switch"
        return SWITCH
    elseif str == "case"
        return CASE
    elseif str == "printf"
        return PRINTF
    elseif str == "scanf"
        return SCANF
    end
    return nothing
end
end