module IDError
struct UnIdentifierError <: Exception
    msg::AbstractString
end
end

using .IDError: UnIdentifierError

function Base.showerror(io::IO, err::UnIdentifierError)
    print(io, err.msg)
end
