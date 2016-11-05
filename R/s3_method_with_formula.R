add_formula = function(x, expr){
    attr(x, "formula") = expr
    class(x) = union("with_formula", class(x))
    x
}

get_formula = function(x){
    attr(x, "formula")
}
