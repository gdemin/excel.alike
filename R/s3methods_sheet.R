VALUE = 1
EXPRESSION = 2
ONLY_EXPRESSION = 3

expression_type = function(expr){
    num_of_var = length(all.vars(expr))
    num_of_all = length(all.names(expr, unique = TRUE))
    if(num_of_var == num_of_all) {
        return(VALUE)
    } else {
        if(num_of_var == 0){
            return(ONLY_EXPRESSION)
        } else {
            return(EXPRESSION)
        }
    }

}


check_existense = function(x, value){
    stopif(!(value %in% colnames(x)), "column '", value, "' not found.")
}

#' @export
"$<-.sheet" = function(x, name, value){
    if(is.expression(value)){
        if(name %in% colnames(x)){
            x[[name]] = add_formula(x[[name]], value)
        } else {
            x[[name]] = NA
            x[[name]] = add_formula(x[[name]], value)
        }
    } else {
        x[[name]] = value
    }
    calculate(x)
}

#' @export
add_formulas = function(x, ...){
    exprs = substitute(list(...))
    all_names = names(exprs)
    for(each in seq_along(exprs)[-1]){
        x = add_single_formula(x, all_names[each], exprs[[each]])
    }
    calculate(x)
}






add_single_formula = function(x, name, expr){

    expr_type = expression_type(expr)
    if(expr_type<EXPRESSION){
        x[[name]] = expr
    } else {
        if(name %in% colnames(x)){
            x[[name]] = add_formula(x[[name]], expr)
        } else {
            x[[name]] = NA
            x[[name]] = add_formula(x[[name]], expr)
        }
    }
    x

}
