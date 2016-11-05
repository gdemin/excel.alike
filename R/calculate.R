#' Calculate/recalculate sheet (data.frame)
#'
#' @param sheet object of class 'sheet'
#' @param ... additional parameters
#'
#' @return original object of class 'sheet' with recalculated formulas
#' @export
#'
#' @examples
#' 1
calculate = function(sheet, ...){
    UseMethod("calculate")

}

#' @export
calculate.sheet = function(sheet, ...){
    for(each_column in seq_along(sheet)){
        sheet[[each_column]] = calculate_column(sheet, each_column)
    }
    sheet
}


calculate_column = function(sheet, column_number){
    curr_formula = get_formula(sheet[[column_number]])
    if(!is.null(curr_formula)){
        add_formula(eval(expr = curr_formula, envir = sheet), curr_formula)
    }  else {
        sheet[[column_number]]
    }
}


