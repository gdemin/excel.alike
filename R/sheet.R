#' Convert data.frame to object of class 'sheet'
#'
#' @param data data.frame/vector etc.
#' @param ... additionla parameters for future versions
#'
#' @return object of class 'sheet' (inherited from data.frame)
#' @export
#'
#' @examples
#' 1
as.sheet = function(data, ...){
    UseMethod("as.sheet")

}

#' @export
as.sheet.default = function(data, ...){
    res = as.dtfrm(data)
    colnames(res) = correct_names(colnames(res))
    class(res) = union("sheet", class(res))
    res
}



#' @export
as.sheet.data.frame = function(data, ...){
    colnames(data) = correct_names(colnames(data))
    class(data) = union("sheet", class(data))
    data
}


#' @export
as.sheet.sheet = function(data, ...){
    data
}

correct_names = function(names){
    gsub("(\\d+)$", "\\1_", names, perl = TRUE)
}

