## stop if condition with message
stopif = function(cond,...){
    if (cond) {
        stop(do.call(paste0,c(list(...))),call. = FALSE)
    }
    invisible()
}


##################
### TRUE if argument is list, not data.frame
is_list=function(x)

{
	is.list(x) && (!is.data.frame(x))
}



### Flatten list
### list(a,list(b,c))->list(a,b,c)
flist=function(x)
{
    need_unlist=vapply(x,is_list, FUN.VALUE = logical(1))
    if (any(need_unlist)) {
        res=lapply(x,function(elem){
            if (is_list(elem)){
                flist(elem)
            } else list(elem)

        })
        do.call(c,res)
    } else x
}


