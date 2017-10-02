makeTfuns <- function(node,qnt=c('cost','qol')){
    ss <- list()
    for(i in seq_along(qnt)) ss[[qnt[i]]] <- getAQ(node,qnt[i])
    for(i in seq_along(qnt)) ss[[i]] <- parse(text=ss[[i]])
    ans <- list()
    for(i in seq_along(qnt)) ans[[paste0(qnt[i],'fun')]] <- function(data) eval(ss[[i]],envir=data)
    ans
}
