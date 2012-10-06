## R code used in the project

### DEBUG/UNDEBUG AT POINT

## length(tf <- apropos(".", mode = "function"))
## length(tg <- getGenerics())
## sum(tg %in% tf) ## all generics are present as function in the vector returnde by apropos
## findMethods("+")
## setClass("FOO", contains="character")
## setMethod("+", c("FOO", "character"), function(e1,e2) paste(e1,e2,collapse=""))
## str(findMethods("+"))
## showMethods("+")
## str(findMethods("+"))
## isGeneric("sin")
## findMethodSignatures("sin")
## getGeneric("sin")
## te <- getMethodsForDispatch(getGeneric("lu"))

local({
    .ess_dbg_getTracedAndDebugged <- function(){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        generics <- methods::getGenerics()
        out <- c()
        for(i in seq_along(generics)){
            menv <- methods::getMethodsForDispatch(methods::getGeneric(generics[[i]], package=generics@package[[i]]))
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                out <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), out)
            if(is(getFunction(generics[[i]], where = .GlobalEnv),  'traceable')) # if the default is traced,  it does not appear in the menv
                out <- c(generics[[i]], out)
        }
        debugged <- apropos('.', mode = 'function')
        ## traced function don't appear here. Not realy needed and would affect performance.
        debugged <- debugged[which(unlist(lapply(debugged, isdebugged) , recursive=FALSE, use.names=FALSE))]
        c(debugged, out)
    }
    .ess_dbg_UntraceOrUndebug <- function(name){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        ## name is a name of a function to be undebugged or has a form name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }
    }
    .ess_dbg_UndebugALL <- function(funcs){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
    }
    environment(.ess_dbg_UndebugALL) <-
        environment(.ess_dbg_UntraceOrUndebug) <-
            environment(.ess_dbg_getTracedAndDebugged) <- .GlobalEnv  ## to see all the funcs
    assign('.ess_dbg_getTracedAndDebugged', .ess_dbg_getTracedAndDebugged, envir= .BaseNamespaceEnv)
    assign('.ess_dbg_UntraceOrUndebug', .ess_dbg_UntraceOrUndebug, envir= .BaseNamespaceEnv)
    assign('.ess_dbg_UndebugALL', .ess_dbg_UndebugALL, envir= .BaseNamespaceEnv)
})

setGeneric("blabla", function(a=34,b=343) standardGeneric("blabla"))

methods:::.showMethodsTable()
sapply(tf,)

isGeneric("plot")
is(m, "MethodDefinition")

###_ WATCHES:
.ess_watch_expressions <- list()
.ess_watch_expressions <- list(a = parse(text = "343 + 4"),
    parse(text = "sdfsf + 33"),
    parse(text = "str(iris)"))

assign('.ess_watch_eval', function(){
    if(!exists('.ess_watch_expressions')){
        assign('.ess_watch_expressions', list(), envir = .GlobalEnv)
    }
    if(length(.ess_watch_expressions) == 0L){
        cat('\n# Watch list is empty!\n
# a/i     append/insert new expression
# k       kill
# e       edit the expression
# r       rename
# n/p     navigate
# u/U     move the expression up/down
# q       kill the buffer')
    }else{
        .parent_frame <- parent.frame()
        .essWEnames <- allNames(.ess_watch_expressions)
        len0p <- !nzchar(.essWEnames)
        .essWEnames[len0p] <- seq_along(len0p)[len0p]
        for(i in seq_along(.ess_watch_expressions)){
            cat('\n@---- ', .essWEnames[[i]], ' ', rep.int('-', max(0, 35 - nchar(.essWEnames[[i]]))), '->\n', sep = '')
            cat( paste('@--->', deparse(.ess_watch_expressions[[i]][[1L]])), ' \n', sep = '')
            tryCatch(print(eval(.ess_watch_expressions[[i]], envir = .parent_frame)),
                     error = function(e) cat('Error:', e$message, '\n' ),
                     warning = function(w) cat('warning: ', w$message, '\n' ))
        }}
}, envir = .BaseNamespaceEnv); environment(.ess_watch_eval)<-.GlobalEnv

assign('.ess_log_eval',  function(log_name){
    if(!exists(log_name, envir = .GlobalEnv, inherits = FALSE))
        assign(log_name, list(), envir = .GlobalEnv)
    log <- get(log_name, envir = .GlobalEnv, inherits = FALSE)
    .essWEnames <- allNames(.ess_watch_expressions)
    cur_log <- list()
    .parent_frame <- parent.frame()
    for(i in seq_along(.ess_watch_expressions)){
        capture.output({
        cur_log[[i]] <-
            tryCatch(eval(.ess_watch_expressions[[i]]), envir = .parent_frame,
                     error = function(e) paste('Error:', e$message, '\n'),
                     warning = function(w) paste('warning: ', w$message, '\n'))
        if(is.null(cur_log[i][[1]]))
            cur_log[i] <- list(NULL)
                   })
    }
    names(cur_log) <- .essWEnames
    assign(log_name, c(log, list(cur_log)), envir = .GlobalEnv)
    invisible(NULL)
}, envir = .BaseNamespaceEnv); environment(.ess_log_eval) <- .GlobalEnv



