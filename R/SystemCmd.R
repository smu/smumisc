#' Wrapper function to system()
#'
#' @param ...  one or more arguments of type string, which can be concatenated
#'             using the paste function (e.g. 'ls','-lrt','/bin').
#' @param sep character used to seperate the terms. Used in the paste function.
#' @param debug print command before execution.
#' @param intern a logical (not 'NA') which indicates whether to capture the
#'        output of the command as an R character vector (parameter to system 
#'        function).
#' @param ignore.stdout,ignore.stderr  a logical (not 'NA') indicating whether
#'        messages written to 'stdout' or 'stderr' should be ignored (parameter 
#'        to system function).
#' @param wait  a logical (not 'NA') indicating whether the R interpreter
#'        should wait for the command to finish, or run it
#'        asynchronously.  This will be ignored (and the interpreter
#'        will always wait) if 'intern = TRUE' (parameter to system function).
#' @param input if a character vector is supplied, this is copied one string
#'        per line to a temporary file, and the standard input of
#'        'command' is redirected to the file (parameter to system function).

#' @return Return value of the executed command.
#' @examples
#'     SystemCmd('ls -lrt /') 
#'     SystemCmd('ls -lrt /', debug = TRUE) 
#'     SystemCmd('ls', '-lrt', '/') 
#' 
#' 

SystemCmd <- function(..., sep = '', debug = FALSE, intern = FALSE,
                     ignore.stdout = FALSE, ignore.stderr = FALSE,
                     wait = TRUE, input = NULL){
    cmd <- paste(..., sep = sep)
    if (debug) print(cmd)
    return(system(cmd, intern, ignore.stdout, ignore.stderr, wait, input))
}
