#' Options for \code{pbapply} functions
#' 
#' @param ... Pass named arguments to set \code{pbapply} options.  Run with no
#' arguments to list currently set options.
#' 
#' @examples
#' pboptions() # lists current arguments
#' pboptions(char = "=", txt.width = 80)
#' pboptions(style = 1) # No percent
#' ## Reload package to reset defaults
#' 
#' @seealso \code{\link[utils]{txtProgressBar}}
#' @export
pboptions <-
function(...)
{
    opar <- getOption("pboptions")
    args <- list(...)
    if (length(args)) {
        if (length(args)==1 && is.list(args[[1]])) {
            npar <- args[[1]]
        } else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options("pboptions"=npar)
        invisible(opar)
    } else {
      opar
    }
    
}
