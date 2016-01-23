#' @name pbapply
#' @title \code{apply} family functions with progress bars
#' @aliases pbsapply
#' @aliases pblapply
#' @aliases pbreplicate
#'
#' @param X either an array for \code{pbapply} or a vector (or list) for
#' \code{pblapply}, \code{pbsapply}, and \code{pbreplicate}
#' @param MARGIN a vector giving the subscripts which the function will be 
#' applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, 
#' c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a 
#' character vector selecting dimension names.
#' @param FUN the function to be applied: see 'Details'. In the case of 
#' functions like +, \%*\%, etc., the function name must be backquoted or quoted.
#' @param ... optional arguments to \code{FUN}
#' @param n integer: the number of replications.
#' @param expr the expression to evaluate repeatedly.
#' @param simplify logical or character string; should the result be simplified 
#' to a vector, matrix or higher dimensional array if possible? For sapply it 
#' must be named and not abbreviated. The default value, TRUE, returns a vector 
#' or matrix if appropriate, whereas if simplify = "array" the result may be an 
#' \code{\link[base]{array}} of "rank" (=length(dim(.))) one higher than the 
#' result of FUN(X[[i]]).
#' @param USE.NAMES logical; if TRUE and if X is character, use X as 
#' \code{\link[base]{names}} for the result unless it had names already. Since 
#' this argument follows ... its name cannot be abbreviated.
#' 
#' @examples 
#' x <- sapply(1:10, function(x) runif(10))
#' y <- lapply(1:10, function(x) runif(10))
#' 
#' pbapply(x, 1, mean)
#' pbsapply(y, mean)
#' pblapply(y, mean)
#' 
#' @description Wrap \code{\link[base]{apply}} family with progress bars.
#' @seealso \code{\link[base]{apply}}, \code{\link[base]{sapply}}, 
#' \code{\link[base]{lapply}}, \code{\link[base]{replicate}}
#' @rdname pbapply
#' @export
pbapply <-
function (X, MARGIN, FUN, ...) 
{
    FUN <- match.fun(FUN)
    dl <- length(dim(X))
    if (!dl) 
        stop("dim(X) must have a positive length")
    if (is.object(X)) 
        X <- if (dl == 2L) 
            as.matrix(X)
        else as.array(X)
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn))) 
            stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN)) 
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 
            1L))
        ans <- forceAndCall(1, FUN, if (length(d.call) < 2L) newX[, 
            1] else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) < 
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)

    pb <- startpb(0, d2) # pb_specific_code

    if (length(d.call) < 2L) {
        if (length(dn.call)) 
            dimnames(newX) <- c(dn.call, list(NULL))
        for (i in 1L:d2) {
            tmp <- forceAndCall(1, FUN, newX[, i], ...)
            if (!is.null(tmp)) 
                ans[[i]] <- tmp
        
            setpb(pb, i) # pb_specific_code
            
        }
    }
    else for (i in 1L:d2) {
        tmp <- forceAndCall(1, FUN, array(newX[, i], d.call, 
            dn.call), ...)
        if (!is.null(tmp)) 
            ans[[i]] <- tmp
        
        setpb(pb, i) # pb_specific_code
        
    }

    closepb(pb) # pb_specific_code

    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])
    ans.names <- names(ans[[1L]])
    if (!ans.list) 
        ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    if (!ans.list && length(ans.names)) {
        all.same <- vapply(ans, function(x) identical(names(x), 
            ans.names), NA)
        if (!all(all.same)) 
            ans.names <- NULL
    }
    len.a <- if (ans.list) 
        d2
    else length(ans <- unlist(ans, recursive = FALSE))
    if (length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if (length(dn.ans[[1L]])) 
            dn.ans[[1L]]
        return(ans)
    }
    if (len.a == d2) 
        return(array(ans, d.ans, dn.ans))
    if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans)) 
            dn.ans <- vector(mode = "list", length(d.ans))
        dn1 <- if (length(dn.call) && length(ans.names) == length(dn.call[[1L]])) 
            dn.call[1L]
        else list(ans.names)
        dn.ans <- c(dn1, dn.ans)
        return(array(ans, c(len.a%/%d2, d.ans), if (!is.null(names(dn.ans)) || 
            !all(vapply(dn.ans, is.null, NA))) dn.ans))
    }
    return(ans)
}

#' @rdname pbapply
#' @export
pbsapply <-
  function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
  {
    FUN <- match.fun(FUN)
    answer <- pblapply(X = X, FUN = FUN, ...) # pb_specific_code
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
      names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer)) 
      simplify2array(answer, higher = (simplify == "array"))
    else answer
  }

#' @rdname pbapply
#' @export
pblapply <-
function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    B <- length(X)
    if (!(interactive() && dopb() && B >= 1)) 
        return(lapply(X, FUN, ...))
    pb <- startpb(0, B)
    rval <- vector("list", B)
    for (i in 1:B) {
        rval[i] <- list(FUN(X[[i]], ...))
        setpb(pb, i)
    }
    close(pb)
    names(rval) <- names(X)
    rval
}

#' @rdname pbapply
#' @export
pbreplicate <-
  function (n, expr, simplify = "array") 
    pbsapply(integer(n), eval.parent(substitute(function(...) expr)), 
             simplify = simplify)
