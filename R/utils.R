closepb <-
function(pb)
    if (is.null(pb))
        invisible(NULL) else close(pb)

getpb <-
function(pb)
{
    if (dopb()) {
        progress.bar <- getOption("pboptions")$type
        rval <- switch(progress.bar, 
            txt = utils::getTxtProgressBar(pb), 
            tk = tcltk::getTkProgressBar(pb),
            #win = utils::getWinProgressbar(pb)
            )
    } else rval <- NULL
    rval
}

setpb <-
function(pb, value)
{
    if (dopb()) {
        control <- getOption("pboptions")
        rval <- switch(control$type, 
            txt = utils::setTxtProgressBar(pb, value), 
            tk = tcltk::setTkProgressBar(pb, value, label=control$label),
            # win = utils::setWinProgressBar(pb, value, label=control$label)
            )
    } else rval <- NULL
    invisible(rval)
}

dopb <-
function()
{
    progress.bar <- getOption("pboptions")$type
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", "win", "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    interactive() && !is.null(progress.bar)
}

startpb <-
function(min=0, max=1)
{
    if (dopb()) {
        control <- getOption("pboptions")
        pb <- switch(control$type, 
            txt = utils::txtProgressBar(min, max, initial=control$initial,
                style = control$style, width = control$txt.width, char = control$char),
            # win = utils::winProgressBar(min=min, max=max, initial=control$initial,
            #     title = control$title, label = control$label, width = control$gui.width),
            tk = tcltk::tkProgressBar(min=min, max=max, initial=control$initial,
                title = control$title, label = control$label, width = control$gui.width))
    } else pb <- NULL
    invisible(pb)
}
