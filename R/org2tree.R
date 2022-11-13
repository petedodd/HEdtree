##' Utility function for \code{org2LoL}
##' 
##' This does the work underlying \code{org2LoL} and \code{org2tree}.
##' The function recursively evaluates to parse Emacs org-mode files. 
##'
##' @param inp 
##' @param lvl 
##' @param LVLS 
##' @return A \code{list} of \code{list}s
##' @author Pete Dodd
##' @export
orgAns0 <- function(inp,lvl,LVLS){
  tpz <- which(LVLS==lvl);
  ans <- list()
  if( length(tpz)>0 ){
    for(i in seq_along(tpz)){
      st <- tpz[i]
      if(i < length(tpz))
        nd <- tpz[i+1]
      else nd <- length(inp)
      ans[[inp[st]]] <- orgAns0(inp[st:nd],lvl+1,LVLS[st:nd])
    }
  }
  ans
}


##' Emacs org-mode file to list-of-lists
##' 
##' Reads and Emacs org-mode file and generates a list-of-lists.
##' This is used primarily as an intermediate: \code{org2tree}
##' returns a \code{data.tree} directly.
##'
##' @param fn the filename
##' @return A \code{list} of \code{list}s
##' @author Pete Dodd
##' @export
org2LoL <- function(fn){
  ## get text
  con <- file(fn)
  txt <- readLines(con)
  close(con)
  ## restrict to org bullets
  bulz <- grepl("^\\*+\\s",txt)
  txt <- txt[bulz]
  lvls <- as.numeric(regexpr("\\s",txt))-1 #get level of bullet
  txt <- gsub("^\\*+\\s","",txt)        #strip stars
  txt <- gsub("^\\s+|\\s+$", "", txt)   #strip leading/trailing space
  return(orgAns0(txt,1,lvls))
}


##' Emacs org-mode file to \code{data.tree} tree
##' 
##' Reads and Emacs org-mode file and generates a \code{data.tree} tree object.
##' This enables rapid specification of and experimentation with tree logics.
##' Different levels of the hierarchy as specified by the number of asterisks
##' the line begins with (a space must follow the asterisks).
##' Emacs provides folding and other tools for rapidly editing org-mode files.
##'
##' @param fn the filename
##' @return A \code{data.tree} tree object
##' @author Pete Dodd
##' @export
org2tree <- function(fn){
  data.tree::as.Node(org2LoL(fn=fn))
}

##' Text file to \code{data.tree} tree
##' 
##' This function reads an MS word organisation chart (saved as plain text)
##' file and generates a \code{data.tree} tree object.
##' This enables rapid specification of and experimentation with tree logics.
##' Different levels of the hierarchy are specified in the text file by the
##' number of tabs the line begins with.
##'
##' @param fn the filename
##' @return A \code{data.tree} tree object
##' @author Pete Dodd
##' @export
MSorg2tree <- function(fn){
  txt <- readLines(fn)
  tmp <- gsub("\\t","\\*",txt)
  tmp <- paste0("*",tmp)
  txt <- stringr::str_replace(tmp, "^(\\*+)", "\\1 ")
  ## see above
  bulz <- grepl("^\\*+\\s",txt)
  txt <- txt[bulz]
  lvls <- as.numeric(regexpr("\\s",txt))-1 #get level of bullet
  txt <- gsub("^\\*+\\s","",txt)        #strip stars
  txt <- gsub("^\\s+|\\s+$", "", txt)   #strip leading/trailing space
  tmp <- orgAns0(txt,1,lvls)
  data.tree::as.Node(tmp)
}

