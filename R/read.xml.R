##' This function is designed to read in trees from draw.io exported as
##' (uncompressed) xml
##' @details
##' Node names are arbitrary (but <br> is stripped from strings).
##' Edge data is best specified as p=1,c=c2 etc. p is treated as a probability.
##' Here a variable called c is created, but a different name and an arbitrary number
##' of other variables can be added. Whitespace is stripped.
##' A utility variable called check is added to leaves to facilitate testing/debugging.
##' @title Reads a tree and returns
##' @param fn The filename of the file to be read.
##' @return A data.tree of with specified node names and data.
##' @author Pete Dodd
##' @import data.table
##' @import data.tree
##' @import xml2
##' @export
read.xml <- function(fn){
  ## check filename for xml
  dat <-  xml2::read_xml(fn) #read in
  if(bot(fn)!='xml') stop('Expecting an xml file! Rename if you are sure it is one!')

  ## parse cells
  mxc <- xml2::xml_find_all(dat,".//mxCell")
  if(length(mxc)==0) stop('No mxCell tags found! Is the input file from draw.io?')

  ## use this to grab edges and vertices
  VL <- EL <- CL <- SL <- XL <- list()
  for( i in 1:length(mxc) ){
    T <- xml2::xml_attrs(mxc[i])[[1]]
    if('vertex' %in% names(T) & !('connectable' %in% names(T)) ){
      VL[[i]] <- data.table::data.table(vid=T['id'],vvalue=T['value'])
      T2 <- xml_attrs(xml_find_all(mxc[i],".//mxGeometry"))[[1]]
      XL[[i]] <- data.table::data.table(vid=T['id'],
                                        vvalue=T['value'],
                                        x=as.numeric(T2['x']),
                                        y=as.numeric(T2['y']),
                                        width=as.numeric(T2['width']),
                                        height=as.numeric(T2['height'])
                                        )
    }
    if('edge' %in% names(T)) {
      EL[[i]] <- data.table::data.table(eid=T['id'],
                                        evalue=T['value'],
                                        src=T['source'],
                                        target=T['target'])
      if(is.na(T['source'])){           #by geom if missing
        T2 <- xml_attrs(xml_find_all(mxc[i],".//mxPoint"))[[1]]
        if(T2['as']=='sourcePoint')
          SL[[i]] <- data.table::data.table(eid=T['id'],
                                            evalue=T['value'],
                                            src.x=as.numeric(T2['x']),
                                            src.y=as.numeric(T2['y']),
                                            target=T['target'])
      }
    }
    if( 'vertex' %in% names(T) & 'connectable' %in% names(T) ){
      CL[[i]] <- data.table::data.table(eid=T['id'],evalue=T['value'],src=T['parent'])
    }
  }
  VL <- data.table::rbindlist(VL)
  EL <- data.table::rbindlist(EL)
  CL <- data.table::rbindlist(CL)
  XL <- data.table::rbindlist(XL)
  SL <- data.table::rbindlist(SL)

  ## dealing with edges of 'connectable' form
  if(nrow(CL)>0){
    EL <- merge(EL,CL[,.(eid=src,ev2=evalue)],by='eid',all.x=TRUE)
    EL[is.na(evalue),evalue:=ev2]; EL[,ev2:=NULL]
  }

  ## merge edge data in
  VL <- merge(VL,EL,by.x = 'vid', by.y = 'target',
              all.x = TRUE, all.y=FALSE)

  ## get rid of <br>
  VL[,vvalue:=nobr(vvalue)]

  ## deal with edges not logically connected
  if(nrow(SL)){
    warning('Some connections are based on location. Please check tree geometry is as expected.\n')
    for(i in 1:nrow(SL)){
      cat('Seaching by location for parent to node:',
          VL[vid==SL[i,target],vvalue],'\n')
      px <- c(SL[i,src.x],SL[i,src.y])
      src0 <- NA
      for(j in 1:nrow(XL))
        if( insq(px,XL[j,x],XL[j,y],XL[j,width],XL[j,height]) ) src0 <- XL[j,vid]
      if(!is.na(src0)) VL[vid==SL[i,target],src:=src0]
    }
  }

  ## add in srcnm
  tmp <- VL[,c('vid','vvalue')]
  names(tmp) <- c('src','srcnm')
  if(any(is.na(tmp$src)))
    tmp <- tmp[!is.na(tmp$src)]
  VL <- merge(VL,tmp,by='src',all.x=TRUE) #no

  ## parse edge data
  if(!all(is.na(VL$evalue))){             #edge data?
    for(i in 1:nrow(VL)){
      if(!is.na(VL[i]$evalue)){
        if(grepl(',',VL[i]$evalue)){      # CSV
          tt <- read.csv(text=VL[i]$evalue,header=FALSE, #parse CSV
                         strip.white = TRUE,stringsAsFactors = FALSE)
          for( k in 1:length(tt)){
            if(!grepl('=',tt[k])) stop('Use of commas in edge labels requires explicit variable=value syntax in all slots!')
            v <- keyvalue(tt[k])          #split on =
            VL[i,c(v[1]):=v[2]]           #add to data
          }                               #loop over variables
        } else {                          #assume probability as non-CSV
          VL[i,p:=nospace(evalue)]
        }
      }                                   #end check there's work
    }                                     #end loop
  }                                       #end check any edge data

  ## new go at pathstring parsing
  rid <- VL[is.na(VL$srcnm)]$vvalue
  if(length(rid)==0) stop('No root found!')
  if(length(rid)>1) stop('Multiple root candidates found!')
  VL$pathString <- VL$vvalue
  for(i in 1:nrow(VL)){
    nw <- VL$vvalue[i]
    nxt <- VL[VL$vvalue==nw]$srcnm
    sfty <- 0
    while( !is.na(nxt) & sfty<nrow(VL) ){
      VL$pathString[i] <- paste(nxt,VL$pathString[i],sep='/')
      nw <- nxt
      nxt <- VL[VL$vvalue==nw]$srcnm
      sfty <- sfty + 1
    }
  }

  ## clean out some variables
  VL[,c('src','vid','vvalue','eid','evalue','srcnm'):=NULL]

  ## convert to data.tree object
  DT <- data.tree::as.Node(VL)

  ## add debug checks
  DT$Set(check=0)
  DT$Set(check=1,filterFun=data.tree::isLeaf)
  return(DT)
}

##' @title Utility nobr strips <br>
##' @param x string or vector thereof
##' @return string or vector thereof
##' @author Pete Dodd
nobr <- function(x) gsub("<br>","",x)

##' @title Utility strips space etc
##' @param x string or vector thereof
##' @return string or vector thereof
##' @author Pete Dodd
nospace <- function(x){
  x <- gsub("\\s+","",x)
  x <- gsub("<br>","",x)
  x <- gsub("&nbsp;","",x)
  x
}

##' @title Utility to split edge data on =
##' @param x string or vector thereof
##' @return string or vector thereof
##' @author Pete Dodd
keyvalue <- function(x){
  x <- as.character(x)
  ss <- strsplit(x,'=')[[1]]
  nospace(ss)
}

##' @title Utility to get last split
##' @param x string
##' @return string
##' @author Pete Dodd
bot <- function(x){
  tmp <- strsplit(x,split = '\\.')[[1]]
  tmp[length(tmp)]
}

##' @title Utility to check if in a box
##' @param p 2-vector location
##' @param X x value of top left
##' @param Y y value of top left (measured downwards)
##' @param W width
##' @param H height
##' @param tol a tolerance by which the box is expanded
##' @return boolean
##' @author Pete Dodd
insq <- function(p,X,Y,W,H,tol=10){            #measures y downwards
  all((p[1]<=X+W+tol),(p[1]>=X-tol),(p[2]<=Y+H+tol),(p[2]>=Y-tol))
}
