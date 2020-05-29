cat('## setwd("absolute path of a directory containing the input data files")',file="code.R",append=TRUE,sep="\n")
cat("options(warn=-1)
library(circlize)
library(RColorBrewer)
library(GenomicRanges)
library(data.table)
library(RLumShiny)
library(grDevices)",file="code.R",append=TRUE,sep="\n")  
cat("",file="code.R",append=TRUE,sep="\n")
cat('plotcircos <- function(x, color, height, plotTypes, units, rotation, gap.width, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new(x,plotType=plotTypes,unit=units)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column=4, connection_height=heightlabelschr, track.margin=c(0.01,marginlabelschr), side="outside")
  }		
  circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = height)	
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column=4, connection_height=heightlabelschr, track.margin=c(0.01,marginlabelschr), side="inside")
  }		
}

plotcircos.notrack <- function(x, plotTypes, units, rotation, gap.width, data.CN, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new(x,plotType=plotTypes,unit=units)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "inside")
  }
}

plotcircos.font <- function(x, color, height, plotTypes, units, rotation, gap.width, cexLabel, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new.font(x, plotType=plotTypes, unit=units, cexlabel=cexLabel)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = height)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "inside")
  }	
}

plotcircos.notrack.font <- function(x, plotTypes, units, rotation, gap.width, cexLabel, data.CN, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr){  
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new.font(x, plotType=plotTypes, unit=units, cexlabel=cexLabel)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "inside")
  }	
}

plotcircos.cyto <- function(x, height, plotTypes, units, rotation, gap.width, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){ 
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new(x, plotType = plotTypes, unit=units)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(x, ylim = c(0, 1), bg.border = NA, 
                                track.height = height, panel.fun = function(region, value, ...){
                                  col = cytoband.col(value[[2]])
                                  circos.genomicRect(region, value, ybottom = 0, 
                                                     ytop = 1, col = col, border = NA, ...)
                                  xlim = get.cell.meta.data("xlim")
                                  circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
                                }, cell.padding = c(0, 0, 0, 0))  
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "inside")
  }	
}

plotcircos.cyto.font <- function(x, height, plotTypes, units, rotation, gap.width, cexLabel, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new.font(x, plotType=plotTypes, unit=units, cexlabel=cexLabel)  
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(x, ylim = c(0, 1), bg.border = NA, 
                                track.height = height, panel.fun = function(region, value, ...){
                                  col = cytoband.col(value[[2]])
                                  circos.genomicRect(region, value, ybottom = 0, 
                                                     ytop = 1, col = col, border = NA, ...)
                                  xlim = get.cell.meta.data("xlim")
                                  circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
                                }, cell.padding = c(0, 0, 0, 0))
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="inner"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "inside")
  }	
}

circos.genomicInitialize.new <- 
  function (data, sector.names = NULL, major.by = NULL, unit = "", plotType, tickLabelsStartFromZero = TRUE, track.height = 0.05, 
            ...) 
  {
    if(is.factor(data[[1]])){
      fa = levels(data[[1]])
    }
    else {
      fa = unique(data[[1]])
    }
    if(!is.null(sector.names)){
      if(length(sector.names) != length(fa)){
        stop("length of `sector.names` and length of sectors differ.")
      }
    }
    else {
      sector.names = fa
    }
    names(sector.names) = fa
    x1 = tapply(data[[2]], data[[1]], min)[fa]
    x2 = tapply(data[[3]], data[[1]], max)[fa]
    op = circos.par("cell.padding")
    ow = circos.par("points.overflow.warning")
    circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
    circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, 
                                                            x2), ...)
    if(any(plotType %in% c("axis", "labels"))){
      circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, 
                                    track.height = track.height, panel.fun = function(region, 
                                                                                      value, ...){
                                      sector.index = get.cell.meta.data("sector.index")
                                      xlim = get.cell.meta.data("xlim")
                                      if(tickLabelsStartFromZero){
                                        offset = xlim[1]
                                        if(is.null(major.by)){
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(xlim[1], xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if(major.by > 1e+06){
                                          major.tick.labels = paste((major.at - offset)/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if(major.by > 1000){
                                          major.tick.labels = paste((major.at - offset)/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste((major.at - offset), 
                                                                    "bp", sep = "")
                                        }
                                      }
                                      else {
                                        if(is.null(major.by)){
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(floor(xlim[1]/major.by) * major.by, 
                                                       xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if(major.by > 1e+06){
                                          major.tick.labels = paste(major.at/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if(major.by > 1000){
                                          major.tick.labels = paste(major.at/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste(major.at, "bp", 
                                                                    sep = "")
                                        }
                                      }
                                      
                                      if(unit==""){ major.tick.labels <- gsub("[mkbp]","",major.tick.labels,ignore.case = T)}
                                      
                                      if(all(c("axis", "labels") %in% plotType)){
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * par("cex"), labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                        circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], 
                                                    cex = par("cex")-0.1, adj = c(0.5, -0.1*par("cex")*6-(par("cex")-1)*3), niceFacing = TRUE)
                                      }
                                      else if("labels" %in% plotType){
                                        circos.text(mean(xlim), 0, labels = sector.names[sector.index], 
                                                    cex = par("cex")-0.1, adj = c(0.5, -0.1*par("cex")*6-(par("cex")-1)*3), niceFacing = TRUE)
                                      }
                                      else if("axis" %in% plotType){
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * par("cex"), labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                      }
                                    })
    }
    circos.par(cell.padding = op, points.overflow.warning = ow)
    return(invisible(NULL))
  }
  
circos.genomicInitialize.new.font <- 
  function (data, sector.names = NULL, major.by = NULL, unit = "", plotType, tickLabelsStartFromZero = TRUE, track.height = 0.05, cexlabel, 
            ...) 
  {
    if(is.factor(data[[1]])){
      fa = levels(data[[1]])
    }
    else {
      fa = unique(data[[1]])
    }
    if(!is.null(sector.names)){
      if(length(sector.names) != length(fa)){
        stop("length of `sector.names` and length of sectors differ.")
      }
    }
    else {
      sector.names = fa
    }
    names(sector.names) = fa
    x1 = tapply(data[[2]], data[[1]], min)[fa]
    x2 = tapply(data[[3]], data[[1]], max)[fa]
    op = circos.par("cell.padding")
    ow = circos.par("points.overflow.warning")
    circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)
    circos.initialize(factor(fa, levels = fa), xlim = cbind(x1, 
                                                            x2), ...)
    if(any(plotType %in% c("axis", "labels"))){
      circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, 
                                    track.height = track.height, panel.fun = function(region, 
                                                                                      value, ...){
                                      sector.index = get.cell.meta.data("sector.index")
                                      xlim = get.cell.meta.data("xlim")
                                      if(tickLabelsStartFromZero){
                                        offset = xlim[1]
                                        if(is.null(major.by)){
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(xlim[1], xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if(major.by > 1e+06){
                                          major.tick.labels = paste((major.at - offset)/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if(major.by > 1000){
                                          major.tick.labels = paste((major.at - offset)/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste((major.at - offset), 
                                                                    "bp", sep = "")
                                        }
                                      }
                                      else {
                                        if(is.null(major.by)){
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(floor(xlim[1]/major.by) * major.by, 
                                                       xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if(major.by > 1e+06){
                                          major.tick.labels = paste(major.at/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if(major.by > 1000){
                                          major.tick.labels = paste(major.at/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste(major.at, "bp", 
                                                                    sep = "")
                                        }
                                      }
                                      
                                      if(unit==""){ major.tick.labels <- gsub("[mkbp]","",major.tick.labels,ignore.case = T)}
									  
                                      if(all(c("axis", "labels") %in% plotType)){
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * cexlabel, labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                        circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], 
                                                    cex = cexlabel, adj = c(0.5, -0.1*cexlabel*6-(cexlabel-1)*3), niceFacing = TRUE)
                                      }
                                      else if("labels" %in% plotType){
                                        circos.text(mean(xlim), 0, labels = sector.names[sector.index], 
                                                    cex = cexlabel, adj = c(0.5, -0.1*cexlabel*6-(cexlabel-1)*3), niceFacing = TRUE)
                                      }
                                      else if("axis" %in% plotType){
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * cexlabel, labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                      }
                                    })
    }
    circos.par(cell.padding = op, points.overflow.warning = ow)
    return(invisible(NULL))
  }
  
.default.major.by = function(sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")){
	d = circos.par("major.by.degree")
	cell.start.degre = get.cell.meta.data("cell.start.degree", sector.index, track.index)
	tm = reverse.circlize(c(cell.start.degre, cell.start.degre-d), rep(get.cell.meta.data("cell.bottom.radius", sector.index = sector.index, track.index = track.index), 2))
	major.by = abs(tm[1, 1] - tm[2, 1])
	digits = as.numeric(gsub("^.*e([+-]\\\\d+)$", "\\\\1", sprintf("%e", major.by)))
	major.by = round(major.by, digits = -1*digits)
	return(major.by)
}

get_most_inside_radius = function() {
	tracks = get.all.track.index()
	if(length(tracks) == 0) {
	   1
	}else{
	   n = length(tracks)
	   get.cell.meta.data("cell.bottom.radius", track.index = tracks[n]) - get.cell.meta.data("track.margin", track.index = tracks[n])[1] - circos.par("track.margin")[2]
	}
}',file="code.R",append=TRUE,sep="\n")
cat("",file="code.R",append=TRUE,sep="\n")
cat(paste("data.C.name <- ",'"',data.C.name,'"',sep=""),file="code.R",append=TRUE,sep="\n")
cat('data.C <- data.frame(fread(data.C.name),stringsAsFactors=F)',file="code.R",append=TRUE,sep="\n")
cat("data.C[,2] <- as.numeric(data.C[,2])
data.C[,3] <- as.numeric(data.C[,3])",file="code.R",append=TRUE,sep="\n")
cat(paste('data.T.file <- c("',paste(uploadTrackfile.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")
if(!is.null(uploadTrackfile.export)){
  cat("data.T <- lapply(1:length(data.T.file),function(x){
		  if(!is.null(data.T.file[x])){
		  data.frame(fread(data.T.file[x]),stringsAsFactors=F)
		  }
		  })",file="code.R",append=TRUE,sep="\n")
} 

if(!is.null(data.CN.name)){
  cat(paste("data.CN.name <- ",'"',data.CN.name,'"',sep=""),file="code.R",append=TRUE,sep="\n")
  cat('data.CN <- data.frame(fread(data.CN.name),stringsAsFactors=F)',file="code.R",append=TRUE,sep="\n")
  cat("data.CN[,2] <- as.numeric(data.CN[,2])
  data.CN[,3] <- as.numeric(data.CN[,3])",file="code.R",append=TRUE,sep="\n")
}else{
  cat('data.CN <- NULL',file="code.R",append=TRUE,sep="\n")
}		  
cat(paste('data.N.file <- c("',paste(markTrackfile.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n") 	
if(!is.null(markTrackfile.export)){
  cat(paste("uploadtrack <- c(",paste(uploadtrack.export,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")		                 
  cat("data.N <- lapply(1:10,function(x){
			 if(uploadtrack[x] == 2 && nchar(data.N.file[x])>0){	  
		     data.frame(fread(data.N.file[x]),stringsAsFactors=F)
			 }
			 })",file="code.R",append=TRUE,sep="\n")
}
if(length(data.T) == 0){
  cat("data.T <- NULL",file="code.R",append=TRUE,sep="\n")
}			  			  
cat(paste("trackindx <- c(",paste(trackindx,collapse =","),")",sep=""),"data.N <- data.N[trackindx]",file="code.R",append=TRUE,sep="\n")
if(length(data.N) == 0){
  cat("data.N <- NULL",file="code.R",append=TRUE,sep="\n")
}

if(!linksTrack.export){
  cat("data.L <- NULL",file="code.R",append=TRUE,sep="\n")
}else if(linksTrack.export && !is.null(linksFile.export)){
  cat(paste("data.L <- data.frame(fread(",'"',linksFile.name,'"',"),stringsAsFactors=F)",sep=""),file="code.R",append=TRUE,sep="\n")		  
  if(ncol(data.L)==6 | ncol(data.L)==7){
    cat("data.L1 <- data.L[,1:3]
		     data.L2 <- data.L[,4:6]
		     data.L1[,2] <- as.numeric(data.L1[,2])
		     data.L1[,3] <- as.numeric(data.L1[,3])
		     data.L2[,2] <- as.numeric(data.L2[,2])
		     data.L2[,3] <- as.numeric(data.L2[,3])	  
		     data.L1$num <- 1:nrow(data.L1)
             data.L2$num <- 1:nrow(data.L2)
		     rownames(data.L1) <- data.L1$num
		     rownames(data.L2) <- data.L2$num",file="code.R",append=TRUE,sep="\n")
  }
}
if(!is.null(data.L)){
  if(nchar(hltdataLinks)!=0){
    cat(paste('hltdataLinks <- "',hltdataLinks,'"',sep=""),file="code.R",append=TRUE,sep="\n")		   
    cat('tmpL <- matrix(strsplit(hltdataLinks, "\\n")[[1]])
		   colnamesL <- c("chr","start","end","color")
		   datL <- matrix(0, length(tmpL), length(colnamesL))
		   colnames(datL) <- colnamesL
		   for(l in 1:length(tmpL)){
		      rowL <- strsplit(tmpL[l], ",")[[1]]
                  if(length(rowL)==4){                                        
                    datL[l,] <- rowL
                  }
		   }
		   datL <- data.frame(datL,stringsAsFactors=F)
		   datL$start <- as.numeric(datL$start)
		   datL$end <- as.numeric(datL$end)
		   datL$color <- datL$color
		   queryL <- GRanges(seqnames = datL$chr,ranges=IRanges(start=datL$start,end=datL$end),seqinfo=NULL)
		   subj1 <- GRanges(seqnames = data.L1[,1],ranges=IRanges(start=data.L1[,2],end=data.L1[,3]),seqinfo=NULL)
		   subj2 <- GRanges(seqnames = data.L2[,1],ranges=IRanges(start=data.L2[,2],end=data.L2[,3]),seqinfo=NULL)
           indx1 <- findOverlaps(queryL,subj1)
           indx1 <- data.frame(indx1,stringsAsFactors=F)
           indx1$queryHits <- as.numeric(indx1$queryHits)
           indx1$subjectHits <- as.numeric(indx1$subjectHits)
           hltregion1 <- data.L1[indx1$subjectHits,]
           data.LL1 <- data.L1
           hltregion1$color <- datL$color[indx1[,1]]
           indx2 <- findOverlaps(queryL,subj2)
           indx2 <- data.frame(indx2,stringsAsFactors=F)
           indx2$queryHits <- as.numeric(indx2$queryHits)
           indx2$subjectHits <- as.numeric(indx2$subjectHits)
           hltregion2 <- data.L2[indx2$subjectHits,]
           data.LL2 <- data.L2
           hltregion2$color <- datL$color[indx2[,1]]',file="code.R",append=TRUE,sep="\n")				
  }
}
cat('for(i in 1:length(data.T.file)){
  assign(paste("hltdata",i,sep=""),"")
}',file="code.R",append=TRUE,sep="\n")				
if(!is.null(data.T)){
  for(k in 1:length(data.T)){			
    if(nchar(hltdata.List[[k]])>0){
      assign(paste("hltdata",k,sep=""),hltdata.List[[k]])			
      cat(paste("hltdata",k,' <- "',get(paste("hltdata",k,sep="")),'"',sep=""),file="code.R",append=TRUE,sep="\n")
    }else{
      assign(paste("hltdata",k,sep=""),NULL)			
      cat(paste("hltdata",k,' <- ""',sep=""),file="code.R",append=TRUE,sep="\n")				
    }
  }
}
cat('hltregion.List <- list()',file="code.R",append=TRUE,sep="\n")
cat('if(!is.null(data.T)){
			for(k in 1:length(data.T)){
			data.TT <- data.T[[k]]
			hltregion.List[[k]] <- ""',file="code.R",append=TRUE,sep="\n")			
cat(paste('if(nchar(get(paste("hltdata",k,sep="")))>0){'),file="code.R",append=TRUE,sep="\n")
cat('tmp <- matrix(strsplit(get(paste("hltdata",k,sep="")), "\\n")[[1]])
            myColnames <- c("chr","start","end","color")
            data <- matrix(0, length(tmp), length(myColnames))
            colnames(data) <- myColnames
            for(p in 1:length(tmp)){
                 myRow <- strsplit(tmp[p], ",")[[1]]
                  if(length(myRow)==4){                                        
                    data[p,] <- myRow
                  }
               }
            data <- data.frame(data,stringsAsFactors=F)
            data$start <- as.numeric(data$start)
            data$end <- as.numeric(data$end)
			query <- GRanges(seqnames = data$chr,ranges=IRanges(start=data$start,end=data$end),seqinfo=NULL)
            subj <- GRanges(seqnames = data.TT[,1],ranges=IRanges(start=data.TT[,2],end=data.TT[,3]),seqinfo=NULL) 
            indx <- findOverlaps(query,subj)
            indx <- data.frame(indx,stringsAsFactors=F)
			indx$queryHits <- as.numeric(indx$queryHits)
			indx$subjectHits <- as.numeric(indx$subjectHits)
            hltregion <- data.TT[indx$subjectHits,]
			hltregion$color <- data$color[indx[,1]]
			hltregion$id <- paste(hltregion[,1],hltregion[,2],hltregion[,3],sep="")
			hltregion.List[[k]] <- hltregion
			}
			}
			}',file="code.R",append=TRUE,sep="\n")
cat("",file="code.R",append=TRUE,sep="\n")
cat(paste('pdf("shinyCircos.pdf", width=',widthSize,"/72,"," height=",heightSize,"/72)",sep=""),paste('## svg("shinyCircos.svg", width=',widthSize,"/72,"," height=",heightSize,"/72)",sep=""),file="code.R",append=TRUE,sep="\n")
## *** The gap width ***
repnumgap <- round(length(unique(data.C[,1]))/length(gap.width))+1
gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]			
gap.width <- as.numeric(gap.width)
rotation <- gap.width[length(gap.width)]/2	
if(fontSize=="custom"){
  cat(paste("cexlabel <- ",cexlabel,sep=""),file="code.R",append=TRUE,sep="\n")
  if(length(legendtext)!=0 && addlegend==1 && poslegend==1){
    cat("par(oma=c(0,0,0,0), mar=c(9,0.5,1,9.5), xpd=TRUE, cex=cexlabel-0.1)",file="code.R",append=TRUE,sep="\n")
  }else{
    cat("par(mar=c(0.6,0.6,0.6,0.6), cex=cexlabel-0.1)",file="code.R",append=TRUE,sep="\n")
  }
}else{
  cat(paste("fontSize <- ",fontSize,sep=""),file="code.R",append=TRUE,sep="\n")			
  if(length(legendtext)!=0 && addlegend==1 && poslegend==1){
    cat("par(oma=c(0,0,0,0), mar=c(9,0.5,1,9.5), xpd=TRUE, cex=fontSize-0.05)",file="code.R",append=TRUE,sep="\n")
  }else{
    cat("par(mar=c(0.6,0.6,0.6,0.6), cex=fontSize-0.05)",file="code.R",append=TRUE,sep="\n")
  }			
}
cat(paste('trackChr <- "',trackChr,'"',sep=""),file="code.R",append=TRUE,sep="\n")
if(outAxis == 1){
	plotTypes <- unique(c(labelChr,"axis"))
}else{
	plotTypes <- NULL
}																																									
cat(paste("plotTypes <- ",'"',plotTypes,'"',sep=""),paste("unitChr <- ",'"',unitChr,'"',sep=""),paste("rotation <- ",rotation,sep=""),paste("gap.width <- c(",paste(gap.width,collapse = ","),")",sep=""),paste("labeltextchr <- ",labeltextchr,sep=""),
    paste("poslabelschr <- ",'"',poslabelschr,'"',sep=""),paste("heightlabelschr <- ",heightlabelschr,sep=""),paste("marginlabelschr <- ",marginlabelschr,sep=""),file="code.R",append=TRUE,sep="\n")			
if(datatypeChr=="general"){
  if(trackChr=="track" && fontSize!="custom"){
    colorChr <- gsub("0x","#", colorChr)        			
    repnumcol <- round(length(unique(data.C[,1]))/length(colorChr))+1
    colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]
    cat(paste('colorChr <- c("',paste(colorChr,collapse = '","'),'")',sep=""),paste("heightChr <- ",heightChr,sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos(data.C, height=heightChr, color=colorChr, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)',file="code.R",append=TRUE,sep="\n")
  }else if(trackChr=="track" && fontSize=="custom"){
    repnumcol <- round(length(unique(data.C[,1]))/length(colorChr))+1
    colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]			
    cat(paste('colorChr <- c("',paste(colorChr,collapse = '","'),'")',sep=""),paste("cexlabel <- ",paste(cexlabel,collapse = ","),sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos.font(data.C, height=heightChr, color=colorChr, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)',file="code.R",append=TRUE,sep="\n")				
  }else if(trackChr!="track" && fontSize!="custom"){
    cat(paste("cexlabel <- ",paste(cexlabel,collapse = ","),sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos.notrack(data.C, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, data.CN=data.CN, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr)',file="code.R",append=TRUE,sep="\n")								
  }else if(trackChr!="track" && fontSize=="custom"){
    cat(paste("cexlabel <- ",paste(cexlabel,collapse = ","),sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos.notrack.font(data.C, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, data.CN=data.CN, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr)',file="code.R",append=TRUE,sep="\n")														
  }
}else{
  if(fontSize!="custom"){			    
    cat(paste("cexlabel <- ",paste(cexlabel,collapse = ","),sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos.cyto(data.C, height=heightChr, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)',file="code.R",append=TRUE,sep="\n")
  }else if(fontSize=="custom"){			
    cat(paste("cexlabel <- ",paste(cexlabel,collapse = ","),sep=""),file="code.R",append=TRUE,sep="\n")
    cat('plotcircos.cyto.font(data.C, height=heightChr, plotTypes=plotTypes, units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)',file="code.R",append=TRUE,sep="\n")																
  }
}
if(!is.null(data.T)){
  cat("takindx <- 1",file="code.R",append=TRUE,sep="\n")
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1){
    cat("takindx <- takindx+2",file="code.R",append=TRUE,sep="\n")			   
  }		 
  cat(paste('typeTrack <- c("',paste(typeTrack,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")  
  for(i in 1:length(data.T)){
    cat(paste("i <- ",i,sep=""),file="code.R",append=TRUE,sep="\n")
    data.TT <- data.T[[i]]
    tktype <- typeTrack[i]
    data.TT[,2] <- as.numeric(data.TT[,2])
    data.TT[,3] <- as.numeric(data.TT[,3])
    data.NN <- data.N[[i]]				
    ## *** The fill color for track ***	
    data.TT$num <- 1:nrow(data.TT)		   
    cat("data.TT <- data.T[[i]]
	tktype <- typeTrack[i]
	data.TT[,2] <- as.numeric(data.TT[,2])
	data.TT[,3] <- as.numeric(data.TT[,3])
	data.NN <- data.N[[i]]
	data.TT$num <- 1:nrow(data.TT)",file="code.R",append=TRUE,sep="\n")			
    if(tktype!="rect" && tktype!="heatmap" && tktype!="ideogram"){
      coltypeTrack <- as.numeric(coltypeTk[i])
      cat("data.TTC <- NULL",paste("coltypeTrack <- ",coltypeTrack,sep=""),file="code.R",append=TRUE,sep="\n")					  
      if(coltypeTrack==2){
        tkcolor <- colorTrack[i]
        tkcolor <- gsub("\\s","",strsplit(tkcolor,",")[[1]])
        tkcolor <- gsub('\\"',"",tkcolor)
        tkcolor <- gsub("0x","#", tkcolor)
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")						
      }else if((coltypeTrack==3 && ("color" %in% colnames(data.TT))) | (coltypeTrack==3 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack")){
        tkcolor <- colorcusTrack[i]
        tkcolor <- unlist(strsplit(tkcolor,";"))
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")												
        tkcolor <- data.frame(id=tkcolor,stringsAsFactors=F)
        tkcolor$group <- gsub("\\:.*","",tkcolor$id)
        tkcolor$cols <- gsub(".*\\:","",tkcolor$id)
        tkcolor$group <- gsub(" ","",tkcolor$group)
        tkcolor$cols <- gsub(" ","",tkcolor$cols)
        colname <- colnames(data.TT)
        tkcolor <- unique(data.TTC.export[[i]]$cols)						
        data.TT <- data.TT[,1:4]						
        cat('tkcolor <- data.frame(id=tkcolor,stringsAsFactors=F)
		tkcolor$group <- gsub("\\\\:.*","",tkcolor$id)
		tkcolor$cols <- gsub(".*\\\\:","",tkcolor$id)
		tkcolor$group <- gsub(" ","",tkcolor$group)
		tkcolor$cols <- gsub(" ","",tkcolor$cols)',file="code.R",append=TRUE,sep="\n")
        cat('colname <- colnames(data.TT)
						if("color" %in% colnames(data.TT)){
			               data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
						}else if(colnames(data.TT)[4]=="stack"){
			               data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)						
						}
						data.TTC <- data.TTC[c(colname,"cols")]
						data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
						tkcolor <- unique(data.TTC$cols)
						data.TT <- data.TT[,1:4]',file="code.R",append=TRUE,sep="\n")
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")						
      }else if(coltypeTrack==1 && ("color" %in% colnames(data.TT))){
        tkcolor <- tkcolor.export[[i]]
        data.TT <- data.TT[,1:4]
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")									            					
        cat('tkcolor <- data.frame(group=unique(data.TT$color),cols=tkcolor,stringsAsFactors=F)',file="code.R",append=TRUE,sep="\n")
        cat('colname <- colnames(data.TT)
		data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
		data.TTC <- data.TTC[c(colname,"cols")]
		data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
		tkcolor <- unique(data.TTC$cols)
		data.TT <- data.TT[,1:4]',file="code.R",append=TRUE,sep="\n")						
      }else if(coltypeTrack==1 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack"){
        tkcolor <- tkcolor.export[[i]]  
        data.TT <- data.TT[,1:4]						
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")												
        cat('tkcolor <- data.frame(group=unique(data.TT$stack),cols=tkcolor,stringsAsFactors=F)
		colname <- colnames(data.TT)',file="code.R",append=TRUE,sep="\n")
        cat('data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)
		data.TTC <- data.TTC[c(colname,"cols")]
		data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
		tkcolor <- unique(data.TTC$cols)
		data.TT <- data.TT[,1:4]',file="code.R",append=TRUE,sep="\n")
      }else{
        tkcolor <- tkcolor.export[[i]]                      
        cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")												
      }
      if(nchar(data.TTC.export[[i]])>0){
        data.TTC <- data.TTC.export[[i]]
      }else{
        data.TTC <- NULL
      }					
      if(!is.null(data.TTC)){					
        cat('data.TTC <- data.TTC[order(data.TTC$num),]
		rownames(data.TTC) <- NULL
		data.TTC$num <- NULL',file="code.R",append=TRUE,sep="\n")
      }
      data.TT$num <- NULL
      cat('data.TT$num <- NULL',file="code.R",append=TRUE,sep="\n")
      if(ncol(data.TT)==5 && ("color" %in% colnames(data.TT))){
        data.TT <- data.TT[,1:4]
        cat('data.TT <- data.TT[,1:4]',file="code.R",append=TRUE,sep="\n")
      }else if(c(ncol(data.TT)==5 | ncol(data.TT)==6 | ncol(data.TT)==7) && ("pch" %in% colnames(data.TT)) && !("color" %in% colnames(data.TT))){
        data.TT <- data.TT[,1:4]
        tkcolor	<- tkcolor[1]
        cat('data.TT <- data.TT[,1:4]
		tkcolor <- tkcolor[1]',file="code.R",append=TRUE,sep="\n")
      }
    }
    ## *** The backgroud color for track ***
    tkbgcol <- bgcolTrack[i] 
    tkbgcol <- gsub("\\s","",strsplit(tkbgcol,",")[[1]])
    tkbgcol <- gsub('\\"',"",tkbgcol)
    tkbgcol <- gsub("0x","#", tkbgcol)        
    repnumcol <- round(length(unique(data.C[,1]))/length(tkbgcol))+1
    tkbgcol <- rep(tkbgcol, repnumcol)[1:length(unique(data.C[,1]))]
    cat(paste('tkbgcol <- c("',paste(tkbgcol,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")									
    ## *** The track margin ***
    tkmargin <- marginTrack[i]
    tkmargin <- as.numeric(tkmargin)
    cat(paste("tkmargin <- ",tkmargin,sep=""),file="code.R",append=TRUE,sep="\n")																	
    ## *** The track height ***
    tkheight <- heightTrack[i]
    tkheight <- as.numeric(tkheight)
    cat(paste("tkheight <- ",tkheight,sep=""),file="code.R",append=TRUE,sep="\n")																				
    ## *** The y coordinates of baselines ***
    tklinecoord <- baselineTrack[i]
    tklinecoord <- as.numeric(unlist(strsplit(tklinecoord,",")))
    cat(paste("tklinecoord <- c(",paste(tklinecoord,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")												
    ## *** The symbol type & point size***
    if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
      symboltype <- symbolTrack[i]
      symboltype <- as.numeric(unlist(strsplit(symboltype,",")))
      symboltype <- rep(symboltype, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]  
      pointsize <- as.numeric(pointsizeTrack[1])
      cat(paste("symboltype <- c(",paste(symboltype,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")						
      cat(paste("pointsize <- c(",pointsize,")",sep=""),file="code.R",append=TRUE,sep="\n")																	
    }
    ## *** The baselines color ***
    tklinecolor <- colorlineTrack[i]
    if(nchar(tklinecolor)!=0){
      tklinecolor <- gsub('\\"',"",tklinecolor)
      tklinecolor <- gsub("0x","#", tklinecolor)
      tklinecolor <- unlist(strsplit(tklinecolor,","))
      tklinecolor <- rep(tklinecolor, length(tklinecoord))[1:length(tklinecoord)]  
      cat(paste('tklinecolor <- c("',paste(tklinecolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")										
    }else{
      cat(paste('tklinecolor <- "',tklinecolor,'"',sep=""),file="code.R",append=TRUE,sep="\n")													
    }
    if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
      tklinecol <- gsub('\\"',"",tklinecolor)
      tklinecol <- gsub("0x","#", tklinecol)
      tklinecol <- unlist(strsplit(tklinecol,","))
      tklinecol <- rep(tklinecol, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]   
      cat(paste('tklinecol <- c("',paste(tklinecol,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")														
    }
    ## *** The fill color for track ***
	if(heatmapcol[i]==1){
      hmapcols <- gsub('\\"',"",colhmapTrack[i])    
	}else{
	  hmapcols <- heatmapcols[i]                                         
	}	   
    hmapcols <- unlist(strsplit(hmapcols,"\\."))  
    cat(paste('hmapcols <- c("',paste(hmapcols,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")														 			
    ## *** Add connection ***
    lineshmap <- lineshmapTrack[i]
    if(lineshmap==1){
      heightlines <- heightlinesTrack[i]			
      marginlines <- marginlinesTrack[i]
      cat(paste("heightlines <- ",heightlines,sep=""),paste("marginlines <- ",marginlines,sep=""),file="code.R",append=TRUE,sep="\n")																					
    }			
    ## *** Add border ***
    tkborder <- borderTrack[i]
    cat(paste('tkborder <- "',tkborder,'"',sep=""),file="code.R",append=TRUE,sep="\n")																								
    innergap <- 0.5 - innergapTrack[i]
    cat(paste("innergap <- ",innergap,sep=""),file="code.R",append=TRUE,sep="\n")
	gridsborder <- gridsborderTrack[i]
    if(gridsborder=="add"){
      tkbordercol <- colgridsborderTrack[i]
      cat(paste('tkbordercol <- c("',tkbordercol,'")',sep=""),file="code.R",append=TRUE,sep="\n")																												
      if(nchar(tkbordercol)==0){
        tkbordercol <- NA
        cat(paste("tkbordercol <- ",tkbordercol,sep=""),file="code.R",append=TRUE,sep="\n")																																
      }
    }else{
      tkbordercol <- NA
      cat(paste("tkbordercol <- ",tkbordercol,sep=""),file="code.R",append=TRUE,sep="\n")																																				
    }
    ## *** The bar direction ***
    tkbardir <- directionTrack[i]
    cat(paste("tkbardir <- ",tkbardir,sep=""),file="code.R",append=TRUE,sep="\n")																																			
    if(tkbardir==2){
      tkbarvalue <- barBoundary[i]
      tkbarcol1 <- coldir1Track[i]    
      tkbarcol2 <- coldir2Track[i]
      tktransparency <- transparencyTrack[i]
      tkbarcol1 <- adjustcolor(tkbarcol1, alpha.f = tktransparency)
      tkbarcol2 <- adjustcolor(tkbarcol2, alpha.f = tktransparency)
      cat(paste("tkbarvalue <- ",tkbarvalue,sep=""),paste("tktransparency <- ",tktransparency,sep=""),paste('tkbarcol1 <- "',tkbarcol1,'"',sep=""),paste('tkbarcol2 <- "',tkbarcol2,'"',sep=""),file="code.R",append=TRUE,sep="\n")																																								
    }
    ## *** The data color ***
    tkrectcol <- rectTrack[i]			  
    ## *** Select color ***
    selrectcol <- rectcolTrack[i]
    cat(paste("tkrectcol <- ",tkrectcol,sep=""),paste("selrectcol <- ",selrectcol,sep=""),file="code.R",append=TRUE,sep="\n")																																											
    if(tkrectcol==1){
      cat(paste('rectcols <- c("',paste(rectcols,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")																
    }else if(tkrectcol==2 && selrectcol==2){
      rectcols <- rectcoldisTrack[i]      
	  cat(paste('rectcols <- c("',rectcols,'")',sep=""),"data.TT[,4] <- rectcols",file="code.R",append=TRUE,sep="\n")																								
    }else if(tkrectcol==2 && selrectcol==3){
      rectcols <- rectcoldiscusTrack[i]
      rectcols <- unlist(strsplit(rectcols,";"))
      cat(paste('rectcols <- c("',paste(rectcols,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")																				
      cat('rectcols <- data.frame(id=rectcols,stringsAsFactors=F)
			    rectcols$group <- gsub("\\\\:.*","",rectcols$id)
                rectcols$cols <- gsub(".*\\\\:","",rectcols$id)
			    rectcols$group <- gsub(" ","",rectcols$group)
			    rectcols$cols <- gsub(" ","",rectcols$cols)
			    colname <- colnames(data.TT)[1:3]
			    data.TT <- merge(data.TT,rectcols,by.x=colnames(data.TT)[4],by.y="group",all.x=T)
			    data.TT <- data.TT[c(colname,"cols")]',file="code.R",append=TRUE,sep="\n")
    }
    ## *** The transparency of color ***
    tktransparency <- transparencyTrack[i]
    cat(paste("tktransparency <- ",tktransparency,sep=""),file="code.R",append=TRUE,sep="\n")																																							
    if((tktype!="rect" && tktype!="heatmap" && tktype!="ideogram") | (tktype=="line" && fillareaTrack[i]!="add")){
      tkcolor <- adjustcolor(tkcolor, alpha.f = tktransparency)
      cat(paste('tkcolor <- c("',paste(tkcolor,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")																
    }
    cat('data.TTT <- data.T[[i]]
	data.TTT$id <- paste(data.TTT[,1],data.TTT[,2],data.TTT[,3],sep="")
	data.TTT$num <- 1:nrow(data.TTT)',file="code.R",append=TRUE,sep="\n")
    cat(paste("transparencyHlt <- c(",paste(transparencyHlt,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																						
    ## *** The links margin ***
    if(i != length(data.T)){
      lkmargin <- 0
    }else{
      lkmargin <- marginLinks
    }
    cat(paste("lkmargin <- ",lkmargin,sep=""),file="code.R",append=TRUE,sep="\n")																			
    if(tkborder=="add"){
      tkborder <- "grey"
      cat(paste('tkborder <- "',tkborder,'"',sep=""),file="code.R",append=TRUE,sep="\n")																										
    }else{
      tkborder <- NA
      cat(paste("tkborder <- ",tkborder,sep=""),file="code.R",append=TRUE,sep="\n")																										
    }
    columns <- c(1:ncol(data.TT))[-c(1:3)]				
    cat(paste("columns <- c(",paste(columns,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
    if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1){
      cat("takindx <- takindx+2",file="code.R",append=TRUE,sep="\n")																										
    }
    if(poslabels[i]=="inner"){
      cat("takindx <- takindx-2",file="code.R",append=TRUE,sep="\n")																														
    }
    if(tktype=="line"){
      cat(paste("selreaTrack <- c(",paste(selreaTrack,collapse =","),")",sep=""),paste('fillareaTrack="',fillareaTrack,'"',sep=""),file="code.R",append=TRUE,sep="\n")																						
      ## *** Fill the area ***
      if(fillareaTrack[i]!="add"){
        area <- FALSE
        borderset <- NA
        lwdnum <- 1
        cat(paste("area <- ",area,sep=""),paste("borderset <- ",borderset,sep=""),paste("lwdnum <- ",lwdnum,sep=""),file="code.R",append=TRUE,sep="\n")																																			
      }else if(fillareaTrack[i]=="add" && selreaTrack[i]==1){
        area <- TRUE
        lwdnum <- 0.2
        cat(paste("area <- ",area,sep=""),paste("lwdnum <- ",lwdnum,sep=""),file="code.R",append=TRUE,sep="\n")																																								
      }else if(fillareaTrack[i]=="add" && selreaTrack[i]==2){				
        area <- TRUE
        borderset <- NA
        if(nchar(borderareaTrack[i])!=0){					
          borderset <- adjustcolor(borderareaTrack[i],alpha.f = tktransparency)
        }
        lwdnum <- 0.2
        cat(paste("area <- ",area,sep=""),paste("borderset <- ",borderset,sep=""),paste("lwdnum <- ",lwdnum,sep=""),file="code.R",append=TRUE,sep="\n")																																								
      }					
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")
      }
      if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
        cat('bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
			    if(coltypeTrack==2){
			        data.TT[data.TT[,4] %in% x,1:3]
			    }else{
			        data.TTC[data.TTC[,4] %in% x,1:3]
			    }
			    })',file="code.R",append=TRUE,sep="\n")				
        cat('circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
                 bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region, value, ...){
                 i = getI(...)			
			if(coltypeTrack==1){
                circos.genomicLines(region, value, col=tkcolor[i], lty=1, ...)
			}else if(coltypeTrack==2){
                circos.genomicLines(region, value, col=tkcolor[1], lty=1, ...)
			}else if(coltypeTrack==3){
                circos.genomicLines(region, value, col=tkcolor[i], lty=1, ...)
			}
			})',file="code.R",append=TRUE,sep="\n")
      }else{
        cat('data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])			
			    circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor[1])!=0){               
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
								for(k in 1:length(tklinecoord)){
                                y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					            }
				              }
							  if((coltypeTrack==1 && !("color" %in% colnames(data.T[[i]]))) | coltypeTrack==2){
							    if(length(columns)==1){
							       tkcolor <- tkcolor[1]
							    }else{
								   tkcolor <- c(tkcolor,rep("grey",length(columns)))
								   tkcolor <- tkcolor[1:length(columns)]
								}								
								if(selreaTrack[i]==1 | fillareaTrack[i]!="add"){
								   borderset <- adjustcolor(tkcolor,alpha.f = tktransparency)
								}
								circos.genomicLines(region, value, numeric.column=columns-3, col=borderset, area=area, border=tkcolor, lwd=lwdnum, lty=1, ...)
							  }
                              })',file="code.R",append=TRUE,sep="\n")				  
        if(coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){			    
          cat('data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
                data.TTC$num <- 1:nrow(data.TTC)				
				lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				    circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				    circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
				if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
			    }				
                dat <- data.TTC[data.TTC[,1] %in% x,]
				lapply(unique(dat$cols),function(m){				     
				     datt <- dat[dat$cols %in% m,]					 
                     ind <- which(data.TTC$id %in% datt$id)
                     datt.fil <- na.omit(unique(data.TTC[ind,]))
                     datt.fil <- datt.fil[datt.fil[,1] %in% x,]
				     rownum <- datt.fil$num
				     rownumdif <- diff(rownum)
				     indx <- which(rownumdif != 1)
				     indx1 <- c(0,indx)
                     rownumdif1 <- c(1,rownumdif)
				     if(length(indx)==0){
				         rownumdif1 <- 1				  
				     }else{
				         for(k in 1:length(which(rownumdif!=1))){
                             rownumdif1[(indx1[k]+1):indx[k]] <- k
                             if(k==length(which(rownumdif!=1))){
                                 rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                             }
                         }
				     }
				     datt.fil$indx <- rownumdif1
			  	     lapply(unique(rownumdif1),function(h){
				         datt.fill <- datt.fil[datt.fil$indx == h,]
				         minnum <- min(datt.fill$num)
				         datt.fill <- rbind(datt.fill,as.character(c(dat[dat$num==(minnum-1),],h)))
				         datt.fill[,2] <- as.numeric(datt.fill[,2])
				         datt.fill[,3] <- as.numeric(datt.fill[,3])
				         datt.fill[,4] <- as.numeric(datt.fill[,4])
				         datt.fill$indx <- as.numeric(datt.fill$indx)
					     datt.fill <- datt.fill[!is.na(datt.fill[,2]),]
				         datt.fill <- datt.fill[order(datt.fill[,2],datt.fill[,3]),]				 
						 if(selreaTrack[i]==1 | fillareaTrack[i]!="add"){
							borderset <- adjustcolor(m,alpha.f = tktransparency)
						 }						 
						 circos.lines((datt.fill[,2]+datt.fill[,3])/2,datt.fill[,4], col=borderset, area=area, border=m, lwd=lwdnum, lty=1)
				     })					 
                })
				})',file="code.R",append=TRUE,sep="\n")												
        }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && ("cols" %in% colnames(data.TTC))){	
          cat('data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
                data.TTC$num <- 1:nrow(data.TTC)	
				lapply(unique(data.TTC[,1]),function(x){
                    if(trackChr=="track"){
				        circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				    }else{
				        circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				    }
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                        y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                        circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				}			
                dat <- data.TTC[data.TTC[,1] %in% x,]
				lapply(unique(dat$cols),function(m){
				     datt <- dat[dat$cols %in% m,]					 
                     ind <- which(data.TTC$id %in% datt$id)
                     datt.fil <- na.omit(unique(data.TTC[ind,]))
                     datt.fil <- datt.fil[datt.fil[,1] %in% x,]
				     rownum <- datt.fil$num
				     rownumdif <- diff(rownum)
				     indx <- which(rownumdif != 1)
				     indx1 <- c(0,indx)
                     rownumdif1 <- c(1,rownumdif)
				     if(length(indx)==0){
				         rownumdif1 <- 1				  
				     }else{
				         for(k in 1:length(which(rownumdif!=1))){
                             rownumdif1[(indx1[k]+1):indx[k]] <- k
                             if(k==length(which(rownumdif!=1))){
                                 rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                             }
                         }
				     }
				     datt.fil$indx <- rownumdif1
			  	     lapply(unique(rownumdif1),function(h){
				         datt.fill <- datt.fil[datt.fil$indx == h,]
				         minnum <- min(datt.fill$num)
				         datt.fill <- rbind(datt.fill,as.character(c(dat[dat$num==(minnum-1),],h)))
				         datt.fill[,2] <- as.numeric(datt.fill[,2])
				         datt.fill[,3] <- as.numeric(datt.fill[,3])
				         datt.fill[,4] <- as.numeric(datt.fill[,4])
				         datt.fill$indx <- as.numeric(datt.fill$indx)
					     datt.fill <- datt.fill[!is.na(datt.fill[,2]),]
				         datt.fill <- datt.fill[order(datt.fill[,2],datt.fill[,3]),]
						 if(selreaTrack[i]==1 | fillareaTrack[i]!="add"){
							borderset <- adjustcolor(m,alpha.f = tktransparency)
						 }	
                         circos.lines((datt.fill[,2]+datt.fill[,3])/2,datt.fill[,4], col=borderset, area=area, border=m, lwd=lwdnum, lty=1)								  				  						 
				     })				 
				})
                })',file="code.R",append=TRUE,sep="\n")				
        }
        assign("hltTrack",hltTrack.List[[i]])
        assign("hltdata",hltdata.List[[i]])
        if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && (length(columns)==1 | colnames(data.T[[i]])[5]=="color")){
          cat('assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				      datt <- hltregion[hltregion[,1] %in% x,]
                      trackk <- data.TTT[data.TTT[,1] %in% x,]
                      trackk <- trackk[!trackk$id %in% datt$id,]
                      col <- unique(datt$color)
				  if(trackChr=="track"){
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if(nchar(tklinecolor[1])!=0){               
					  xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
					  for(k in 1:length(tklinecoord)){
                          y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                          circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					  }
				  }				  
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ind <- which(data.TTT$id %in% dattt$id)
					   ind <- unique(c(ind-1,ind,ind+1))					   
                       dattt.fil <- na.omit(unique(data.TTT[ind,]))
                       dattt.fil <- dattt.fil[dattt.fil[,1] %in% x,]
                       dattt.fil <- dattt.fil[order(dattt.fil$num),]
                       dattt.fil$groups <- c(diff(dattt.fil$num),diff(dattt.fil$num)[1])
                       dattt.fill <- dattt.fil[dattt.fil$groups==1,]
						 borderset <- m
					   circos.lines((dattt.fill[,2]+dattt.fill[,3])/2,dattt.fill[,4], col=borderset, area=area, border=m, lwd=lwdnum, lty=1)
                  })
				  if("color" %in% colnames(trackk)){
				      data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
				      data.TTC1 <- data.TTC[!data.TTC$id %in% datt$id,]
                      indd <- which(data.TTC1$id %in% trackk$id)
				      indd <- unique(c(indd-1,indd,indd+1))
				      trackkk <- na.omit(unique(data.TTC1[indd,]))
				      trackkk <- trackkk[trackkk[,1] %in% x,]
				      lapply(unique(trackkk$cols),function(f){
				          trackkkk <- trackkk[trackkk$cols %in% f,]
				          rownum <- as.numeric(rownames(trackkkk))
				          rownumdif <- diff(rownum)
				          indx <- which(rownumdif != 1)
				          indx1 <- c(0,indx)
                          rownumdif1 <- c(1,rownumdif)
				          if(length(indx)==0){
				              rownumdif1 <- 1				  
				          }else{
				              for(k in 1:length(which(rownumdif!=1))){
                                  rownumdif1[(indx1[k]+1):indx[k]] <- k
                                  if(k==length(which(rownumdif!=1))){
                                      rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                                  }
                              }
				          }
				  trackkkk$indx <- rownumdif1
				  lapply(unique(rownumdif1),function(h){
				      trackkkkk <- trackkkk[trackkkk$indx == h,]
				      minnum <- min(as.numeric(rownames(trackkkkk)))
				      trackkkkk <- rbind(trackkkkk,as.character(c(trackkk[rownames(trackkk)==(minnum-1),]),h))
				      trackkkkk[,2] <- as.numeric(trackkkkk[,2])
				      trackkkkk[,3] <- as.numeric(trackkkkk[,3])
				      trackkkkk[,4] <- as.numeric(trackkkkk[,4])
				      trackkkkk$indx <- as.numeric(trackkkkk$indx)
				      trackkkkk <- trackkkkk[!is.na(trackkkkk[,2]),]
				      trackkkkk <- trackkkkk[order(trackkkkk[,2],trackkkkk[,3]),]
					  if(selreaTrack[i]==1 | fillareaTrack[i]!="add"){
						 borderset <- adjustcolor(f,alpha.f = tktransparency)
					  }					  
					  circos.lines((trackkkkk[,2]+trackkkkk[,3])/2,trackkkkk[,4], col=borderset, area=area, border=f, lwd=lwdnum, lty=1)
				  })
				  })
				  }else{
				      rownum <- as.numeric(rownames(trackk))
				      rownumdif <- diff(rownum)
				      indx <- which(rownumdif != 1)
				      indx1 <- c(0,indx)
                      rownumdif1 <- c(1,rownumdif)
				      if(length(indx)==0){
				          rownumdif1 <- 1				 
				      }else{
				          for(k in 1:length(which(rownumdif!=1))){
                              rownumdif1[(indx1[k]+1):indx[k]] <- k
                              if(k==length(which(rownumdif!=1))){
                                  rownumdif1[(indx[k]+1):length(rownumdif1)] <- k+1
                              }
                          }
				      }    
				  trackk$indx <- rownumdif1
				  lapply(unique(rownumdif1),function(h){
				      trackkkkk <- trackk[trackk$indx == h,]
					  if(selreaTrack[i]==1 | fillareaTrack[i]!="add"){
						 borderset <- adjustcolor(tkcolor,alpha.f = tktransparency)
					  }					  
					  circos.lines((trackkkkk[,2]+trackkkkk[,3])/2,trackkkkk[,4], col=borderset[1], area=area, border=tkcolor[1], lwd=lwdnum, lty=1)
				  })				  
				  }				  
            })',file="code.R",append=TRUE,sep="\n")               		
        }
      }
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")
      }	
    }else if(tktype=="point"){			
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")					   
      }
      if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
        cat('bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
			    if(coltypeTrack==2){
			        data.TT[data.TT[,4] %in% x,1:3]
			    }else{
			        data.TTC[data.TTC[,4] %in% x,1:3]
			    }
			    })',file="code.R",append=TRUE,sep="\n")				
        cat('circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
                 bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region, value, ...){
                 i = getI(...)			
			if(coltypeTrack==1){
                 circos.lines(CELL_META$cell.xlim, c(i, i), lty = 2, col = tklinecol[i])			
                 circos.genomicPoints(region, value, pch = symboltype[i], cex = pointsize, col = tkcolor[i],...)
			}else if(coltypeTrack==2){
                 circos.lines(CELL_META$cell.xlim, c(i, i), lty = 2, col = tklinecol[i])				
                 circos.genomicPoints(region, value, pch = symboltype[i], cex = pointsize, col = tkcolor[1],...)		
			}else if(coltypeTrack==3){
                 circos.lines(CELL_META$cell.xlim, c(i, i), lty = 2, col = tklinecol[i])                 
				 circos.genomicPoints(region, value, pch = symboltype[i], cex = pointsize, col = tkcolor[i],...)			
			}
			})',file="code.R",append=TRUE,sep="\n")
      }else{
        cat('data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor[1])!=0){               
					            xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
					            for(k in 1:length(tklinecoord)){
                                y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					            }
				              }
							  if(!("cex" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.T[[i]])) && ((coltypeTrack==1 && !("color" %in% colnames(data.T[[i]]))) | coltypeTrack==2)){
							    if(length(columns)==1){
							       tkcolor <- tkcolor[1]
							    }else{
								   tkcolor <- c(tkcolor,rep("grey",length(columns)))
								   tkcolor <- tkcolor[1:length(columns)]
								}
							  circos.genomicPoints(region, value, numeric.column=columns-3, col=tkcolor, cex=0.6, pch=16, ...)
							  }
                             })',file="code.R",append=TRUE,sep="\n")
        if(!("cex" %in% colnames(data.T[[i]]))){
          if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){  			
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=dat$pch)  
                })',file="code.R",append=TRUE,sep="\n")			
          }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){   			
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                })',file="code.R",append=TRUE,sep="\n")
          }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
            cat('lapply(unique(data.T[[i]][,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
			    if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
				tkcols <- data.TTC$cols[data.TTC[,1] %in% x]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(tkcols,alpha.f = tktransparency), cex=0.6, pch=dat$pch)
                })',file="code.R",append=TRUE,sep="\n")				
          }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
            cat('lapply(unique(data.T[[i]][,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
			    if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor, cex=0.6, pch=dat$pch)
                })',file="code.R",append=TRUE,sep="\n")			
          }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                })',file="code.R",append=TRUE,sep="\n")			
          }
        }else if("cex" %in% colnames(data.T[[i]])){
          if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){  			
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=dat$pch)  
                })',file="code.R",append=TRUE,sep="\n")			
          }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){   			
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                })',file="code.R",append=TRUE,sep="\n")
          }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
            cat('lapply(unique(data.T[[i]][,1]),function(x){
                if(trackChr=="track"){
				    circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				    circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
			    if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
				tkcols <- data.TTC$cols[data.TTC[,1] %in% x]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(tkcols,alpha.f = tktransparency), cex=dat$cex, pch=dat$pch)
                })',file="code.R",append=TRUE,sep="\n")				
          }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
            cat('lapply(unique(data.T[[i]][,1]),function(x){
                if(trackChr=="track"){
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
			    if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor, cex=dat$cex, pch=dat$pch)
                })',file="code.R",append=TRUE,sep="\n")			
          }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
            cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                })',file="code.R",append=TRUE,sep="\n")			
          }else if(!("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC))){
            cat('lapply(unique(data.T[[i]][,1]),function(x){
                if(trackChr=="track"){
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				  circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
			    if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				} 				
                dat <- data.T[[i]][data.T[[i]][,1] %in% x,]
                circos.points((dat[,2]+dat[,3])/2,dat[,4], col=tkcolor[1], cex=dat$cex, pch=16)
                })',file="code.R",append=TRUE,sep="\n")
          }
        }				
        assign("hltTrack",hltTrack.List[[i]])
        assign("hltdata",hltdata.List[[i]])
        if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && (length(columns)==1 | any(c("color","pch","cex") %in% colnames(data.T[[i]])))){
          cat('assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if(trackChr=="track"){
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				  }
                  if(!"pch" %in% colnames(data.TTT)){                  
                     dattt_pch <- 16
                     trackk_pch <- 16
                  }else{
                     trackk_pch <- trackk$pch
                  }
				  if(!"cex" %in% colnames(data.TTT)){                  
                     dattt_cex <- 0.6
					 trackk_cex <- 0.6
                  }else{
				     trackk_cex <- trackk$cex
				  }
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
					   if("pch" %in% colnames(data.TTT)){
                          dattt_pch <- dattt$pch
                       }
					   if("cex" %in% colnames(data.TTT)){
                          dattt_cex <- dattt$cex
                       }					   
                       circos.points((dattt[,2]+dattt[,3])/2,dattt[,4], col=m, cex=dattt_cex, pch=dattt_pch)
                  })
				  if("color" %in% colnames(trackk) && coltypeTrack!=2){
				     data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")			  
				     trackkk <- data.TTC[data.TTC$id %in% trackk$id,]
				     trackkk <- trackkk[trackkk[,1] %in% x,]
				     lapply(unique(trackkk$cols),function(f){
				        trackkkk <- trackkk[trackkk$cols %in% f,]
				        if("cex" %in% colnames(trackkkk)){
				           trackk_cex <- trackkkk$cex
				        }
				        if("pch" %in% colnames(trackkkk)){
				           trackk_pch <- trackkkk$pch
				        }
                        circos.points((trackkkk[,2]+trackkkk[,3])/2,trackkkk[,4], col=adjustcolor(f,alpha.f = tktransparency), cex=trackk_cex, pch=trackk_pch)				  
				     })
				  }else{
                     circos.points((trackk[,2]+trackk[,3])/2,trackk[,4], col=tkcolor[1], cex=trackk_cex, pch=trackk_pch)
				  }
                  })',file="code.R",append=TRUE,sep="\n") 				  
        }
      }
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")			   
      }
    }else if(tktype=="bar"){
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")
      }
      cat('data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
			        if(!("color" %in% colnames(data.T[[i]])) && !("cols" %in% colnames(data.TTC))){
			        if(length(columns)==1 && tkbardir==1){
					if(nchar(tklinecolor[1])!=0){               
					  xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
					  for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					  }
				    }
					if(coltypeTrack==1){
                       circos.genomicRect(region, value, numeric.column=columns-3, ytop.column = 1, ybottom = min(data.TT[,4]), col=tkcolor, border = NA, ...)
					}
                    }else if(length(columns)==1 && tkbardir==2){
					if(nchar(tklinecolor[1])!=0){               
					  xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
					  for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					  }
				    }
				    tkbarvalue <- as.numeric(tkbarvalue)
					indx <- value[,1] > tkbarvalue
                    if(length(value[indx,])!=0 && length(value[!indx,])!=0){
                         circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                         circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }else if(length(value[indx,])!=0 && length(value[!indx,])==0){
                         circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                    }else if(length(value[indx,])==0 && length(value[!indx,])!=0){
                         circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }
					}else if(length(columns)==2 && tkbardir==1){
					if(nchar(tklinecolor[1])!=0){               
					   xlim <- get.cell.meta.data("xlim")
                       ylim <- get.cell.meta.data("ylim")
					   for(k in 1:length(tklinecoord)){
                       y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                       circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					   }
				    }
				  if(coltypeTrack!=3){					
					    tkcolor <- c(tkcolor,rep("grey",length(columns)))
					    tkcolor <- tkcolor[1:length(columns)]				
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[1], border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[2], border = NA, ...)
                  }						
					}else if(length(columns)==2 && tkbardir==2){
					if(nchar(tklinecolor[1])!=0){               
					  xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
					  for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					  }
				    }
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol1, border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol2, border = NA, ...)						
					}
					}
			        if(length(columns)==1 && tkbardir==1 && coltypeTrack==2){
					if(nchar(tklinecolor[1])!=0){               
					  xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
					  for(k in 1:length(tklinecoord)){
                      y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					  }
				    }
                      circos.genomicRect(region, value, numeric.column=columns-3, ytop.column = 1, ybottom = min(data.TT[,4]), col=tkcolor[1], border = NA, ...)
                    }	
					})',file="code.R",append=TRUE,sep="\n")			
      if(length(columns)==1 && tkbardir==1 && coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){		
        cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				}					
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)                
                })',file="code.R",append=TRUE,sep="\n")
      }else if(length(columns)==1 && tkbardir==1 && coltypeTrack==1 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){		
        cat('lapply(unique(data.TTC[,1]),function(x){
                if(trackChr=="track"){
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}else{
				   circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				}
                if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				}					
                dat <- data.TTC[data.TTC[,1] %in% x,]
                circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)                
                })',file="code.R",append=TRUE,sep="\n")
      }					
      assign("hltTrack",hltTrack.List[[i]])
      assign("hltdata",hltdata.List[[i]])
      if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && tkbardir==1 && length(columns)==1){
        cat('assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if(trackChr=="track"){
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
                       circos.rect(xleft=dattt[,2], xright=dattt[,3],ytop=dattt[,4],ybottom=rep(ylim[1],nrow(dattt)), col=m, border = NA)
                  })
                  circos.rect(xleft=trackk[,2], xright=trackk[,3],ytop=trackk[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(trackk)), col=tkcolor[1], border = NA)
                  })',file="code.R",append=TRUE,sep="\n")          
      }else if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && tkbardir==2 && length(columns)==1){
        cat('assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if(trackChr=="track"){
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=takindx+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if(nchar(tklinecolor[1])!=0){               
					xlim <- get.cell.meta.data("xlim")
                    ylim <- get.cell.meta.data("ylim")
					for(k in 1:length(tklinecoord)){
                    y1 <- as.numeric(quantile(ylim,probs=tklinecoord[k]))
                    circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor[k], lwd=0.1)
					}
				  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
					   tkbarvalue <- as.numeric(tkbarvalue)
				       indx <- dattt[,4] > tkbarvalue
				       value <- dattt[,4]
				       region <- dattt[,c(1:3)]
					   if(length(value[indx])!=0 && length(value[!indx])!=0){
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                       }else if(length(value[indx])!=0 && length(value[!indx])==0){
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                       }else if(length(value[indx])==0 && length(value[!indx])!=0){
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                      } 
                  })
				   tkbarvalue <- as.numeric(tkbarvalue)
				   indx <- trackk[,4] > tkbarvalue
				   value <- trackk[,4]
				   region <- trackk[,c(1:3)]
                   if(length(value[indx])!=0 && length(value[!indx])!=0){
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                   }else if(length(value[indx])!=0 && length(value[!indx])==0){
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                   }else if(length(value[indx])==0 && length(value[!indx])!=0){
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                   }
                  })',file="code.R",append=TRUE,sep="\n")
      }
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")			   
      }
    }else if(tktype=="rect"){
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")
      }								
      if(tkrectcol==2){
        if(selrectcol==1){
          cat("data.TT[,4] <- as.numeric(as.factor(data.TT[,4]))   
			dat.T <- data.TT",file="code.R",append=TRUE,sep="\n")
          
          selcol <- tkcolor.export[[i]]
          cat(paste('selcol <- c("',paste(selcol,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")				
          cat("data.TT[,4] <- selcol[data.TT[,4]]
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)	 
            })",file="code.R",append=TRUE,sep="\n")
        }else{
          cat("circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)	  
            })",file="code.R",append=TRUE,sep="\n")
        }
      }else{
        cat("dat.T <- data.TT 
			f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(f(value[[1]]),alpha.f = tktransparency), border = NA, ...)	 
			})",file="code.R",append=TRUE,sep="\n")
      }
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")
      }
    }else if(tktype=="heatmap"){
      cat("data.TT$num <- NULL",file="code.R",append=TRUE,sep="\n")
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")			   
      }
      cat("break1 <- min(as.numeric(as.matrix(data.TT[,-c(1:3)])))
			break2 <- max(as.numeric(as.matrix(data.TT[,-c(1:3)])))
			midpoint <- (break1+break2)/2
			f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = hmapcols)",file="code.R",append=TRUE,sep="\n")
      if(is.na(tkbordercol)){
        if(lineshmap==2){
          cat('circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                              panel.fun = function(region, value, ...){
							    i = getI(...)
                                circos.genomicRect(region, value, col = f(value[[1]]), ybottom = i - innergap, ytop = i + innergap,
                                                   border = f(value[[1]]), posTransform = posTransform.default, ...)
                              }, bg.border = NA)',file="code.R",append=TRUE,sep="\n")
        }else{
          cat('circos.genomicPosTransformLines(data.TT, posTransform = posTransform.default,
                                horizontalLine = "top", track.height = heightlines, track.margin = c(0,marginlines))
            circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                              panel.fun = function(region, value, ...){
                                i = getI(...)
								circos.genomicRect(region, value, col = f(value[[1]]), ybottom = i - innergap, ytop = i + innergap,
                                                   border = f(value[[1]]), posTransform = posTransform.default, ...)
                              }, bg.border = NA)',file="code.R",append=TRUE,sep="\n")			
        }			
      }else{
        if(lineshmap==2){
          cat('circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                              panel.fun = function(region, value, ...){
                                i = getI(...)
								circos.genomicRect(region, value, col = f(value[[1]]), lwd = 0.1, ybottom = i - innergap, ytop = i + innergap,
                                                   border = tkbordercol, posTransform = posTransform.default, ...)
                              }, bg.border = NA)',file="code.R",append=TRUE,sep="\n")
        }else{
          cat('circos.genomicPosTransformLines(data.TT, posTransform = posTransform.default,
                                horizontalLine = "top", track.height = heightlines, track.margin = c(0,marginlines))
            circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                              panel.fun = function(region, value, ...){
                                i = getI(...)
								circos.genomicRect(region, value, col = f(value[[1]]), lwd = 0.1, ybottom = i - innergap, ytop = i + innergap,
                                                   border = tkbordercol, posTransform = posTransform.default, ...)
                              }, bg.border = NA)',file="code.R",append=TRUE,sep="\n")			
        }
      }
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")			   
      }			
    }else if(tktype=="ideogram"){
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")',file="code.R",append=TRUE,sep="\n")			   
      }
      cat('circos.genomicIdeogram(data.TT,track.height = tkheight, track.margin = c(lkmargin,tkmargin))',file="code.R",append=TRUE,sep="\n")
      if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
        cat(paste("heightlabels <- c(",paste(heightlabels,collapse =","),")",sep=""),paste("marginlabels <- c(",paste(marginlabels,collapse =","),")",sep=""),file="code.R",append=TRUE,sep="\n")																			
        cat('circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")',file="code.R",append=TRUE,sep="\n")			   
      }
    }
    cat(paste('poslabels <- c("',paste(poslabels,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")																			
    cat('if(poslabels[i]=="inner"){
			    takindx <- takindx+3
			}else{
			    takindx <- takindx+1
			}',file="code.R",append=TRUE,sep="\n") 
  }
}

if(!is.null(data.L) && linksTrack.export){	
  if(is.null(data.T)){
    cat(paste("marginLinks <- ",marginLinks,sep=""),file="code.R",append=TRUE,sep="\n")
    cat("circos.par(track.margin = c(0,marginLinks))",file="code.R",append=TRUE,sep="\n")
  }	
  cat(paste("transparencyLinks <- ",transparencyLinks,sep=""),file="code.R",append=TRUE,sep="\n")			
  cat("rou <- get_most_inside_radius()
			rou <- rou[1]",file="code.R",append=TRUE,sep="\n")
  if(colformatLinks!=3){  
  if(colorLinks==2){
    splitcol <- ":" %in% unlist(strsplit(selcolorLinks,""))
    if(ncol(data.L)==7 && colnames(data.L)[7]=="color" && splitcol){
      selcolorLinks <- unlist(strsplit(selcolorLinks,";"))
      cat(paste('selcolorLinks <- c("',paste(selcolorLinks,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")
      cat('data.L$num <- 1:nrow(data.L)
				   selcolorLinks <- data.frame(id=selcolorLinks,stringsAsFactors=F)
				   selcolorLinks$group <- gsub("\\\\:.*","",selcolorLinks$id)
                   selcolorLinks$cols <- gsub(".*\\\\:","",selcolorLinks$id)
				   selcolorLinks$group <- gsub(" ","",selcolorLinks$group)
				   selcolorLinks$cols <- gsub(" ","",selcolorLinks$cols)
				   data.LC <- merge(data.L,selcolorLinks,by.x="color",by.y="group",all.x=T)
				   data.LC$cols[is.na(data.LC$cols)] <- "grey"
				   data.LC <- data.LC[order(data.LC$num),]
				   data.LC$num <- NULL
				   rownames(data.LC) <- NULL
				   data.L$num <- NULL
			       colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)',file="code.R",append=TRUE,sep="\n")
    }else if(ncol(data.L)==6 && !splitcol){	
      cat("colLinks <- adjustcolor(selcolorLinks, alpha.f = transparencyLinks)",file="code.R",append=TRUE,sep="\n")
    }
    if(highlightLinks==1 && (!is.null(hltregion1.export) | !is.null(hltregion2.export))){ 
      cat(paste("transparencyhltLinks <- ",transparencyhltLinks,sep=""),file="code.R",append=TRUE,sep="\n")										
      cat('colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
					colindx <- (!data.LL1$num %in% c(hltregion1$num,hltregion2$num)) & (!data.LL2$num %in% c(hltregion1$num,hltregion2$num))',file="code.R",append=TRUE,sep="\n")
      if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks[colindx], border = NA)",file="code.R",append=TRUE,sep="\n")
      }else{
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks, border = NA)",file="code.R",append=TRUE,sep="\n")
      }
      cat("lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if(nrow(hltregion11)!=0){
                            circos.genomicLink(hltregion11, hltregion12, rou = rou, col = x, border = NA)
                        }
                        if(nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, rou = rou, col = x, border = NA)
                        }
				    })",file="code.R",append=TRUE,sep="\n")
    }else{
      if(!(ncol(data.L)==6 && splitcol)){		            
        
        cat("data.L1 <- data.L[,c(1:3)]
				   data.L2 <- data.L[,c(4:6)]
			       circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA)",file="code.R",append=TRUE,sep="\n")
      }
    }
  }else{
    if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
      cat(paste('linkscolor.export <- c("',paste(linkscolor.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")
      cat('groupnum <- length(unique(data.L[,7]))
				   randcolorLinks <- data.frame(group=unique(data.L[,7]), cols=linkscolor.export, stringsAsFactors=F)
				   data.LC <- merge(data.L,randcolorLinks,by.x="color",by.y="group",all.x=T)	
			       colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)',file="code.R",append=TRUE,sep="\n")
    }	
    if(highlightLinks==1 && (!is.null(hltregion1.export) | !is.null(hltregion2.export))){ 
      cat(paste("transparencyhltLinks <- ",transparencyhltLinks,sep=""),file="code.R",append=TRUE,sep="\n")										
      cat('colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
					colindx <- (!data.LL1$num %in% c(hltregion1$num,hltregion2$num)) & (!data.LL2$num %in% c(hltregion1$num,hltregion2$num))',file="code.R",append=TRUE,sep="\n")
      if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks[colindx], border = NA)",file="code.R",append=TRUE,sep="\n")
      }else{
        cat(paste('linkscolor.export <- c("',paste(linkscolor.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")					
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = linkscolor.export, border = NA)",file="code.R",append=TRUE,sep="\n")					
      }
      cat("lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if(nrow(hltregion11)!=0){
                            circos.genomicLink(hltregion11, hltregion12, rou = rou, col = x, border = NA)
                        }
                        if(nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, rou = rou, col = x, border = NA)
                        }
				    })",file="code.R",append=TRUE,sep="\n")
    }else{
      cat("data.L1 <- data.L1[,c(1:3)]
				   data.L2 <- data.L2[,c(1:3)]",file="code.R",append=TRUE,sep="\n")
      if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
        cat("circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA)",file="code.R",append=TRUE,sep="\n")
      }else{
        cat(paste('linkscolor.export <- c("',paste(linkscolor.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")					
        cat("circos.genomicLink(data.L1, data.L2, rou = rou, col = linkscolor.export, border = NA)",file="code.R",append=TRUE,sep="\n")
      }
    }
  }
	}else{
        if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
            cat("break1 <- min(as.numeric(as.matrix(data.L[,-c(1:6)])))
            break2 <- max(as.numeric(as.matrix(data.L[,-c(1:6)])))
            midpoint <- (break1+break2)/2
            f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = gracolinks)
			colLinks <- f(data.L$color)",file="code.R",append=TRUE,sep="\n")
        }else if((ncol(data.L)==6 | ncol(data.L)==7)){
            cat(paste('colLinks <- c("',paste(linkscolor.export,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")							  
        }
      if(highlightLinks==1 && (!is.null(hltregion1.export) | !is.null(hltregion2.export))){          
      cat(paste("transparencyhltLinks <- ",transparencyhltLinks,sep=""),file="code.R",append=TRUE,sep="\n")										
      cat('colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
					colindx <- (!data.LL1$num %in% c(hltregion1$num,hltregion2$num)) & (!data.LL2$num %in% c(hltregion1$num,hltregion2$num))',file="code.R",append=TRUE,sep="\n")
      if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks[colindx], border = NA)",file="code.R",append=TRUE,sep="\n")
      }else{
        cat("circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks, border = NA)",file="code.R",append=TRUE,sep="\n")
      }
      cat("lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if(nrow(hltregion11)!=0){
                            circos.genomicLink(hltregion11, hltregion12, rou = rou, col = x, border = NA)
                        }
                        if(nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, rou = rou, col = x, border = NA)
                        }
				    })",file="code.R",append=TRUE,sep="\n")
          }else{
            if(!(ncol(data.L)==6)){		  
                cat("data.L1 <- data.L[,c(1:3)]
				   data.L2 <- data.L[,c(4:6)]
			       circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA)",file="code.R",append=TRUE,sep="\n")
            }
          }
		}  
}
n <- length(legendtext)
if(n!=0 && addlegend==1){
  if(poslegend==1 && fontSize!="custom"){
    xleft <- 1.2+(as.numeric(fontSize)-1)*0.25
  }else if(poslegend==1 && fontSize=="custom"){
    xleft <- 1.2+(as.numeric(cexlabel)-1)*0.25
  }else{
    xleft <- -0.01
  }
  xright <- xleft+0.02 
  ybottom <- -0.13*0.22/10-n*0.03
  ytop <- -0.13*0.22/10+n*0.03		
  len <- ytop-ybottom
  gap <- len/(n-0.8)
  cat(paste("n <- ",n,sep=""),paste("xleft <- ",xleft,sep=""),paste("xright <- ",xright,sep=""),paste("ybottom <- ",ybottom,sep=""),paste("ytop <- ",ytop,sep=""),paste("len <- ",len,sep=""),paste("gap <- ",gap,sep=""),file="code.R",append=TRUE,sep="\n")
  cat(paste('legendtext <- c("',paste(legendtext,collapse ='","'),'")',sep=""),file="code.R",append=TRUE,sep="\n")					
  cat('for(i in 1:n){
			   assign(paste("n",i,sep=""),legendtext[i])
			}',file="code.R",append=TRUE,sep="\n")		
  cat('rect(xleft, ybottom, xright, ytop, col = "black")
            polygon(x=c(xleft-0.01,(xleft+xright)/2,xright+0.01), y=c(ybottom,ybottom-0.02,ybottom), col="black")
            text(x=xleft-0.08, y=ybottom, labels="inner", cex=0.95)',file="code.R",append=TRUE,sep="\n")
  if(n!=1){
    cat('text(x=xleft-0.08, y=ytop-0.02, labels="outer", cex=0.95)',file="code.R",append=TRUE,sep="\n")
  }
  if(n==1){
    cat('text(x=xleft-0.08, y=ytop-0.01, labels="outer", cex=0.95)
			    text(x=xright+0.025, y=ytop-0.04, labels=get("n1"), cex=1, adj=c(0,0))',file="code.R",append=TRUE,sep="\n")
  }else{
    cat('for(i in 1:n){
                    text(x=xright+0.028, y=ytop-gap*(i-1)-0.025, labels=get(paste("n",i,sep="")), cex=1, adj=c(0,0))
                }',file="code.R",append=TRUE,sep="\n")
  }
}			
cat("dev.off()",file="code.R",append=TRUE,sep="\n")
cat("circos.clear()",file="code.R",append=TRUE,sep="\n")

out <<- readLines("code.R")
file.remove("code.R")





