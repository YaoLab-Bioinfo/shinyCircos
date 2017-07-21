plotcircos <- function(x, color, plotTypes, units, rotation, gap.width, fontsize){
	par(mar = c(0.5,0.5,0.5,0.5),cex=fontsize)  
	circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
	circos.genomicInitialize.new(x,plotType=plotTypes,unit=units)
	circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = 0.05)
}

plotcircos.notrack <- function(x, plotTypes, units, rotation, gap.width, fontsize){
	par(mar = c(0.5,0.5,0.5,0.5),cex=fontsize)  
	circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
	circos.genomicInitialize.new(x,plotType=plotTypes,unit=units)
}

plotcircos.font <- function(x, color, plotTypes, units, rotation, gap.width, cexLabel, cexAxis){
	par(mar = c(0.5,0.5,0.5,0.5))  
	circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
	circos.genomicInitialize.new.font(x,plotType=plotTypes,unit=units,cexlabel=cexLabel, cexaxis=cexAxis)
	circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = 0.05)
}

plotcircos.notrack.font <- function(x, plotTypes, units, rotation, gap.width, cexLabel, cexAxis){
	par(mar = c(0.5,0.5,0.5,0.5))  
	circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
	circos.genomicInitialize.new.font(x,plotType=plotTypes,unit=units,cexlabel=cexLabel, cexaxis=cexAxis)
}

plotcircos.cyto <- function(x, plotTypes, units, rotation, gap.width, fontsize){
  par(mar = c(0.5,0.5,0.5,0.5),cex=fontsize)  
  circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
  circos.initializeWithIdeogram.new(x,plottype=plotTypes,unit=units)
}

plotcircos.cyto.font <- function(x, plotTypes, units, rotation, gap.width, cexLabel, cexAxis){
  par(mar = c(0.5,0.5,0.5,0.5))  
  circos.par("start.degree" = 90-rotation,"gap.degree" = gap.width,cell.padding = c(0, 0, 0, 0), track.margin=c(0,0))
  circos.initializeWithIdeogram.new.font(x,plottype=plotTypes,unit=units,cexlabel=cexLabel, cexaxis=cexAxis)
}

circos.genomicInitialize.new <- 
  function (data, sector.names = NULL, major.by = NULL, unit = "", plotType, tickLabelsStartFromZero = TRUE, track.height = 0.05, 
            ...) 
  {
    if (is.factor(data[[1]])) {
      fa = levels(data[[1]])
    }
    else {
      fa = unique(data[[1]])
    }
    if (!is.null(sector.names)) {
      if (length(sector.names) != length(fa)) {
        stop("length of `sector.names` and length of sectors differ.\n")
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
    if (any(plotType %in% c("axis", "labels"))) {
      circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, 
                                    track.height = track.height, panel.fun = function(region, 
                                                                                      value, ...) {
                                      sector.index = get.cell.meta.data("sector.index")
                                      xlim = get.cell.meta.data("xlim")
                                      if (tickLabelsStartFromZero) {
                                        offset = xlim[1]
                                        if (is.null(major.by)) {
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(xlim[1], xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if (major.by > 1e+06) {
                                          major.tick.labels = paste((major.at - offset)/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if (major.by > 1000) {
                                          major.tick.labels = paste((major.at - offset)/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste((major.at - offset), 
                                                                    "bp", sep = "")
                                        }
                                      }
                                      else {
                                        if (is.null(major.by)) {
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(floor(xlim[1]/major.by) * major.by, 
                                                       xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if (major.by > 1e+06) {
                                          major.tick.labels = paste(major.at/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if (major.by > 1000) {
                                          major.tick.labels = paste(major.at/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste(major.at, "bp", 
                                                                    sep = "")
                                        }
                                      }
                                      
                                      if (unit=="") { major.tick.labels <- gsub("[mkbp]","",major.tick.labels,ignore.case = T)}
                                      
                                      if (all(c("axis", "labels") %in% plotType)) {
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * par("cex"), labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                        circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], 
                                                    cex = par("cex"), adj = c(0.5, -0.1*par("cex")*2.1-(par("cex")-1)*3), niceFacing = TRUE)
                                      }
                                      else if ("labels" %in% plotType) {
                                        circos.text(mean(xlim), 0, labels = sector.names[sector.index], 
                                                    cex = par("cex"), adj = c(0.5, -0.1*par("cex")*2.1-(par("cex")-1)*3), niceFacing = TRUE)
                                      }
                                      else if ("axis" %in% plotType) {
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
  function (data, sector.names = NULL, major.by = NULL, unit = "", plotType, tickLabelsStartFromZero = TRUE, track.height = 0.05, cexlabel, cexaxis,
            ...) 
  {
    if (is.factor(data[[1]])) {
      fa = levels(data[[1]])
    }
    else {
      fa = unique(data[[1]])
    }
    if (!is.null(sector.names)) {
      if (length(sector.names) != length(fa)) {
        stop("length of `sector.names` and length of sectors differ.\n")
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
    if (any(plotType %in% c("axis", "labels"))) {
      circos.genomicTrackPlotRegion(data, ylim = c(0, 1), bg.border = NA, 
                                    track.height = track.height, panel.fun = function(region, 
                                                                                      value, ...) {
                                      sector.index = get.cell.meta.data("sector.index")
                                      xlim = get.cell.meta.data("xlim")
                                      if (tickLabelsStartFromZero) {
                                        offset = xlim[1]
                                        if (is.null(major.by)) {
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(xlim[1], xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if (major.by > 1e+06) {
                                          major.tick.labels = paste((major.at - offset)/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if (major.by > 1000) {
                                          major.tick.labels = paste((major.at - offset)/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste((major.at - offset), 
                                                                    "bp", sep = "")
                                        }
                                      }
                                      else {
                                        if (is.null(major.by)) {
                                          xlim = get.cell.meta.data("xlim")
                                          major.by = .default.major.by()
                                        }
                                        major.at = seq(floor(xlim[1]/major.by) * major.by, 
                                                       xlim[2], by = major.by)
                                        major.at = c(major.at, major.at[length(major.at)] + 
                                                       major.by)
                                        if (major.by > 1e+06) {
                                          major.tick.labels = paste(major.at/1e+06, 
                                                                    "MB", sep = "")
                                        }
                                        else if (major.by > 1000) {
                                          major.tick.labels = paste(major.at/1000, 
                                                                    "KB", sep = "")
                                        }
                                        else {
                                          major.tick.labels = paste(major.at, "bp", 
                                                                    sep = "")
                                        }
                                      }
                                      
                                      if (unit=="") { major.tick.labels <- gsub("[mkbp]","",major.tick.labels,ignore.case = T)}
									  
                                      if (all(c("axis", "labels") %in% plotType)) {
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * cexaxis, labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                        circos.text(mean(xlim), 1.2, labels = sector.names[sector.index], 
                                                    cex = cexlabel, adj = c(0.5, -0.1*cexlabel*2.1-(cexlabel-1)*3), niceFacing = TRUE)
                                      }
                                      else if ("labels" %in% plotType) {
                                        circos.text(mean(xlim), 0, labels = sector.names[sector.index], 
                                                    cex = cexlabel, adj = c(0.5, -0.1*cexlabel*2.1-(cexlabel-1)*3), niceFacing = TRUE)
                                      }
                                      else if ("axis" %in% plotType) {
                                        circos.axis(h = 0, major.at = major.at, labels = major.tick.labels, 
                                                    labels.cex = 0.49 * cexaxis, labels.facing = "clockwise", 
                                                    major.tick.percentage = 0.2)
                                      }
                                    })
    }
    circos.par(cell.padding = op, points.overflow.warning = ow)
    return(invisible(NULL))
  }

  circos.initializeWithIdeogram.new <- 
  function (cytoband = paste(system.file(package = "circlize"), 
                             "/extdata/cytoBand.txt", sep = ""), species = NULL, sort.chr = TRUE, 
            chromosome.index = NULL, major.by = NULL, units = "", plottype, plotType = c("ideogram", 
                                                                                      "axis", "labels"), track.height = 0.05, ideogram.height = 0.05,
            ...) 
  {
    e = try(cytoband <- read.cytoband(cytoband, species = species, 
                                      sort.chr = sort.chr, chromosome.index = chromosome.index), 
            silent = TRUE)
    if (class(e) == "try-error" && !is.null(species)) {
      e2 = try(cytoband <- read.chromInfo(species = species, 
                                          sort.chr = sort.chr, chromosome.index = chromosome.index), 
               silent = TRUE)
      if (class(e2) == "try-error") {
        message(e)
        message(e2)
        stop("Cannot download either cytoband or chromInfo file from UCSC.\n")
      }
      else {
        message("Downloading cytoBand file from UCSC failed. Use chromInfo file instead.\nNote ideogram track will be removed from the plot.")
        plotType = setdiff(plotType, "ideogram")
        if (is.null(chromosome.index)) {
          chromInfo = read.chromInfo(species = species)
          chr_len = sort(chromInfo$chr.len, decreasing = TRUE)
          i = which(chr_len[seq_len(length(chr_len) - 1)]/chr_len[seq_len(length(chr_len) - 
                                                                            1) + 1] > 5)[1]
          if (length(i)) {
            chromosome = chromInfo$chromosome[chromInfo$chromosome %in% 
                                                names(chr_len[chr_len >= chr_len[i]])]
          }
          else {
            chromosome = chromInfo$chromosome
          }
          cytoband = read.chromInfo(species = species, 
                                    chromosome.index = chromosome, sort.chr = sort.chr)
        }
      }
    }
    else if (class(e) == "try-error") {
      stop(e)
    }
    df = cytoband$df
    chromosome = cytoband$chromosome
    if (is.null(chromosome.index)) {
      chromosome.index = chromosome
    }
    df[[1]] = factor(as.vector(df[[1]]), levels = chromosome.index)
    sn = unique(as.vector(df[[1]]))
    o.cell.padding = circos.par("cell.padding")
    circos.par(cell.padding = c(o.cell.padding[1], 0, o.cell.padding[3], 
                                0))
    circos.genomicInitialize.new(df, sector.names = sn, major.by = major.by, 
                                 plotType = plottype, track.height = track.height, unit=units, ...)
    if (any(plotType %in% "ideogram")) {
      circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, 
                                    track.height = ideogram.height, panel.fun = function(region, 
                                                                                         value, ...) {
                                      col = cytoband.col(value[[2]])
                                      circos.genomicRect(region, value, ybottom = 0, 
                                                         ytop = 1, col = col, border = NA, ...)
                                      xlim = get.cell.meta.data("xlim")
                                      circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
                                    }, cell.padding = c(0, 0, 0, 0))
    }
  }

   circos.initializeWithIdeogram.new.font <- 
  function (cytoband = paste(system.file(package = "circlize"), 
                             "/extdata/cytoBand.txt", sep = ""), species = NULL, sort.chr = TRUE, 
            chromosome.index = NULL, major.by = NULL, units = "", plottype, plotType = c("ideogram", 
                                                                                      "axis", "labels"), track.height = 0.05, ideogram.height = 0.05, cexlabel, cexaxis,
            ...) 
  {
    e = try(cytoband <- read.cytoband(cytoband, species = species, 
                                      sort.chr = sort.chr, chromosome.index = chromosome.index), 
            silent = TRUE)
    if (class(e) == "try-error" && !is.null(species)) {
      e2 = try(cytoband <- read.chromInfo(species = species, 
                                          sort.chr = sort.chr, chromosome.index = chromosome.index), 
               silent = TRUE)
      if (class(e2) == "try-error") {
        message(e)
        message(e2)
        stop("Cannot download either cytoband or chromInfo file from UCSC.\n")
      }
      else {
        message("Downloading cytoBand file from UCSC failed. Use chromInfo file instead.\nNote ideogram track will be removed from the plot.")
        plotType = setdiff(plotType, "ideogram")
        if (is.null(chromosome.index)) {
          chromInfo = read.chromInfo(species = species)
          chr_len = sort(chromInfo$chr.len, decreasing = TRUE)
          i = which(chr_len[seq_len(length(chr_len) - 1)]/chr_len[seq_len(length(chr_len) - 
                                                                            1) + 1] > 5)[1]
          if (length(i)) {
            chromosome = chromInfo$chromosome[chromInfo$chromosome %in% 
                                                names(chr_len[chr_len >= chr_len[i]])]
          }
          else {
            chromosome = chromInfo$chromosome
          }
          cytoband = read.chromInfo(species = species, 
                                    chromosome.index = chromosome, sort.chr = sort.chr)
        }
      }
    }
    else if (class(e) == "try-error") {
      stop(e)
    }
    df = cytoband$df
    chromosome = cytoband$chromosome
    if (is.null(chromosome.index)) {
      chromosome.index = chromosome
    }
    df[[1]] = factor(as.vector(df[[1]]), levels = chromosome.index)
    sn = unique(as.vector(df[[1]]))
    o.cell.padding = circos.par("cell.padding")
    circos.par(cell.padding = c(o.cell.padding[1], 0, o.cell.padding[3], 
                                0))
    circos.genomicInitialize.new.font(df, sector.names = sn, major.by = major.by, 
                                 plotType = plottype, track.height = track.height, unit=units, cexlabel=cexlabel, cexaxis=cexaxis,...)
    if (any(plotType %in% "ideogram")) {
      circos.genomicTrackPlotRegion(df, ylim = c(0, 1), bg.border = NA, 
                                    track.height = ideogram.height, panel.fun = function(region, 
                                                                                         value, ...) {
                                      col = cytoband.col(value[[2]])
                                      circos.genomicRect(region, value, ybottom = 0, 
                                                         ytop = 1, col = col, border = NA, ...)
                                      xlim = get.cell.meta.data("xlim")
                                      circos.rect(xlim[1], 0, xlim[2], 1, border = "black")
                                    }, cell.padding = c(0, 0, 0, 0))
    }
  }

plotfigg <- function(input, output, trackindx, data.L, data.L1, data.L2, data.C, data.T, hltTrack.List, hltdata.List, heightSize, widthSize, colorChr, gap.width, cexAxis, cexAxislabel, unitChr, labelChr, fontsizeChr, trackChr, datatypeChr, transparencyHlt, transparencyhltLinks, transparencyTrack, transparencyLinks, marginLinks, selcolorLinks, barBoundary, coldir1Track, coldir2Track, colrectTrack, colorTrack, colorLinks, linksTrack, typeTrack, coltypeTk, rectTrack, borderTrack, directionTrack, colorlineTrack, heightTrack, marginTrack , bgcolTrack){       
	   ## *** The highlight regions ***
	   if(!is.null(data.L)){
		assign("highlightLinks",input$highlightLinks)
		assign("hltdataLinks",input$hltDataLinks)
        if(nchar(hltdataLinks)==0){
		}else{
		tmpL <- matrix(strsplit(hltdataLinks, "\n")[[1]])
		colnamesL <- c("chr","start","end","color")
		datL <- matrix(0, length(tmpL), length(colnamesL))
		colnames(datL) <- colnamesL
		for(l in 1:length(tmpL)){
		   rowL <- strsplit(tmpL[l], ",")[[1]]
		   datL[l,] <- rowL
		   }
		datL <- data.frame(datL,stringsAsFactors = F)
		datL$start <- as.numeric(datL$start)
		datL$end <- as.numeric(datL$end)
		datL$color <- datL$color
		queryL <- GRanges(seqnames = datL$chr,ranges=IRanges(start=datL$start,end=datL$end),seqinfo = NULL)
		subj1 <- GRanges(seqnames = data.L1[,1],ranges=IRanges(start=data.L1[,2],end=data.L1[,3]),seqinfo = NULL)
		subj2 <- GRanges(seqnames = data.L2[,1],ranges=IRanges(start=data.L2[,2],end=data.L2[,3]),seqinfo = NULL)
		
        indx1 <<- findOverlaps(queryL,subj1)
        indx1 <<- data.frame(indx1,stringsAsFactors=F)
        indx1$queryHits <- as.numeric(indx1$queryHits)
        indx1$subjectHits <- as.numeric(indx1$subjectHits)
        hltregion1 <- data.L1[indx1$subjectHits,]
        data.LL1 <- data.L1
        hltregion1$color <- datL$color[indx1[,1]]

        indx2 <<- findOverlaps(queryL,subj2)
        indx2 <<- data.frame(indx2,stringsAsFactors=F)
        indx2$queryHits <- as.numeric(indx2$queryHits)
        indx2$subjectHits <- as.numeric(indx2$subjectHits)
        hltregion2 <- data.L2[indx2$subjectHits,]
        data.LL2 <- data.L2
        hltregion2$color <- datL$color[indx2[,1]]				
		}
		}	
	      output$circosfigure <- renderPlot({
			data.C[,2] <- as.numeric(data.C[,2])
			data.C[,3] <- as.numeric(data.C[,3])
	        circos.clear()
			colorChr <- gsub('\\"',"",colorChr)
			colorChr <- gsub("0x","#", colorChr)        
			repnumcol <- round(length(unique(data.C[,1]))/length(colorChr)) + 1
			colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]
			## *** The gap width ***
			repnumgap <- round(length(unique(data.C[,1]))/length(gap.width)) + 1
			gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]
			gap.width <- as.numeric(gap.width)
			rotation <- gap.width[length(gap.width)]/2			
			if(datatypeChr=="general"){
			if (trackChr=="track" & fontsizeChr!="custom") {
				plotcircos(data.C, color = colorChr, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, fontsize=fontsizeChr)
			}else if (trackChr=="track" & fontsizeChr=="custom") {
				plotcircos.font(data.C, color = colorChr, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=cexAxislabel, cexAxis=cexAxis)
			}else if (trackChr!="track" & fontsizeChr!="custom") {
				plotcircos.notrack(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, fontsize=fontsizeChr)
			}else if (trackChr!="track" & fontsizeChr=="custom"){
				plotcircos.notrack.font(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=cexAxislabel, cexAxis=cexAxis)
			}
			}else{
			if (fontsizeChr!="custom") {
			    plotcircos.cyto(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, fontsize=fontsizeChr)
			}else if (fontsizeChr=="custom") {
				plotcircos.cyto.font(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=cexAxislabel, cexAxis=cexAxis)
			}
			}
			if(!is.null(data.T)){
			for(i in 1:length(data.T)){
			data.TT <- data.T[[i]]
			for(x in 2:(ncol(data.TT)-1)){
			data.TT[,x] <- as.numeric(data.TT[,x])
			}
			tktype <- typeTrack[i]
			## *** The fill color for track ***
			coltypeTrack <- coltypeTk[i]
			if(coltypeTrack==2){
			tkcolor <- colorTrack[i]
			tkcolor <- gsub("\\s","",strsplit(tkcolor,",")[[1]])
			tkcolor <- gsub('\\"',"",tkcolor)
			tkcolor <- gsub("0x","#", tkcolor)
			}else{
            selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
			tkcolor <- sample(selcols,ncol(data.T[[i]])-3)
			}
			## *** The backgroud color for track ***
			tkbgcol <- bgcolTrack[i] 
			tkbgcol <- gsub("\\s","",strsplit(tkbgcol,",")[[1]])
			tkbgcol <- gsub('\\"',"",tkbgcol)
			tkbgcol <- gsub("0x","#", tkbgcol)        
			repnumcol <- round(length(unique(data.C[,1]))/length(tkbgcol)) + 1
			tkbgcol <- rep(tkbgcol, repnumcol)[1:length(unique(data.C[,1]))]
			## *** The track margin ***
			tkmargin <- marginTrack[i]
			tkmargin <- as.numeric(tkmargin)
			## *** The track height ***
			tkheight <- heightTrack[i]
			tkheight <- as.numeric(tkheight)
			## *** The baselines color ***
			tklinecolor <- colorlineTrack[i]
			if (nchar(tklinecolor)!=0){
			tklinecolor <<- gsub('\\"',"",tklinecolor)
			tklinecolor <<- gsub("0x","#", tklinecolor)
			}
			## *** Add border ***
			tkborder <- borderTrack[i]
			## *** The bar direction ***
			tkbardir <- directionTrack[i]
			if (tkbardir==2){
			tkbarvalue <- barBoundary[i]
			tkbarcol1 <- coldir1Track[i]    
			tkbarcol2 <- coldir2Track[i]
			tktransparency <- transparencyTrack[i]
			tkbarcol1 <- adjustcolor(tkbarcol1, alpha.f = tktransparency)
            tkbarcol2 <- adjustcolor(tkbarcol2, alpha.f = tktransparency)
			}
			tkrectcol <- rectTrack[i]
			if (tkrectcol==1){
			rectcol <- colrectTrack[i]
			if (rectcol=="blue"){
			rectcols <<- c("#EDEDFD","#6969F5","#00008B")
			}else if (rectcol=="red") {
			rectcols <<- c("#FDEDED","#F56969","#8B0000")
			}else if (rectcol=="green") {
			rectcols <<- c("#EDFBED","#69E169","#008B00")
			}else if (rectcol=="cyan") {
			rectcols <<- c("#EDFBFB","#69E1E1","#008B8B")
			}else if (rectcol=="purple") {
			rectcols <<- c("#F6F0FB","#B27FE1","#551A8B")
			}else if (rectcol=="pink") {
			rectcols <<- c("#FBEEF5","#E172AE","#8B1076")
			}else if (rectcol=="orange") {
			rectcols <<- c("#FDF5ED","#F5AE69","#8B4500")
			}else if (rectcol=="yellow") {
			rectcols <<- c("#FDFDED","#EFEF1A","#8B8B00")
			}else if (rectcol=="navy") {
			rectcols <<- c("#EDEDF6","#7272B8","#000080")
			}else if (rectcol=="seagreen") {
			rectcols <<- c("#F2FBF6","#4EEE94","#2E8B57")
			}else if (rectcol=="maroon") {
			rectcols <<- c("#FFF4FB","#FF69C7","#8B1C62")
			}else if (rectcol=="olivedrab") {
			rectcols <<- c("#FBFFF4","#C6FF52","#698B22")
			}else if (rectcol=="gold") {
			rectcols <<- c("#FFFCF1","#FFDD28","#8B7500")
			}else if (rectcol=="lightblue") {
			rectcols <<- c("#EFF5F7","#AFCDD7","#68838B")
			}else if (rectcol=="navy.yellow") {
			rectcols <<- c("#000080","#7B7B41","#FFFF00")
			}else if (rectcol=="purple.seagreen") {
			rectcols <<- c("#551A8B","#548994","#54FF9F")
			}else if (rectcol=="navy.orange") {
			rectcols <<- c("#000080","#7B5041","#FFA500")
			}else if (rectcol=="navy.cyan") {
			rectcols <<- c("#000080","#007BBD","#00FFFF")
			}else if (rectcol=="blue.red") {
			rectcols <<- c("#0000FF","#730083","#EE0000")
			}else if (rectcol=="green.red") {
			rectcols <<- c("#00EE00","#757800","#EE0000")
			}
			}
			## *** The transparency of color ***
			tktransparency <- transparencyTrack[i]
			tkcolor <- adjustcolor(tkcolor, alpha.f = tktransparency)
			data.TTT <- data.TT
			data.TTT$id <- paste(data.TTT[,1],data.TTT[,2],data.TTT[,3],sep="")
			data.TTT$num <- 1:nrow(data.TTT)
			## *** The links margin ***
			if(i != length(data.T)){
			   lkmargin <- 0
			}else{
			   lkmargin <- marginLinks
			}
			if(tkborder=="add"){
			tkborder <- "grey"
			}else{
			tkborder <- NA
			}
			columns <- c(1:ncol(data.TT))[-c(1:3)]		
			if(tktype=="line"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, numeric.column= columns, track.height = tkheight, track.margin=c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                                circos.genomicLines(region, value, col=tkcolor, lwd=1, lty=1, ...)
                              })
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0){
			      assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ind <- which(data.TTT$id %in% dattt$id)
                       ind <- c(ind-1,ind,ind+1)
                       dattt.fil <- unique(data.TTT[ind,])
                       dattt.fil <- dattt.fil[dattt.fil[,1] %in% x,]
                       dattt.fil <- dattt.fil[order(dattt.fil$num),]
                       dattt.fil$groups <- c(diff(dattt.fil$num),diff(dattt.fil$num)[1])
                       dattt.fill <- dattt.fil[dattt.fil$groups==1,]
                       circos.lines((dattt.fill[,2]+dattt.fill[,3])/2,dattt.fill[,4], col=m, lwd=1, lty=1)
                  })
             circos.lines((trackk[,2]+trackk[,3])/2,trackk[,4], col=tkcolor, lwd=1, lty=1)
            })         
           }
			}else if(tktype=="point"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin=c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                                circos.genomicPoints(region, value, numeric.column=columns-3, col=tkcolor, cex=0.5, pch=16, ...)
                              })							 		
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0){
			      assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }   
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       circos.points((dattt[,2]+dattt[,3])/2,dattt[,4], col=m, cex=0.6, pch=16)
                  })
                  circos.points((trackk[,2]+trackk[,3])/2,trackk[,4], col=tkcolor, cex=0.6, pch=16)
                  })           
            }
			}else if(tktype=="bar"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, numeric.column= columns, track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
			        if(length(columns)==1 & tkbardir==1){
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                        circos.genomicRect(region, value, ytop.column = 1, ybottom = min(data.TT[,4]), col=tkcolor, border = NA, ...)
                    }else if (length(columns)==1 & tkbardir==2) {
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
				    tkbarvalue <- as.numeric(tkbarvalue)
					indx <- value[,1] > tkbarvalue
                    if (length(value[indx,])!=0 & length(value[!indx,])!=0) {
                             circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                             circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }else if (length(value[indx,])!=0 & length(value[!indx,])==0) {
                             circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                    }else if (length(value[indx,])==0 & length(value[!indx,])!=0) {
                             circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }
					}else if(length(columns)==2 & tkbardir==1){
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[1], border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[length(tkcolor)], border = NA, ...)						
					}else if (length(columns)==2 & tkbardir==2) {
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol1, border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol2, border = NA, ...)						
					}
				})
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0 & tkbardir==1){
			      assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
                       circos.rect(xleft=dattt[,2], xright=dattt[,3],ytop=dattt[,4],ybottom=rep(ylim[1],nrow(dattt)), col=m, border = NA)
                  })
                  circos.rect(xleft=trackk[,2], xright=trackk[,3],ytop=trackk[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(trackk)), col=tkcolor, border = NA)
                  })          
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0 & tkbardir==2) {
			      assign("hltregion",hltregion.List[[i]])
				  hlttransparency <- transparencyHlt[i]
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
					   tkbarvalue <- as.numeric(tkbarvalue)
				       indx <- dattt[,4] > tkbarvalue
				       value <- dattt[,4]
				       region <- dattt[,c(1:3)]
					   if (length(value[indx])!=0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                       }else if (length(value[indx])!=0 & length(value[!indx])==0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                       }else if (length(value[indx])==0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                      } 
                  })
				   tkbarvalue <- as.numeric(tkbarvalue)
				   indx <- trackk[,4] > tkbarvalue
				   value <- trackk[,4]
				   region <- trackk[,c(1:3)]
                    if (length(value[indx])!=0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                    }else if (length(value[indx])!=0 & length(value[!indx])==0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                    }else if (length(value[indx])==0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                    }
                  })
			}
			}else if (tktype=="rect") {
			if (tkrectcol==2){
			data.TT[,4] <- as.numeric(as.factor(data.TT[,4]))   
			cols=c(brewer.pal(11,'Set3'),brewer.pal(9,'Set1')[c(-1,-3,-6)],brewer.pal(8,'Dark2'),"chartreuse","aquamarine","cornflowerblue","blue","cyan","bisque1","darkorchid2","firebrick1","gold1","magenta1","olivedrab1","navy","maroon1","tan","yellow3","black","bisque4","seagreen3","plum2","yellow1","springgreen","slateblue1","lightsteelblue1","lightseagreen","limegreen")
			selcol <- sample(cols,length(unique(data.TT[,4])))
			data.TT[,4] <- selcol[data.TT[,4]]
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)  
            })
			}else{
			f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(f(value[[1]]),alpha.f = tktransparency), border = NA, ...) 
			})
			}
			}
			}}			
			if (!is.null(data.L) && linksTrack){	
            if (is.null(data.T)){
			marginLinks <- as.numeric(marginLinks)
			circos.par(track.margin=c(0,marginLinks))
			}			
			if (colorLinks==2) {
			    colLinks <- adjustcolor(selcolorLinks, alpha.f = transparencyLinks)
				if(highlightLinks==1 & nchar(hltdataLinks)!=0){
				    colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    circos.genomicLink(linkk1, linkk2, col = colLinks, border = NA)
					lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if (nrow(hltregion11)!=0) {
                            circos.genomicLink(hltregion11, hltregion12, col = x, border = NA)
                        }
                        if (nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, col = x, border = NA)
                        }
				    })
				}else{
                   data.L1 <- data.L1[,c(1:3)]
				   data.L2 <- data.L2[,c(1:3)]
			       circos.genomicLink(data.L1, data.L2, col = colLinks, border = NA)
				}
			}else {
				if(highlightLinks==1 & nchar(hltdataLinks)!=0){
				    colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    circos.genomicLink(linkk1, linkk2, col = rand_color(nrow(linkk1), transparency = 1-transparencyLinks), border = NA)					
					lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if (nrow(hltregion11)!=0) {
                            circos.genomicLink(hltregion11, hltregion12, col = x, border = NA)
                        }
                        if (nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, col = x, border = NA)
                        }
				    })
				}else{
                   data.L1 <- data.L1[,c(1:3)]
				   data.L2 <- data.L2[,c(1:3)]
			       circos.genomicLink(data.L1, data.L2, col = rand_color(nrow(data.L1), transparency = 1-transparencyLinks), border = NA)
				}
			}
			}
			circos.clear()
	      }, height = heightSize, width = widthSize)
		  }
		  
plotfig <- function(input, trackindx, data.L, data.L1, data.L2, data.C, data.T, hltTrack.List, hltdata.List){
		if(!is.null(data.L)){
		assign("highlightLinks",input$highlightLinks)
		assign("hltdataLinks",input$hltDataLinks)
        if(nchar(hltdataLinks)==0){
		}else{
		tmpL <- matrix(strsplit(hltdataLinks, "\n")[[1]])
		colnamesL <- c("chr","start","end","color")
		datL <- matrix(0, length(tmpL), length(colnamesL))
		colnames(datL) <- colnamesL
		for(l in 1:length(tmpL)){
		   rowL <- strsplit(tmpL[l], ",")[[1]]
		   datL[l,] <- rowL
		   }
		datL <- data.frame(datL,stringsAsFactors = F)
		datL$start <- as.numeric(datL$start)
		datL$end <- as.numeric(datL$end)
		datL$color <- datL$color
		queryL <- GRanges(seqnames = datL$chr,ranges=IRanges(start=datL$start,end=datL$end),seqinfo = NULL)
		subj1 <- GRanges(seqnames = data.L1[,1],ranges=IRanges(start=data.L1[,2],end=data.L1[,3]),seqinfo = NULL)
		subj2 <- GRanges(seqnames = data.L2[,1],ranges=IRanges(start=data.L2[,2],end=data.L2[,3]),seqinfo = NULL)
		
        indx1 <<- findOverlaps(queryL,subj1)
        indx1 <<- data.frame(indx1,stringsAsFactors=F)
        indx1$queryHits <- as.numeric(indx1$queryHits)
        indx1$subjectHits <- as.numeric(indx1$subjectHits)
        hltregion1 <- data.L1[indx1$subjectHits,]
        data.LL1 <- data.L1
        hltregion1$color <- datL$color[indx1[,1]]

        indx2 <<- findOverlaps(queryL,subj2)
        indx2 <<- data.frame(indx2,stringsAsFactors=F)
        indx2$queryHits <- as.numeric(indx2$queryHits)
        indx2$subjectHits <- as.numeric(indx2$subjectHits)
        hltregion2 <- data.L2[indx2$subjectHits,]
        data.LL2 <- data.L2
        hltregion2$color <- datL$color[indx2[,1]]				
		}
		}	
			data.C[,2] <- as.numeric(data.C[,2])
			data.C[,3] <- as.numeric(data.C[,3])
	        circos.clear()
			colorChr <- gsub("\\s","",strsplit(input$colorChr,",")[[1]])
			colorChr <- gsub('\\"',"",colorChr)
			colorChr <- gsub("0x","#", colorChr)        
			repnumcol <- round(length(unique(data.C[,1]))/length(colorChr)) + 1
			colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]
			## *** The gap width ***
			gap.width <- gsub("\\s","",strsplit(input$gapChr,",")[[1]])
			repnumgap <- round(length(unique(data.C[,1]))/length(gap.width)) + 1
			gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]
			gap.width <- as.numeric(gap.width)
			rotation <- gap.width[length(gap.width)]/2			
			if(input$datatypeChr=="general"){
			if (input$trackChr=="track" & input$fontsizeChr!="custom") {
				plotcircos(data.C, color = colorChr, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, fontsize=input$fontsizeChr)
			}else if (input$trackChr=="track" & input$fontsizeChr=="custom") {
				plotcircos.font(data.C, color = colorChr, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=input$cexAxislabel, cexAxis=input$cexAxis)
			}else if (input$trackChr!="track" & input$fontsizeChr!="custom") {
				plotcircos.notrack(data.C, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, fontsize=input$fontsizeChr)
			}else if (input$trackChr!="track" & input$fontsizeChr=="custom"){
				plotcircos.notrack.font(data.C, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=input$cexAxislabel, cexAxis=input$cexAxis)
			}
			}else{
			if (input$fontsizeChr!="custom") {
			    plotcircos.cyto(data.C, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, fontsize=input$fontsizeChr)
			}else if (input$fontsizeChr=="custom") {
				plotcircos.cyto.font(data.C, plotTypes=unique(c(input$labelChr,"axis")), units=input$unitChr, rotation=rotation,  gap.width=gap.width, cexLabel=input$cexAxislabel, cexAxis=input$cexAxis)
			}			
			}
			if(!is.null(data.T)){
			for(i in 1:length(data.T)){
			data.TT <- data.T[[i]]
			for(x in 2:(ncol(data.TT)-1)){
			data.TT[,x] <- as.numeric(data.TT[,x])
			}
			assign("tktype",input[[paste("typeTrack",trackindx[i],sep="")]])      
			## *** The fill color for track ***
			assign("coltypeTrack",input[[paste("coltypeTrack",trackindx[i],sep="")]])
			if(coltypeTrack==2){
			assign("tkcolor",input[[paste("colorTrack",trackindx[i],sep="")]])
			tkcolor <- gsub("\\s","",strsplit(tkcolor,",")[[1]])
			tkcolor <- gsub('\\"',"",tkcolor)
			tkcolor <- gsub("0x","#", tkcolor)
			}else{
            selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
			tkcolor <- sample(selcols,ncol(data.T[[i]])-3)
			}
			## *** The backgroud color for track ***
			assign("tkbgcol",input[[paste("bgcolTrack",trackindx[i],sep="")]])
			tkbgcol <- gsub("\\s","",strsplit(tkbgcol,",")[[1]])
			tkbgcol <- gsub('\\"',"",tkbgcol)
			tkbgcol <- gsub("0x","#", tkbgcol)
			## *** The track margin ***
			assign("tkmargin",input[[paste("marginTrack",trackindx[i],sep="")]])
			tkmargin <- as.numeric(tkmargin)
			## *** The track height ***
			assign("tkheight",input[[paste("heightTrack",trackindx[i],sep="")]])
			tkheight <- as.numeric(tkheight)
			## *** The baselines color ***
			assign("tklinecolor",input[[paste("colorlineTrack",trackindx[i],sep="")]])
			if (nchar(tklinecolor)!=0){
			tklinecolor <<- gsub('\\"',"",tklinecolor)
			tklinecolor <<- gsub("0x","#", tklinecolor)
			}
			## *** Add border ***
			assign("tkborder",input[[paste("borderTrack",trackindx[i],sep="")]])
			## *** The bar direction ***
			assign("tkbardir",input[[paste("directionTrack",trackindx[i],sep="")]])
			if (tkbardir==2){
			assign("tkbarvalue",input[[paste("barBoundary",trackindx[i],sep="")]])
			assign("tkbarcol1",input[[paste("coldir1Track",trackindx[i],sep="")]])
			assign("tkbarcol2",input[[paste("coldir2Track",trackindx[i],sep="")]])
			assign("tktransparency",input[[paste("transparencyTrack",trackindx[i],sep="")]])
			tkbarcol1 <<- adjustcolor(tkbarcol1, alpha.f = tktransparency)
            tkbarcol2 <<- adjustcolor(tkbarcol2, alpha.f = tktransparency)
			}
			assign("tkrectcol",input[[paste("rectTrack",trackindx[i],sep="")]])
			if (tkrectcol==1){
			assign("rectcol",input[[paste("colrectTrack",trackindx[i],sep="")]])
			if (rectcol=="blue"){
			rectcols <<- c("#EDEDFD","#6969F5","#00008B")
			}else if (rectcol=="red") {
			rectcols <<- c("#FDEDED","#F56969","#8B0000")
			}else if (rectcol=="green") {
			rectcols <<- c("#EDFBED","#69E169","#008B00")
			}else if (rectcol=="cyan") {
			rectcols <<- c("#EDFBFB","#69E1E1","#008B8B")
			}else if (rectcol=="purple") {
			rectcols <<- c("#F6F0FB","#B27FE1","#551A8B")
			}else if (rectcol=="pink") {
			rectcols <<- c("#FBEEF5","#E172AE","#8B1076")
			}else if (rectcol=="orange") {
			rectcols <<- c("#FDF5ED","#F5AE69","#8B4500")
			}else if (rectcol=="yellow") {
			rectcols <<- c("#FDFDED","#EFEF1A","#8B8B00")
			}else if (rectcol=="navy") {
			rectcols <<- c("#EDEDF6","#7272B8","#000080")
			}else if (rectcol=="seagreen") {
			rectcols <<- c("#F2FBF6","#4EEE94","#2E8B57")
			}else if (rectcol=="maroon") {
			rectcols <<- c("#FFF4FB","#FF69C7","#8B1C62")
			}else if (rectcol=="olivedrab") {
			rectcols <<- c("#FBFFF4","#C6FF52","#698B22")
			}else if (rectcol=="gold") {
			rectcols <<- c("#FFFCF1","#FFDD28","#8B7500")
			}else if (rectcol=="lightblue") {
			rectcols <<- c("#EFF5F7","#AFCDD7","#68838B")
			}else if (rectcol=="navy.yellow") {
			rectcols <<- c("#000080","#7B7B41","#FFFF00")
			}else if (rectcol=="purple.seagreen") {
			rectcols <<- c("#551A8B","#548994","#54FF9F")
			}else if (rectcol=="navy.orange") {
			rectcols <<- c("#000080","#7B5041","#FFA500")
			}else if (rectcol=="navy.cyan") {
			rectcols <<- c("#000080","#007BBD","#00FFFF")
			}else if (rectcol=="blue.red") {
			rectcols <<- c("#0000FF","#730083","#EE0000")
			}else if (rectcol=="green.red") {
			rectcols <<- c("#00EE00","#757800","#EE0000")
			}
			}
			## *** The transparency of color ***
			assign("tktransparency",input[[paste("transparencyTrack",trackindx[i],sep="")]])
			tkcolor <- adjustcolor(tkcolor, alpha.f = tktransparency)
			data.TTT <- data.TT
			data.TTT$id <- paste(data.TTT[,1],data.TTT[,2],data.TTT[,3],sep="")
			data.TTT$num <- 1:nrow(data.TTT)
			## *** The links margin ***
			if(i != length(data.T)){
			   lkmargin <- 0
			}else{
			   lkmargin <- input$marginLinks
			}
			if(tkborder=="add"){
			tkborder <- "grey"
			}else{
			tkborder <- NA
			}
			columns <- c(1:ncol(data.TT))[-c(1:3)]		
			if(tktype=="line"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, numeric.column= columns, track.height = tkheight, track.margin=c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                                circos.genomicLines(region, value, col=tkcolor, lwd=1, lty=1, ...)
                              })
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0){
			      assign("hltregion",hltregion.List[[i]])
				  assign("hlttransparency",input[[paste("transparencyHlt",trackindx[i],sep="")]])
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (input$trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ind <- which(data.TTT$id %in% dattt$id)
                       ind <- c(ind-1,ind,ind+1)
                       dattt.fil <- unique(data.TTT[ind,])
                       dattt.fil <- dattt.fil[dattt.fil[,1] %in% x,]
                       dattt.fil <- dattt.fil[order(dattt.fil$num),]
                       dattt.fil$groups <- c(diff(dattt.fil$num),diff(dattt.fil$num)[1])
                       dattt.fill <- dattt.fil[dattt.fil$groups==1,]
                       circos.lines((dattt.fill[,2]+dattt.fill[,3])/2,dattt.fill[,4], col=m, lwd=1, lty=1)
                  })
             circos.lines((trackk[,2]+trackk[,3])/2,trackk[,4], col=tkcolor, lwd=1, lty=1)
            })          
           }
			}else if(tktype=="point"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin=c(lkmargin,tkmargin),
                              bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
							  if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                                circos.genomicPoints(region, value, numeric.column=columns-3, col=tkcolor, cex=0.5, pch=16, ...)
                              })							 		
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0){
			      assign("hltregion",hltregion.List[[i]])
				  assign("hlttransparency",input[[paste("transparencyHlt",trackindx[i],sep="")]])
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (input$trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }   
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       circos.points((dattt[,2]+dattt[,3])/2,dattt[,4], col=m, cex=0.6, pch=16)
                  })
                  circos.points((trackk[,2]+trackk[,3])/2,trackk[,4], col=tkcolor, cex=0.6, pch=16)
                  })          
            }
			}else if(tktype=="bar"){
			data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
			circos.genomicTrackPlotRegion(data.TT, numeric.column= columns, track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
			        if(length(columns)==1 & tkbardir==1){
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
                        circos.genomicRect(region, value, ytop.column = 1, ybottom = min(data.TT[,4]), col=tkcolor, border = NA, ...)
                    }else if (length(columns)==1 & tkbardir==2) {
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
				    tkbarvalue <- as.numeric(tkbarvalue)
					indx <- value[,1] > tkbarvalue
                    if (length(value[indx,])!=0 & length(value[!indx,])!=0) {
                             circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                             circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }else if (length(value[indx,])!=0 & length(value[!indx,])==0) {
                             circos.genomicRect(region[indx,], value[indx,], ytop.column = 1, ybottom = tkbarvalue, col=tkbarcol1, border = NA, ...)
                    }else if (length(value[indx,])==0 & length(value[!indx,])!=0) {
                             circos.genomicRect(region[!indx,], value[!indx,], ytop.column = 1, ybottom =  tkbarvalue, col=tkbarcol2, border = NA, ...)
                    }
					}else if(length(columns)==2 & tkbardir==1){
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[1], border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkcolor[length(tkcolor)], border = NA, ...)						
					}else if (length(columns)==2 & tkbardir==2) {
					if(nchar(tklinecolor)!=0){
							    xlim <- get.cell.meta.data("xlim")
                                ylim <- get.cell.meta.data("ylim")
                                y1 <- as.numeric(quantile(ylim,probs=0.25))
                                y2 <- as.numeric(quantile(ylim,probs=0.75))
                                circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                                circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
							  }
						circos.genomicRect(region, value, ytop.column = 1, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol1, border = NA, ...)
                        circos.genomicRect(region, value, ytop.column = 2, ybottom = min(c(data.TT[,4],data.TT[,5])), col=tkbarcol2, border = NA, ...)						
					}
				})
			assign("hltTrack",hltTrack.List[[i]])
			assign("hltdata",hltdata.List[[i]])
			if (nchar(hltdata)==0) {
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0 & tkbardir==1){
			      assign("hltregion",hltregion.List[[i]])
				  assign("hlttransparency",input[[paste("transparencyHlt",trackindx[i],sep="")]])
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (input$trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }   
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
                       circos.rect(xleft=dattt[,2], xright=dattt[,3],ytop=dattt[,4],ybottom=rep(ylim[1],nrow(dattt)), col=m, border = NA)
                  })
                  circos.rect(xleft=trackk[,2], xright=trackk[,3],ytop=trackk[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(trackk)), col=tkcolor, border = NA)
                  })          
            }else if (hltTrack==1 & nrow(hltregion.List[[i]])>0 & tkbardir==2) {
			      assign("hltregion",hltregion.List[[i]])
				  assign("hlttransparency",input[[paste("transparencyHlt",trackindx[i],sep="")]])
				  hltregion$color <- adjustcolor(hltregion$color, alpha.f = hlttransparency)
			      hltregion$color <- gsub("0x","#", hltregion$color)
                  chrr <- unique(hltregion[,1])
                  lapply(chrr, function(x){
				  datt <- hltregion[hltregion[,1] %in% x,]
                  trackk <- data.TTT[data.TTT[,1] %in% x,]
                  trackk <- trackk[!trackk$id %in% datt$id,]
                  col <- unique(datt$color)
				  if (input$trackChr=="track") {
				      circos.updatePlotRegion(sector.index = x, track.index=i+2, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }else{
				      circos.updatePlotRegion(sector.index = x, track.index=i+1, bg.col = tkbgcol[which(unique(data.C[,1])==x)], bg.border = tkborder)
				  }
				  if (nchar(tklinecolor)!=0) {
                      xlim <- get.cell.meta.data("xlim")
                      ylim <- get.cell.meta.data("ylim")
                      y1 <- as.numeric(quantile(ylim,probs=0.25))
                      y2 <- as.numeric(quantile(ylim,probs=0.75))
                      circos.lines(x=xlim,y=c(y1,y1), col=tklinecolor, lwd=0.1)
                      circos.lines(x=xlim,y=c(y2,y2), col=tklinecolor, lwd=0.1)
                  }    
                  lapply(col, function(m){
                       dattt <- datt[datt$color %in% m,]
                       ylim <- get.cell.meta.data("ylim")
					   tkbarvalue <- as.numeric(tkbarvalue)
				       indx <- dattt[,4] > tkbarvalue
				       value <- dattt[,4]
				       region <- dattt[,c(1:3)]
					   if (length(value[indx])!=0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                       }else if (length(value[indx])!=0 & length(value[!indx])==0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=m, border = NA)
                       }else if (length(value[indx])==0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=m, border = NA)
                      } 
                  })
				   tkbarvalue <- as.numeric(tkbarvalue)
				   indx <- trackk[,4] > tkbarvalue
				   value <- trackk[,4]
				   region <- trackk[,c(1:3)]
                    if (length(value[indx])!=0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                    }else if (length(value[indx])!=0 & length(value[!indx])==0) {
                             circos.rect(xleft=region[indx,2], xright=region[indx,3], ytop = value[indx], ybottom = rep(tkbarvalue,length(value[indx])), col=tkbarcol1, border = NA)
                    }else if (length(value[indx])==0 & length(value[!indx])!=0) {
                             circos.rect(xleft=region[!indx,2], xright=region[!indx,3], ytop = value[!indx], ybottom =  rep(tkbarvalue,length(value[!indx])), col=tkbarcol2, border = NA)
                    }
                  })
			}
			}else if (tktype=="rect") {
			if (tkrectcol==2){
			data.TT[,4] <- as.numeric(as.factor(data.TT[,4]))   
			cols=c(brewer.pal(11,'Set3'),brewer.pal(9,'Set1')[c(-1,-3,-6)],brewer.pal(8,'Dark2'),"chartreuse","aquamarine","cornflowerblue","blue","cyan","bisque1","darkorchid2","firebrick1","gold1","magenta1","olivedrab1","navy","maroon1","tan","yellow3","black","bisque4","seagreen3","plum2","yellow1","springgreen","slateblue1","lightsteelblue1","lightseagreen","limegreen")
			selcol <- sample(cols,length(unique(data.TT[,4])))
			data.TT[,4] <- selcol[data.TT[,4]]
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)   
            })
			}else{
			f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
			circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin=c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
            circos.genomicRect(region, value, col=adjustcolor(f(value[[1]]),alpha.f = tktransparency), border = NA, ...)   
			})
			}
			}
			}}			
			if (!is.null(data.L) && input$linksTrack){
            if (is.null(data.T)){
			marginLinks <- as.numeric(input$marginLinks)
			circos.par(track.margin=c(0,marginLinks))
			}		  			
			if (input$colorLinks==2) {
			    colLinks <- adjustcolor(input$selcolorLinks, alpha.f = input$transparencyLinks)
				if(highlightLinks==1 & nchar(hltdataLinks)!=0){
				    colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = input$transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = input$transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = input$transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    circos.genomicLink(linkk1, linkk2, col = colLinks, border = NA)
					lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if (nrow(hltregion11)!=0) {
                            circos.genomicLink(hltregion11, hltregion12, col = x, border = NA)
                        }
                        if (nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, col = x, border = NA)
                        }
				    })
				}else{
                   data.L1 <- data.L1[,c(1:3)]
				   data.L2 <- data.L2[,c(1:3)]
			       circos.genomicLink(data.L1, data.L2, col = colLinks, border = NA)
				}
			}else {
				if(highlightLinks==1 & nchar(hltdataLinks)!=0){
				    colL <- unique(datL[,4])
					colL <- adjustcolor(colL, alpha.f = input$transparencyhltLinks)
		            colL <- gsub("0x","#", colL)
					hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = input$transparencyhltLinks)
		            hltregion1$color <- gsub("0x","#", hltregion1$color)
					hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = input$transparencyhltLinks)
		            hltregion2$color <- gsub("0x","#", hltregion2$color)
                    linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
                    circos.genomicLink(linkk1, linkk2, col = rand_color(nrow(linkk1), transparency = 1-input$transparencyLinks), border = NA)					
					lapply(colL, function(x){
                        hltregion11 <- hltregion1[hltregion1$color %in% x,]
                        hltregion12 <- data.LL2[data.L2$num %in% hltregion11$num,]
                        hltregion12 <- hltregion12[,c(1:3)]
                        hltregion11 <- hltregion11[,c(1:3)]
                        hltregion22 <- hltregion2[hltregion2$color %in% x,]
                        hltregion21 <- data.LL1[data.L1$num %in% hltregion22$num,]
                        hltregion21 <- hltregion21[,c(1:3)]
                        hltregion22 <- hltregion22[,c(1:3)]
                        if (nrow(hltregion11)!=0) {
                            circos.genomicLink(hltregion11, hltregion12, col = x, border = NA)
                        }
                        if (nrow(hltregion22)!=0){
                            circos.genomicLink(hltregion21, hltregion22, col = x, border = NA)
                        }
				    })
				}else{
                   data.L1 <- data.L1[,c(1:3)]
				   data.L2 <- data.L2[,c(1:3)]
			       circos.genomicLink(data.L1, data.L2, col = rand_color(nrow(data.L1), transparency = 1-input$transparencyLinks), border = NA)
				}
			}
			}
			circos.clear()
			}

downloadfile = function(name){downloadHandler(
	  filename = function() { name },
	  content = function(file) {
	    chr <- read.csv(name,head=T,as.is=T)
		write.csv(chr, file, row.names=FALSE, quote=F)
    })
	}
	
updateText = function(x, input, session){if (input[[paste("uploadtrack",x,sep="")]] == 2) {
	        assign(paste("highlightclear",x,sep=""),input[[paste("clearText_button",x,sep="")]])
			if (get(paste("highlightclear",x,sep="")) == 0) return(NULL)
		    isolate({updateTextInput(session, paste("hltData",x,sep=""), label = ",", value = "")})
			}
			}
			
.default.major.by = function(sector.index = get.cell.meta.data("sector.index"),
	track.index = get.cell.meta.data("track.index")) {
	d = circos.par("major.by.degree")
	cell.start.degre = get.cell.meta.data("cell.start.degree", sector.index, track.index)
	tm = reverse.circlize(c(cell.start.degre, cell.start.degre-d), rep(get.cell.meta.data("cell.bottom.radius", sector.index = sector.index, track.index = track.index), 2))
	major.by = abs(tm[1, 1] - tm[2, 1])
	digits = as.numeric(gsub("^.*e([+-]\\d+)$", "\\1", sprintf("%e", major.by)))
	major.by = round(major.by, digits = -1*digits)
	return(major.by)
}
