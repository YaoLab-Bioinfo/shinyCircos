plotcircos <- function(x, color, plotTypes, units, rotation, gap.width, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new(x,plotType=plotTypes,unit=units)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column=4, connection_height=heightlabelschr, track.margin=c(0.01,marginlabelschr), side="outside")
  }		
  circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = 0.05)	
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

plotcircos.font <- function(x, color, plotTypes, units, rotation, gap.width, cexLabel, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new.font(x, plotType=plotTypes, unit=units, cexlabel=cexLabel)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(ylim = c(0, 1),bg.col = color, bg.border = NA, track.height = 0.05)
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

plotcircos.cyto <- function(x, plotTypes, units, rotation, gap.width, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){ 
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new(x, plotType = plotTypes, unit=units)
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(x, ylim = c(0, 1), bg.border = NA, 
                                track.height = 0.05, panel.fun = function(region, value, ...){
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

plotcircos.cyto.font <- function(x, plotTypes, units, rotation, gap.width, cexLabel, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, data.CN){
  circos.par("start.degree"=90-rotation, "gap.degree"=gap.width, cell.padding=c(0,0,0,0), track.margin=c(0,0))
  circos.genomicInitialize.new.font(x, plotType=plotTypes, unit=units, cexlabel=cexLabel)  
  if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1 && poslabelschr=="outer"){
    circos.genomicLabels(data.CN, labels.column = 4, connection_height = heightlabelschr, track.margin = c(0.01,marginlabelschr), side = "outside")
  }	
  circos.genomicTrackPlotRegion(x, ylim = c(0, 1), bg.border = NA, 
                                track.height = 0.05, panel.fun = function(region, value, ...){
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

downloadfile = function(name){downloadHandler(
  filename = function(){ name },
  content = function(file){
    chr <- read.csv(name,head=T,as.is=T)
    write.csv(chr, file, row.names=FALSE, quote=F)
  })
}

updateText = function(x, input, session){if(input[[paste("uploadtrack",x,sep="")]] == 2){
  assign(paste("highlightclear",x,sep=""),input[[paste("clearText_button",x,sep="")]])
  if(get(paste("highlightclear",x,sep="")) == 0) return(NULL)
  isolate({updateTextInput(session, paste("hltData",x,sep=""), label = ",", value = "")})
}
}

.default.major.by = function(sector.index = get.cell.meta.data("sector.index"),
                             track.index = get.cell.meta.data("track.index")){
  d = circos.par("major.by.degree")
  cell.start.degre = get.cell.meta.data("cell.start.degree", sector.index, track.index)
  tm = reverse.circlize(c(cell.start.degre, cell.start.degre-d), rep(get.cell.meta.data("cell.bottom.radius", sector.index = sector.index, track.index = track.index), 2))
  major.by = abs(tm[1, 1] - tm[2, 1])
  digits = as.numeric(gsub("^.*e([+-]\\d+)$", "\\1", sprintf("%e", major.by)))
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
}

plotfig <- function(input, output, trackindx, data.L, data.L1, data.L2, data.C, data.T, data.N, data.CN, hltTrack.List, hltdata.List, heightSize, widthSize, colorChr, gap.width, cexlabel, unitChr, labelChr, fontsize, trackChr, datatypeChr, transparencyHlt, legendtext, labeltext, poslabels, heightlabels, marginlabels, fillareaTrack, borderareaTrack, selreaTrack, addlegend, poslegend, transparencyhltLinks, labeltextchr, poslabelschr, heightlabelschr, marginlabelschr, transparencyTrack, transparencyLinks, marginLinks, selcolorLinks, barBoundary, coldir1Track, coldir2Track, colrectTrack, colorTrack, colorLinks, linksTrack, typeTrack, coltypeTk, colorcusTrack, rectTrack, rectcolTrack, rectcoldisTrack, rectcoldiscusTrack, borderTrack, gridsborderTrack, colgridsborderTrack, directionTrack, colorlineTrack, symbolTrack, pointsizeTrack, baselineTrack, heightTrack, colhmapTrack, lineshmapTrack, heightlinesTrack, marginlinesTrack, marginTrack , bgcolTrack){       
  ## *** The highlight regions ***
  if(!is.null(data.L)){
    highlightLinks <<- input$highlightLinks
    hltdataLinks <<- input$hltDataLinks
    hltregion1.export <<- NULL
    hltregion2.export <<- NULL	
    if(highlightLinks==1 && nchar(hltdataLinks)!=0){
      tmpL <- matrix(strsplit(hltdataLinks, "\n")[[1]])
      colnamesL <- c("chr","start","end","color")
      datL <- matrix(0, length(tmpL), length(colnamesL))
      colnames(datL) <- colnamesL
      for(l in 1:length(tmpL)){
        rowL <- strsplit(tmpL[l], ",")[[1]]
        if(length(rowL)==4){                                        
          datL[l,] <- rowL
        }		       
      }
      datL.ht <- as.data.frame(t(datL),stringsAsFactors=F)
      datL.ht1 <- unlist(lapply(datL.ht,function(x){
        all(x==0)
      })
      )
      output$errorinfo3 <- renderPrint({
        if(input$linksTrack && !is.null(input$linksFile) && input$highlightLinks==1 && nchar(input$hltDataLinks)>0){				
          validate(
            need(all(datL.ht1==FALSE), "Error: Data to highlight genomic regions should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color.")
          )
        }
      })
      outputOptions(output, "errorinfo3", suspendWhenHidden = FALSE)	
      datL <- data.frame(datL,stringsAsFactors=F)
      datL$start <- as.numeric(datL$start)
      datL$end <- as.numeric(datL$end)
      datL$color <- datL$color
      queryL <- GRanges(seqnames = datL$chr,ranges=IRanges(start=datL$start,end=datL$end),seqinfo=NULL)
      subj1 <- GRanges(seqnames = data.L1[,1],ranges=IRanges(start=data.L1[,2],end=data.L1[,3]),seqinfo=NULL)
      subj2 <- GRanges(seqnames = data.L2[,1],ranges=IRanges(start=data.L2[,2],end=data.L2[,3]),seqinfo=NULL)
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
      hltregion1.export <<- hltregion1
      hltregion2.export <<- hltregion2	  
    }
  }	
  output$circosfigure <- renderPlot({
    withProgress(message='Making plots',value = 0,{
      data.C[,2] <- as.numeric(data.C[,2])
      data.C[,3] <- as.numeric(data.C[,3])
      circos.clear()
      colorChr <- gsub('\\"',"",colorChr)
      colorChr <- gsub("0x","#", colorChr)        
      repnumcol <- round(length(unique(data.C[,1]))/length(colorChr))+1
      colorChr <- rep(colorChr, repnumcol)[1:length(unique(data.C[,1]))]
      ## *** The gap width ***
      repnumgap <- round(length(unique(data.C[,1]))/length(gap.width))+1
      gap.width <- rep(gap.width, repnumgap)[1:length(unique(data.C[,1]))]
      gap.width <- as.numeric(gap.width)
      rotation <- gap.width[length(gap.width)]/2	
      if(fontsize=="custom"){
        if(length(legendtext)!=0 && addlegend==1 && poslegend==1){
          par(oma=c(0,0,0,0),mar = c(9,0.5,1,9.5),xpd=TRUE,cex=cexlabel-0.1)
        }else{
          par(mar = c(0.6,0.6,0.6,0.6),cex=cexlabel-0.1) 
        }
      }else{
        if(length(legendtext)!=0 && addlegend==1 && poslegend==1){
          par(oma=c(0,0,0,0),mar = c(9,0.5,1,9.5),xpd=TRUE,cex=as.numeric(fontsize)-0.05)
        }else{
          par(mar = c(0.6,0.6,0.6,0.6),cex=as.numeric(fontsize)-0.05) 
        }			
      }			
      if(datatypeChr=="general"){
        if(trackChr=="track" && fontsize!="custom"){
          plotcircos(data.C, color=colorChr, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)
        }else if(trackChr=="track" && fontsize=="custom"){
          plotcircos.font(data.C, color=colorChr, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)
        }else if(trackChr!="track" && fontsize!="custom"){
          plotcircos.notrack(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, data.CN=data.CN, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr)
        }else if(trackChr!="track" && fontsize=="custom"){
          plotcircos.notrack.font(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, data.CN=data.CN, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr)
        }
      }else{
        if(fontsize!="custom"){			    
          plotcircos.cyto(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)
        }else if(fontsize=="custom"){			
          plotcircos.cyto.font(data.C, plotTypes=unique(c(labelChr,"axis")), units=unitChr, rotation=rotation, gap.width=gap.width, cexLabel=cexlabel-0.1, labeltextchr=labeltextchr, poslabelschr=poslabelschr, heightlabelschr=heightlabelschr, marginlabelschr=marginlabelschr, data.CN=data.CN)
        }
      }
      if(!is.null(data.T)){
        takindx <- 1
        if(!is.null(data.CN) && ncol(data.CN)==4 && labeltextchr==1){
          takindx <- takindx+2
        }
        tkcolor.export <<- list()
        data.TTC.export <<- list()
        for(i in 1:length(data.T)){
          data.TT <- data.T[[i]]
          tktype <- typeTrack[i]
          data.TT[,2] <- as.numeric(data.TT[,2])
          data.TT[,3] <- as.numeric(data.TT[,3])
          data.NN <- data.N[[i]]			
          ## *** The fill color for track ***
          data.TT$num <- 1:nrow(data.TT)
          data.TTC.export[[i]] <<- ""
          if(tktype!="rect" && tktype!="heatmap" && tktype!="ideogram"){
            data.TTC <- NULL
            coltypeTrack <- coltypeTk[i]
            if(coltypeTrack==2){
              tkcolor <- colorTrack[i]
              tkcolor <- gsub("\\s","",strsplit(tkcolor,",")[[1]])
              tkcolor <- gsub('\\"',"",tkcolor)
              tkcolor <- gsub("0x","#", tkcolor)			
            }else if((coltypeTrack==3 && ("color" %in% colnames(data.TT))) | (coltypeTrack==3 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack")){
              tkcolor <- colorcusTrack[i]
              tkcolor <- unlist(strsplit(tkcolor,";"))
              tkcolor <- data.frame(id=tkcolor,stringsAsFactors=F)
              tkcolor$group <- gsub("\\:.*","",tkcolor$id)
              tkcolor$cols <- gsub(".*\\:","",tkcolor$id)
              tkcolor$group <- gsub(" ","",tkcolor$group)
              tkcolor$cols <- gsub(" ","",tkcolor$cols)
              colname <- colnames(data.TT)
              if("color" %in% colnames(data.TT)){
                data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
              }else if(colnames(data.TT)[4]=="stack"){
                data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)						
              }
              data.TTC <- data.TTC[c(colname,"cols")]
              data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
              tkcolor <- unique(data.TTC$cols)
              data.TT <- data.TT[,1:4]
            }else if(coltypeTrack==1 && ("color" %in% colnames(data.TT))){
              selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
              tkcolor <- sample(selcols,length(unique(data.TT$color)))
              tkcolor.export[[i]] <<- tkcolor
              tkcolor <- data.frame(group=unique(data.TT$color),cols=tkcolor,stringsAsFactors=F)
              colname <- colnames(data.TT)
              data.TTC <- merge(data.TT,tkcolor,by.x="color",by.y="group",all.x=T)
              data.TTC <- data.TTC[c(colname,"cols")]
              data.TTC$cols[is.na(data.TTC$cols)] <- "grey"
              tkcolor <- unique(data.TTC$cols)
              data.TT <- data.TT[,1:4]			
            }else if(coltypeTrack==1 && ncol(data.T[[i]])==4 && colnames(data.TT)[4]=="stack"){
              selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
              tkcolor <- sample(selcols,length(unique(data.TT$stack)))	
              tkcolor.export[[i]] <<- tkcolor
              tkcolor <- data.frame(group=unique(data.TT$stack),cols=tkcolor,stringsAsFactors=F)
              colname <- colnames(data.TT)
              data.TTC <- merge(data.TT,tkcolor,by.x="stack",by.y="group",all.x=T)
              data.TTC <- data.TTC[c(colname,"cols")]
              data.TTC$cols[is.na(data.TTC$cols)] <- "grey"			
              tkcolor <- unique(data.TTC$cols)
              data.TT <- data.TT[,1:4]									
            }else{
              selcols <- c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "burlywood3", "magenta2")
              tkcolor <- sample(selcols,ncol(data.T[[i]])-3)
              tkcolor.export[[i]] <<- tkcolor
            }					
            if(!is.null(data.TTC)){
              data.TTC <- data.TTC[order(data.TTC$num),]
              rownames(data.TTC) <- NULL
              data.TTC$num <- NULL
            }
            data.TT$num <- NULL
            if(ncol(data.TT)==5 && ("color" %in% colnames(data.TT))){
              data.TT <- data.TT[,1:4]
            }else if(c(ncol(data.TT)==5 | ncol(data.TT)==6 | ncol(data.TT)==7) && ("pch" %in% colnames(data.TT)) && !("color" %in% colnames(data.TT))){
              data.TT <- data.TT[,1:4]
              tkcolor	<- tkcolor[1]
            }
            if(is.null(data.TTC)){
              data.TTC.export[[i]] <<- ""
            }else{
              data.TTC.export[[i]] <<- data.TTC
            }
          }		
          ## *** The backgroud color for track ***
          tkbgcol <- bgcolTrack[i] 
          tkbgcol <- gsub("\\s","",strsplit(tkbgcol,",")[[1]])
          tkbgcol <- gsub('\\"',"",tkbgcol)
          tkbgcol <- gsub("0x","#", tkbgcol)        
          repnumcol <- round(length(unique(data.C[,1]))/length(tkbgcol))+1
          tkbgcol <- rep(tkbgcol, repnumcol)[1:length(unique(data.C[,1]))]
          ## *** The track margin ***
          tkmargin <- marginTrack[i]
          tkmargin <- as.numeric(tkmargin)
          ## *** The track height ***
          tkheight <- heightTrack[i]
          tkheight <- as.numeric(tkheight)
          ## *** The y coordinates of baselines ***
          tklinecoord <- baselineTrack[i]
          tklinecoord <- as.numeric(unlist(strsplit(tklinecoord,",")))
          ## *** The symbol type & point size***
          if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
            symboltype <- symbolTrack[i]
            symboltype <- as.numeric(unlist(strsplit(symboltype,",")))
            symboltype <- rep(symboltype, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]  
            pointsize <- as.numeric(pointsizeTrack[1])
          }
          ## *** The baselines color ***
          tklinecolor <- colorlineTrack[i]
          if(nchar(tklinecolor)!=0){
            tklinecolor <<- gsub('\\"',"",tklinecolor)
            tklinecolor <<- gsub("0x","#", tklinecolor)
            tklinecolor <- unlist(strsplit(tklinecolor,","))
            tklinecolor <- rep(tklinecolor, length(tklinecoord))[1:length(tklinecoord)]  
          }
          if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
            tklinecol <<- gsub('\\"',"",tklinecolor)
            tklinecol <<- gsub("0x","#", tklinecol)
            tklinecol <- unlist(strsplit(tklinecol,","))
            tklinecol <- rep(tklinecol, length(unique(data.T[[i]][,4])))[1:length(unique(data.T[[i]][,4]))]   
          }
          ## *** The fill color for track ***
          hmapcols <- gsub('\\"',"",colhmapTrack[i])    
          hmapcols <- unlist(strsplit(hmapcols,"\\."))   			
          ## *** Add connection ***
          lineshmap <- lineshmapTrack[i]
          if(lineshmap==1){
            heightlines <<- heightlinesTrack[i]			
            marginlines <<- marginlinesTrack[i]
          }			
          ## *** Add border ***
          tkborder <- borderTrack[i]
          gridsborder <- gridsborderTrack[i]
          if(gridsborder=="add"){
            tkbordercol <<- colgridsborderTrack[i]
            if(nchar(tkbordercol)==0){
              tkbordercol <<- NA
            }
          }else{
            tkbordercol <<- NA
          }
          ## *** The bar direction ***
          tkbardir <- directionTrack[i]
          if(tkbardir==2){
            tkbarvalue <- barBoundary[i]
            tkbarcol1 <- coldir1Track[i]    
            tkbarcol2 <- coldir2Track[i]
            tktransparency <- transparencyTrack[i]
            tkbarcol1 <- adjustcolor(tkbarcol1, alpha.f = tktransparency)
            tkbarcol2 <- adjustcolor(tkbarcol2, alpha.f = tktransparency)
          }
          ## *** The data color ***
          tkrectcol <- rectTrack[i]
          ## *** Select color ***
          selrectcol <- rectcolTrack[i]
          if(tkrectcol==1){
            rectcol <- colrectTrack[i]
            if(rectcol=="blue"){
              rectcols <<- c("#EDEDFD","#6969F5","#00008B")
            }else if(rectcol=="red"){
              rectcols <<- c("#FDEDED","#F56969","#8B0000")
            }else if(rectcol=="green"){
              rectcols <<- c("#EDFBED","#69E169","#008B00")
            }else if(rectcol=="cyan"){
              rectcols <<- c("#EDFBFB","#69E1E1","#008B8B")
            }else if(rectcol=="purple"){
              rectcols <<- c("#F6F0FB","#B27FE1","#551A8B")
            }else if(rectcol=="pink"){
              rectcols <<- c("#FBEEF5","#E172AE","#8B1076")
            }else if(rectcol=="orange"){
              rectcols <<- c("#FDF5ED","#F5AE69","#8B4500")
            }else if(rectcol=="yellow"){
              rectcols <<- c("#FDFDED","#EFEF1A","#8B8B00")
            }else if(rectcol=="navy"){
              rectcols <<- c("#EDEDF6","#7272B8","#000080")
            }else if(rectcol=="seagreen"){
              rectcols <<- c("#F2FBF6","#4EEE94","#2E8B57")
            }else if(rectcol=="maroon"){
              rectcols <<- c("#FFF4FB","#FF69C7","#8B1C62")
            }else if(rectcol=="olivedrab"){
              rectcols <<- c("#FBFFF4","#C6FF52","#698B22")
            }else if(rectcol=="gold"){
              rectcols <<- c("#FFFCF1","#FFDD28","#8B7500")
            }else if(rectcol=="lightblue"){
              rectcols <<- c("#EFF5F7","#AFCDD7","#68838B")
            }else if(rectcol=="navy.yellow"){
              rectcols <<- c("#000080","#7B7B41","#FFFF00")
            }else if(rectcol=="purple.seagreen"){
              rectcols <<- c("#551A8B","#548994","#54FF9F")
            }else if(rectcol=="navy.orange"){
              rectcols <<- c("#000080","#7B5041","#FFA500")
            }else if(rectcol=="navy.cyan"){
              rectcols <<- c("#000080","#007BBD","#00FFFF")
            }else if(rectcol=="blue.red"){
              rectcols <<- c("#0000FF","#730083","#EE0000")
            }else if(rectcol=="green.red"){
              rectcols <<- c("#00EE00","#757800","#EE0000")
            }
          }else if(tkrectcol==2 && selrectcol==2){
            rectcols <- rectcoldisTrack[i]
            data.TT[,4] <- rectcols
          }else if(tkrectcol==2 && selrectcol==3){
            rectcols <- rectcoldiscusTrack[i]
            rectcols <- unlist(strsplit(rectcols,";"))
            rectcols <- data.frame(id=rectcols,stringsAsFactors=F)
            rectcols$group <- gsub("\\:.*","",rectcols$id)
            rectcols$cols <- gsub(".*\\:","",rectcols$id)
            rectcols$group <- gsub(" ","",rectcols$group)
            rectcols$cols <- gsub(" ","",rectcols$cols)
            colname <- colnames(data.TT)[1:3]
            data.TT <- merge(data.TT,rectcols,by.x=colnames(data.TT)[4],by.y="group",all.x=T)
            data.TT <- data.TT[c(colname,"cols")]
          }
          ## *** The transparency of color ***
          tktransparency <- transparencyTrack[i]
          if((tktype!="rect" && tktype!="heatmap" && tktype!="ideogram") | (tktype=="line" && fillareaTrack[i]!="add")){
            tkcolor <- adjustcolor(tkcolor, alpha.f = tktransparency)
          }
          data.TTT <- data.T[[i]]
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
          if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1){
            takindx <- takindx+2
          }
          if(poslabels[i]=="inner"){
            takindx <- takindx-2
          }	
          output$errorinfo4 <- renderPrint({
            if(!("color" %in% colnames(data.T[[i]])) & ncol(data.T[[i]])>=4 & colnames(data.T[[i]])[4]!="stack"){	
              if(input[[paste("uploadtrack",trackindx[i],sep="")]]==2){						
				validate(  
                  need(input[[paste("coltypeTrack",trackindx[i],sep="")]]!=3, paste("Error: Data color error for Track",trackindx[i],". The type of data color should be 'Random' or 'Custom for data with multi-column' based on the uploaded data without column as 'color' or 'stack'.",sep=""))
				)
              }
            }
          })
          outputOptions(output, "errorinfo4", suspendWhenHidden = FALSE)	
          output$errorinfo5 <- renderPrint({			  
            if(input[[paste("uploadtrack",trackindx[i],sep="")]]==2 && input[[paste("highlightTrack",trackindx[i],sep="")]]==1){		
              validate(
                need(nchar(hltdata.List[[i]])>0, paste("Warning: Highlight regions are empty for Track",trackindx[i],". Please input applicable genomic regions.",sep=""))
              )
            }
          })
          outputOptions(output, "errorinfo5", suspendWhenHidden = FALSE) 			  
          if(tktype=="line"){
            ## *** Fill the area ***
            if(fillareaTrack[i]!="add"){
              area <- FALSE
              borderset <- NA
              lwdnum <- 1
            }else if(fillareaTrack[i]=="add" && selreaTrack[i]==1){
              area <- TRUE
              lwdnum <- 0.2
            }else if(fillareaTrack[i]=="add" && selreaTrack[i]==2){				
              area <- TRUE
              borderset <- NA
              if(nchar(borderareaTrack[i])!=0){					
                borderset <- adjustcolor(borderareaTrack[i],alpha.f = tktransparency)
              }
              lwdnum <- 0.2
            }	
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){		
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }				
            if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
              bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
                if(coltypeTrack==2){
                  data.TT[data.TT[,4] %in% x,1:3]
                }else{
                  data.TTC[data.TTC[,4] %in% x,1:3]
                }
              })			
              circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
                                            bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region, value, ...){
                                              i = getI(...)			
                                              if(coltypeTrack==1){
                                                circos.genomicLines(region, value, col=tkcolor[i], lty=1, ...)
                                              }else if(coltypeTrack==2){
                                                circos.genomicLines(region, value, col=tkcolor[1], lty=1, ...)
                                              }else if(coltypeTrack==3){
                                                circos.genomicLines(region, value, col=tkcolor[i], lty=1, ...)
                                              }
                                            })
            }else{
              data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])			
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
                                            })
              if(coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){
                data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
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
                })
              }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && ("cols" %in% colnames(data.TTC))){
                data.TTC$id <- paste(data.TTC[,1],data.TTC[,2],data.TTC[,3],sep="")
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
                })			
              }
              assign("hltTrack",hltTrack.List[[i]])
              assign("hltdata",hltdata.List[[i]])
              
              if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && (length(columns)==1 | colnames(data.T[[i]])[5]=="color")){               
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
                })         
              }
            }
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }	
          }else if(tktype=="point"){
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }		
            if(ncol(data.T[[i]])==4 && colnames(data.T[[i]])[4]=="stack"){
              bed_list <- lapply(unique(data.T[[i]][,4]),function(x){
                if(coltypeTrack==2){
                  data.TT[data.TT[,4] %in% x,1:3]
                }else{
                  data.TTC[data.TTC[,4] %in% x,1:3]
                }
              })			
              circos.genomicTrackPlotRegion(bed_list, stack = TRUE, track.height = tkheight, track.margin = c(lkmargin,tkmargin),
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
                                            })
            }else{
              data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
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
                                            })
              if(!("cex" %in% colnames(data.T[[i]]))){
                if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){  			
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=dat$pch)  
                  })			
                }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){   			
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                  })
                }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
                  lapply(unique(data.T[[i]][,1]),function(x){
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
                  })				
                }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
                  lapply(unique(data.T[[i]][,1]),function(x){
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
                  })			
                }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=0.6, pch=16)
                  })			
                }
              }else if("cex" %in% colnames(data.T[[i]])){
                if(coltypeTrack==3 && ("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){  			
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=dat$pch)  
                  })			
                }else if(coltypeTrack==3 && ncol(data.TTC)>=6 && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){   			
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                  })
                }else if(coltypeTrack!=3 && ("pch" %in% colnames(data.T[[i]])) && ("color" %in% colnames(data.T[[i]]))){
                  lapply(unique(data.T[[i]][,1]),function(x){
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
                  })				
                }else if(("pch" %in% colnames(data.T[[i]])) && !("color" %in% colnames(data.T[[i]]))){
                  lapply(unique(data.T[[i]][,1]),function(x){
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
                  })			
                }else if(coltypeTrack==1 && ("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC)) && ("cols" %in% colnames(data.TTC))){
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
                    circos.points((dat[,2]+dat[,3])/2,dat[,4], col=adjustcolor(dat$cols,alpha.f = tktransparency), cex=dat$cex, pch=16)
                  })			
                }else if(!("color" %in% colnames(data.T[[i]])) && !("pch" %in% colnames(data.TTC))){
                  lapply(unique(data.T[[i]][,1]),function(x){
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
                  })
                }
              }				
              assign("hltTrack",hltTrack.List[[i]])
              assign("hltdata",hltdata.List[[i]])  
              if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && (length(columns)==1 | any(c("color","pch","cex") %in% colnames(data.T[[i]])))){
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
                })           
              }
            }
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }
          }else if(tktype=="bar"){
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }		
            data.TT[,ncol(data.TT)] <- as.numeric(data.TT[,ncol(data.TT)])
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
            })			
            if(length(columns)==1 && tkbardir==1 && coltypeTrack==3 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){		
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
                circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)                
              })
            }else if(length(columns)==1 && tkbardir==1 && coltypeTrack==1 && ncol(data.TTC)>=6 && ("cols" %in% colnames(data.TTC))){		
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
                circos.rect(xleft=dat[,2], xright=dat[,3],ytop=dat[,4],ybottom=rep(get.cell.meta.data("ylim")[1],nrow(dat)), col=adjustcolor(dat$cols,alpha.f = tktransparency), border = NA)                
              })
            }					
            assign("hltTrack",hltTrack.List[[i]])
            assign("hltdata",hltdata.List[[i]])		
            if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && tkbardir==1 && length(columns)==1){
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
                  circos.rect(xleft=dattt[,2], xright=dattt[,3], ytop=dattt[,4], ybottom=rep(ylim[1],nrow(dattt)), col=m, border = NA)
                })
                circos.rect(xleft=trackk[,2], xright=trackk[,3], ytop=trackk[,4], ybottom=rep(get.cell.meta.data("ylim")[1],nrow(trackk)), col=tkcolor[1], border = NA)				  
              })          
            }else if(hltTrack==1 && !is.null(nrow(hltregion.List[[i]])) && nrow(hltregion.List[[i]])>0 && tkbardir==2 && length(columns)==1){
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
              })
            }
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }
          }else if(tktype=="rect"){
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }	
            output$errorinfo6 <- renderPrint({
              if(input[[paste("uploadtrack",trackindx[i],sep="")]]==2){
                if(input[[paste("rectTrack",trackindx[i],sep="")]]==2 && input[[paste("rectcolTrack",trackindx[i],sep="")]]!=2){  			
                  validate(
                    need(length(unique(data.T[[i]][,4]))<=50, paste("Error: The 4th column of the uploaded data should be a discrete character vector with no more than 50 groups for 'Discrete' type. Please choose 'Gradual' type or upload applicable data for Track",trackindx[i],".",sep=""))
                  )
                }
                if(input[[paste("rectTrack",trackindx[i],sep="")]]==1){
                  validate(
                    need(is.numeric(data.T[[i]][,4]), paste("Error: Data uploaded is not appropriate 'Discrete' data. Please select appropriate 'Data type' for Track",trackindx[i],".",sep=""))
                  )		
                }				
              }
            })
            outputOptions(output, "errorinfo6", suspendWhenHidden = FALSE)				
            if(tkrectcol==2){
              if(selrectcol==1){
                data.TT[,4] <- as.numeric(as.factor(data.TT[,4]))   
                cols <- c(brewer.pal(11,'Set3'),brewer.pal(9,'Set1')[c(-1,-3,-6)],brewer.pal(8,'Dark2'),"chartreuse","aquamarine","cornflowerblue","blue","cyan","bisque1","darkorchid2","firebrick1","gold1","magenta1","olivedrab1","navy","maroon1","tan","yellow3","black","bisque4","seagreen3","plum2","yellow1","springgreen","slateblue1","lightsteelblue1","lightseagreen","limegreen")
                selcol <- sample(cols,length(unique(data.TT[,4])))
                tkcolor.export[[i]] <<- selcol
                data.TT[,4] <- selcol[data.TT[,4]]
                circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
                  circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)  
                })
              }else{
                circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
                  circos.genomicRect(region, value, col=adjustcolor(value[[1]],alpha.f = tktransparency), border = NA, ...)  
                })
              }
            }else{
              f <- colorRamp2(breaks = c(min(data.TT[,4]), mean(data.TT[,4]), max(data.TT[,4])), colors = rectcols)
              circos.genomicTrackPlotRegion(data.TT, ylim=c(0,1),track.height = tkheight, track.margin = c(lkmargin,tkmargin), bg.col = tkbgcol, bg.border = tkborder, panel.fun = function(region,value,...){
                circos.genomicRect(region, value, col=adjustcolor(f(value[[1]]),alpha.f = tktransparency), border = NA, ...) 
              })
            }
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }
          }		
          else if(tktype=="heatmap"){
            data.TT$num <- NULL			
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }
            break1 <- min(as.numeric(as.matrix(data.TT[,-c(1:3)])))
            break2 <- max(as.numeric(as.matrix(data.TT[,-c(1:3)])))
            midpoint <- (break1+break2)/2
            f <- colorRamp2(breaks = c(break1, midpoint, break2), colors = hmapcols)
            if(is.na(tkbordercol)){
              if(lineshmap==2){
                circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                                              panel.fun = function(region, value, ...){
                                                circos.genomicRect(region, value, col = f(value[[1]]),
                                                                   border = f(value[[1]]), posTransform = posTransform.default, ...)
                                              }, bg.border = NA)
              }else{
                circos.genomicPosTransformLines(data.TT, posTransform = posTransform.default,
                                                horizontalLine = "top", track.height = heightlines, track.margin = c(0,marginlines))
                circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                                              panel.fun = function(region, value, ...){
                                                circos.genomicRect(region, value, col = f(value[[1]]),
                                                                   border = f(value[[1]]), posTransform = posTransform.default, ...)
                                              }, bg.border = NA)			
              }			
            }else{
              if(lineshmap==2){
                circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                                              panel.fun = function(region, value, ...){
                                                circos.genomicRect(region, value, col = f(value[[1]]), lwd = 0.1,
                                                                   border = tkbordercol, posTransform = posTransform.default, ...)
                                              }, bg.border = NA)
              }else{
                circos.genomicPosTransformLines(data.TT, posTransform = posTransform.default,
                                                horizontalLine = "top", track.height = heightlines, track.margin = c(0,marginlines))
                circos.genomicTrackPlotRegion(data.TT, track.height = tkheight, track.margin = c(lkmargin,tkmargin), stack = TRUE,
                                              panel.fun = function(region, value, ...){
                                                circos.genomicRect(region, value, col = f(value[[1]]), lwd = 0.1,
                                                                   border = tkbordercol, posTransform = posTransform.default, ...)
                                              }, bg.border = NA)			
              }
            }
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }			
          }else if(tktype=="ideogram"){
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="outer"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "outside")
            }
            circos.genomicIdeogram(data.TT,track.height = tkheight, track.margin = c(lkmargin,tkmargin))
            if(!is.null(data.NN) && ncol(data.NN)==4 && labeltext[i]==1 && poslabels[i]=="inner"){
              circos.genomicLabels(data.NN, labels.column = 4, connection_height = heightlabels[i], track.margin = c(0.01,marginlabels[i]), side = "inside")
            }
          }
          if(poslabels[i]=="inner"){
            takindx <- takindx+3
          }else{
            takindx <- takindx+1
          }
        }}			
      if(!is.null(data.L) && linksTrack){	
        if(is.null(data.T)){
          circos.par(track.margin = c(0,marginLinks))
        }
        rou <- get_most_inside_radius()
        rou <- rou[1]
        output$errorinfo7 <- renderPrint({
          if(highlightLinks==1 && input$linksTrack){		
            validate(
              need(nchar(hltdataLinks)>0, "Warning: Data to highlight regions for links are empty. Please input applicable genomic regions.")
            )
          }
        })
        outputOptions(output, "errorinfo7", suspendWhenHidden = FALSE)		
        output$errorinfo8 <- renderPrint({
          if(!("color" %in% colnames(data.L))){
            if(input$colorLinks==2 && input$linksTrack){			
              validate(
                need(!(":" %in% unlist(strsplit(selcolorLinks,""))), "Error: Data color error for links. The type of data color should be 'Random' or 'Specific'.")
              )
            }
          }
        })
        outputOptions(output, "errorinfo8", suspendWhenHidden = FALSE)       		
        if(colorLinks==2){
          splitcol <- ":" %in% unlist(strsplit(selcolorLinks,""))		  
          if(ncol(data.L)==7 && colnames(data.L)[7]=="color" && splitcol){
            data.L$num <- 1:nrow(data.L)				
            selcolorLinks <- unlist(strsplit(selcolorLinks,";"))
            selcolorLinks <- data.frame(id=selcolorLinks,stringsAsFactors=F)
            selcolorLinks$group <- gsub("\\:.*","",selcolorLinks$id)
            selcolorLinks$cols <- gsub(".*\\:","",selcolorLinks$id)
            selcolorLinks$group <- gsub(" ","",selcolorLinks$group)
            selcolorLinks$cols <- gsub(" ","",selcolorLinks$cols)
            data.LC <- merge(data.L,selcolorLinks,by.x="color",by.y="group",all.x=T)
            data.LC$cols[is.na(data.LC$cols)] <- "grey"
            data.LC <- data.LC[order(data.LC$num),]
            data.LC$num <- NULL
            rownames(data.LC) <- NULL
            data.L$num <- NULL
            colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)
          }else if(ncol(data.L)==6 && !splitcol){		  
            colLinks <- adjustcolor(selcolorLinks, alpha.f = transparencyLinks)
          }
          if(highlightLinks==1 && (!is.null(hltregion1.export) | !is.null(hltregion2.export))){          
            colL <- unique(datL[,4])
            colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
            colL <- gsub("0x","#", colL)
            hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
            hltregion1$color <- gsub("0x","#", hltregion1$color)
            hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
            hltregion2$color <- gsub("0x","#", hltregion2$color)			
            linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
            linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
            colindx <- (!data.LL1$num %in% c(hltregion1$num,hltregion2$num)) & (!data.LL2$num %in% c(hltregion1$num,hltregion2$num))
            if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
              circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks[colindx], border = NA)
            }else{
              circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks, border = NA)
            }
            lapply(colL, function(x){
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
            })
          }else{
            if(!(ncol(data.L)==6 && splitcol)){		  
              data.L1 <- data.L[,c(1:3)]
              data.L2 <- data.L[,c(4:6)]
              circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA)
            }
          }
        }else{
          if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
            groupnum <- length(unique(data.L[,7]))
            linkscolor.export <<- rand_color(groupnum)
            randcolorLinks <- data.frame(group=unique(data.L[,7]), cols=linkscolor.export, stringsAsFactors=F)
            data.LC <- merge(data.L,randcolorLinks,by.x="color",by.y="group",all.x=T)	
            colLinks <- adjustcolor(data.LC$cols, alpha.f = transparencyLinks)
          }	
          if(highlightLinks==1 && (!is.null(hltregion1.export) | !is.null(hltregion2.export))){ 
            colL <- unique(datL[,4])
            colL <- adjustcolor(colL, alpha.f = transparencyhltLinks)
            colL <- gsub("0x","#", colL)
            hltregion1$color <- adjustcolor(hltregion1$color, alpha.f = transparencyhltLinks)
            hltregion1$color <- gsub("0x","#", hltregion1$color)
            hltregion2$color <- adjustcolor(hltregion2$color, alpha.f = transparencyhltLinks)
            hltregion2$color <- gsub("0x","#", hltregion2$color)			
            linkk1 <- data.LL1[!data.LL1$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
            linkk2 <- data.LL2[!data.LL2$num %in% c(hltregion1$num,hltregion2$num),][,c(1:3)]
            colindx <- (!data.LL1$num %in% c(hltregion1$num,hltregion2$num)) & (!data.LL2$num %in% c(hltregion1$num,hltregion2$num))
            if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
              circos.genomicLink(linkk1, linkk2, rou = rou, col = colLinks[colindx], border = NA)										
            }else{
              linkscolor.export <<- rand_color(nrow(linkk1), transparency = 1-transparencyLinks)					   
              circos.genomicLink(linkk1, linkk2, rou = rou, col = linkscolor.export, border = NA)					
            }
            lapply(colL, function(x){
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
            })
          }else{
            data.L1 <- data.L1[,c(1:3)]
            data.L2 <- data.L2[,c(1:3)]
            if(ncol(data.L)==7 && colnames(data.L)[7]=="color"){
              circos.genomicLink(data.L1, data.L2, rou = rou, col = colLinks, border = NA)				   
            }else{
              linkscolor.export <<- rand_color(nrow(data.L1), transparency = 1-transparencyLinks)				   
              circos.genomicLink(data.L1, data.L2, rou = rou, col = linkscolor.export, border = NA)
            }
          }
        }
      }							
      n <- length(legendtext)
      if(n!=0 && addlegend==1){
        if(poslegend==1 && fontsize!="custom"){
          xleft <- 1.2+(as.numeric(fontsize)-1)*0.25
        }else if(poslegend==1 && fontsize=="custom"){
          xleft <- 1.2+(as.numeric(cexlabel)-1)*0.25
        }else{
          xleft <- -0.01
        }
        xright <- xleft+0.02 
        ybottom <- -0.13*0.22/10-n*0.03
        ytop <- -0.13*0.22/10+n*0.03		
        len <- ytop-ybottom
        gap <- len/(n-0.8)
        for(r in 1:n){
          assign(paste("n",r,sep=""),legendtext[r])
        }		
        rect(xleft, ybottom, xright, ytop, col = "black")
        polygon(x=c(xleft-0.01,(xleft+xright)/2,xright+0.01), y=c(ybottom,ybottom-0.02,ybottom), col="black")
        text(x=xleft-0.08, y=ybottom, labels="inner", cex=0.95)
        if(n!=1){
          text(x=xleft-0.08, y=ytop-0.02, labels="outer", cex=0.95)
        }
        if(n==1){
          text(x=xleft-0.08, y=ytop-0.01, labels="outer", cex=0.95)
          text(x=xright+0.025, y=ytop-0.04, labels=get("n1"), cex=1, adj=c(0,0))
        }else{
          for(w in 1:n){
            text(x=xright+0.028, y=ytop-gap*(w-1)-0.025, labels=get(paste("n",w,sep="")), cex=1, adj=c(0,0))
          }
        }
      }			
      circos.clear()
      figurecp <<- recordPlot()
      incProgress(1/1, detail="Finished")
    })
  }, height=heightSize, width=widthSize)
}
