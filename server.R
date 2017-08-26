
options(shiny.maxRequestSize = 200*1024^2)
library(circlize)
library(RColorBrewer)
library(GenomicRanges)
library(grDevices)
source("plot.R")

shinyServer(function(input, output, session) {
 ## *** Upload data ***
  observe({
    if (input$submit1>0) {
      isolate({
          inFile <- input$uploadChr
          if (is.null(input$uploadChr)) {
		  data.C <<- NULL
		  }else{
          data.C <<- read.table(inFile$datapath, as.is=TRUE, head=TRUE, sep=",")
		  }
		  data.T <<- lapply(1:10,function(x){
		  assign(paste("uploadtrack",x,sep=""),input[[paste("uploadtrack",x,sep="")]])
		  if (get(paste("uploadtrack",x,sep="")) == 2){
		  trackfil <- input[[paste("uploadTrackfile",x,sep="")]]
		  read.table(trackfil$datapath, as.is=TRUE, head=TRUE, sep=",")
		  }
		  })
		  trackindx <<- which(!unlist(lapply(data.T, is.null)))
		  data.T <<- data.T[trackindx]
		  if(length(data.T) == 0) {
		  data.T <<- NULL
		  }
		  if (!input$linksTrack)  {
		  data.L <<- NULL
		  }else if(input$linksTrack & !is.null(input$linksFile)){
		  trackfil <- input$linksFile
		  data.L <<- read.table(trackfil$datapath, as.is=TRUE, head=TRUE, sep=",")
		  data.L1 <<- data.L[,1:3]
		  data.L2 <<- data.L[,4:6]
		  data.L1[,2] <- as.numeric(data.L1[,2])
		  data.L1[,3] <- as.numeric(data.L1[,3])
		  data.L2[,2] <- as.numeric(data.L2[,2])
		  data.L2[,3] <- as.numeric(data.L2[,3])	  
		  data.L1$num <<- 1:nrow(data.L1)
          data.L2$num <<- 1:nrow(data.L2)
		  rownames(data.L1) <<- data.L1$num
		  rownames(data.L2) <<- data.L2$num
		  }       
## *** Show uploading data *** 
   output$viewChr <- renderTable(digits=3,{
		head(data.C,n=3)
  })
    output$chrdat <- reactive({
    return(!is.null(input$uploadChr))
  })
  outputOptions(output, "chrdat", suspendWhenHidden = FALSE)
        lapply(1:length(data.T),function(v){
		viewname <<- paste("viewTrack", trackindx[v],sep="")
		output[[viewname]] <<- renderTable(digits=3,{
		head(data.T[[v]],n=3)
		})
   output[[paste("trackdat",trackindx[v],sep="")]] <- reactive({
    return(ncol(data.T[[v]])==4)
  })
  outputOptions(output, paste("trackdat",trackindx[v],sep=""), suspendWhenHidden = FALSE)
  })
        output$viewLink <- renderTable(digits=3,{
		head(data.L,n=3)
  })
      })
    } else {NULL}
  })
  observe({
  ## *** Clear button ***
  observe({updateText(x=1,input=input,session=session)})
  observe({updateText(x=2,input=input,session=session)})
  observe({updateText(x=3,input=input,session=session)})
  observe({updateText(x=4,input=input,session=session)})
  observe({updateText(x=5,input=input,session=session)})
  observe({updateText(x=6,input=input,session=session)})
  observe({updateText(x=7,input=input,session=session)})
  observe({updateText(x=8,input=input,session=session)})
  observe({updateText(x=9,input=input,session=session)})
  observe({updateText(x=10,input=input,session=session)})
  observe({
  if (input$clearText_buttonLinks == 0) return(NULL)
	isolate({updateTextInput(session, "hltDataLinks", label = ",", value = "")})
	})	
	## *** Plot circos images *** 
	observe({	
	  if (input$submit2>0) {
	    isolate({
		## *** The plot dimensions ***
	      heightSize <<- input$myHeight
	      widthSize <<- input$myWidth
		  addlegend <<- input$seladdlegend
		  poslegend <<- input$selposlegend
		  legendtext <<- c()
		if(!is.null(data.T)){
		  hltregion.List <<- list()           
		  hltTrack.List <<- list()
		  hltdata.List <<- list()
		  barBoundary <<- c()
		  coldir1Track <<- c()
		  coldir2Track <<- c()
		  transparencyHlt <<- c()
          colorTrack <<- c()
		  colrectTrack <<- c()
		  transparencyTrack <<- c()
		  rectTrack <<- c()
		  rectcolTrack <<- c()
		  rectcoldisTrack <<- c()
		  borderTrack <<- c()
		  directionTrack <<- c()
		  colorlineTrack <<- c()
		  baselineTrack <<- c()
		  midpointTrack <<- c()
		  midhmapTrack <<- c()
		  colhmapTrack <<- c()
		  lineshmapTrack <<- c()
		  heightlinesTrack <<- c()
		  marginlinesTrack <<- c()
		  heightTrack <<- c()
		  marginTrack <<- c()
		  bgcolTrack <<- c()
		  typeTrack <<- c()
		  coltypeTk <<- c()  
		for(k in 1:length(data.T)){
			data.TT <- data.T[[k]]
			## *** The highlight regions ***
			assign("hltTrack",input[[paste("highlightTrack",trackindx[k],sep="")]])
			hltTrack.List[[k]] <<- hltTrack
			assign("hltdata",input[[paste("hltData",trackindx[k],sep="")]])
			hltdata.List[[k]] <<- hltdata
			hltregion.List[[k]] <<- ""
			if(nchar(input[[paste("hltData",trackindx[k],sep="")]])==0) {
            }else{
                tmp <- matrix(strsplit(hltdata, "\n")[[1]])
                myColnames <- c("chr","start","end","color")
                data <- matrix(0, length(tmp), length(myColnames))
                colnames(data)<-myColnames
                for(p in 1:length(tmp)){
                     myRow<-strsplit(tmp[p], ",")[[1]]
                     data[p,]<-myRow
                   }
                data <- data.frame(data,stringsAsFactors = F)
                data$start <- as.numeric(data$start)
                data$end <- as.numeric(data$end)
			query <- GRanges(seqnames = data$chr,ranges=IRanges(start=data$start,end=data$end),seqinfo = NULL)
            subj <- GRanges(seqnames = data.TT[,1],ranges=IRanges(start=data.TT[,2],end=data.TT[,3]),seqinfo = NULL) 
            indx <- findOverlaps(query,subj)
            indx <- data.frame(indx,stringsAsFactors=F)
			indx$queryHits <- as.numeric(indx$queryHits)
			indx$subjectHits <- as.numeric(indx$subjectHits)
            hltregion <- data.TT[indx$subjectHits,]
			hltregion$color <- data$color[indx[,1]]
			hltregion$id <- paste(hltregion[,1],hltregion[,2],hltregion[,3],sep="")
			hltregion.List[[k]] <<- hltregion
			}
			
			barBoundary <<- c(barBoundary,input[[paste("barBoundary",trackindx[k],sep="")]])
			coldir1Track <<- c(coldir1Track,input[[paste("coldir1Track",trackindx[k],sep="")]])
			coldir2Track <<- c(coldir2Track,input[[paste("coldir2Track",trackindx[k],sep="")]])
			transparencyHlt <<- c(transparencyHlt,input[[paste("transparencyHlt",trackindx[k],sep="")]])
            colorTrack <<- c(colorTrack,input[[paste("colorTrack",trackindx[k],sep="")]])			
			colrectTrack <<- c(colrectTrack,input[[paste("colrectTrack",trackindx[k],sep="")]])
			transparencyTrack <<- c(transparencyTrack,input[[paste("transparencyTrack",trackindx[k],sep="")]])
			rectTrack <<- c(rectTrack,input[[paste("rectTrack",trackindx[k],sep="")]])
			rectcolTrack <<- c(rectcolTrack,input[[paste("rectcolTrack",trackindx[k],sep="")]])
			rectcoldisTrack <<- c(rectcoldisTrack,input[[paste("rectcoldisTrack",trackindx[k],sep="")]])
			borderTrack <<- c(borderTrack,input[[paste("borderTrack",trackindx[k],sep="")]])
			directionTrack <<- c(directionTrack,input[[paste("directionTrack",trackindx[k],sep="")]])
			colorlineTrack <<- c(colorlineTrack,input[[paste("colorlineTrack",trackindx[k],sep="")]])
			baselineTrack <<- c(baselineTrack,input[[paste("baselineTrack",trackindx[k],sep="")]])
			midpointTrack <<- c(midpointTrack,input[[paste("midpointTrack",trackindx[k],sep="")]])
			midhmapTrack <<- c(midhmapTrack,input[[paste("midhmapTrack",trackindx[k],sep="")]])
			colhmapTrack <<- c(colhmapTrack,input[[paste("colhmapTrack",trackindx[k],sep="")]])
			lineshmapTrack <<- c(lineshmapTrack,input[[paste("lineshmapTrack",trackindx[k],sep="")]])
			heightlinesTrack <<- c(heightlinesTrack,input[[paste("heightlinesTrack",trackindx[k],sep="")]])
			marginlinesTrack <<- c(marginlinesTrack,input[[paste("marginlinesTrack",trackindx[k],sep="")]])
			heightTrack <<- c(heightTrack,input[[paste("heightTrack",trackindx[k],sep="")]])
			marginTrack <<- c(marginTrack,input[[paste("marginTrack",trackindx[k],sep="")]])
			bgcolTrack <<- c(bgcolTrack,input[[paste("bgcolTrack",trackindx[k],sep="")]])
			typeTrack <<- c(typeTrack,input[[paste("typeTrack",trackindx[k],sep="")]])
			coltypeTk <<- c(coltypeTk,input[[paste("coltypeTrack",trackindx[k],sep="")]])
			legendtext <<- c(legendtext,input[[paste("text",trackindx[k],sep="")]])
		}
		}
		transparencyhltLinks <<- input$transparencyhltLinks
		transparencyLinks <<- input$transparencyLinks
		selcolorLinks <<- input$selcolorLinks
		colorLinks <<- input$colorLinks
		linksTrack <<- input$linksTrack
		marginLinks <<- input$marginLinks
		cexAxis <<- input$cexAxis
		cexAxislabel <<- input$cexAxislabel
		unitChr <<- input$unitChr
		labelChr <<- input$labelChr
		fontsizeChr <<- input$fontsizeChr
		trackChr <<- input$trackChr
		datatypeChr <<- input$datatypeChr
		if (input$linksTrack)  {
		legendtext <<- c(legendtext,input$text11)
		}
		if (datatypeChr=="general" && trackChr!="track"){
		legendtext <<- legendtext
		}else{
		legendtext <<- c(input$text0,legendtext)
		}
		legendtext <<- legendtext[nchar(legendtext)>0]
		colorChr <<- gsub("\\s","",strsplit(input$colorChr,",")[[1]])
        gap.width <<- gsub("\\s","",strsplit(input$gapChr,",")[[1]])	
	plotfigg(input = input, output = output, trackindx = trackindx, data.L = data.L, data.L1 = data.L1, data.L2 = data.L2, data.C = data.C, barBoundary = barBoundary, coldir1Track = coldir1Track, 
	coldir2Track = coldir2Track, data.T = data.T, hltTrack.List = hltTrack.List, hltdata.List = hltdata.List, heightSize = heightSize, widthSize = widthSize, addlegend = addlegend, poslegend = poslegend, colorChr = colorChr,
	gap.width = gap.width, legendtext = legendtext, cexAxis = cexAxis, cexAxislabel = cexAxislabel, unitChr = unitChr, labelChr = labelChr, fontsizeChr = fontsizeChr, colorTrack = colorTrack, transparencyHlt = transparencyHlt,
	trackChr = trackChr, datatypeChr = datatypeChr, transparencyhltLinks = transparencyhltLinks, transparencyTrack = transparencyTrack, transparencyLinks = transparencyLinks,
	colorLinks = colorLinks, linksTrack = linksTrack, typeTrack = typeTrack, coltypeTk = coltypeTk, marginLinks = marginLinks, selcolorLinks = selcolorLinks, colrectTrack = colrectTrack,
	rectTrack = rectTrack, rectcolTrack = rectcolTrack, rectcoldisTrack = rectcoldisTrack, borderTrack = borderTrack, directionTrack = directionTrack, colorlineTrack = colorlineTrack, baselineTrack = baselineTrack, midpointTrack = midpointTrack, midhmapTrack = midhmapTrack, colhmapTrack = colhmapTrack, lineshmapTrack = lineshmapTrack, heightlinesTrack = heightlinesTrack, marginlinesTrack = marginlinesTrack, heightTrack = heightTrack, marginTrack = marginTrack , bgcolTrack = bgcolTrack)
	    })
	  } else {NULL}
	})
	## *** Download PDF file ***
	output$downloadPlotPDF <- downloadHandler(
	  filename <- function() { paste('shinyCircos.pdf') },
	  content <- function(file) {
	    pdf(file, width = input$myWidth/72, height = input$myHeight/72)
		 legendtext <<- c()
		 for(k in 1:10){
         legendtext <<- c(legendtext,input[[paste("text",k,sep="")]])
		 }
		 if (input$linksTrack)  {
		 legendtext <<- c(legendtext,input$text11)
		 }
		 if (input$datatypeChr=="general" && input$trackChr!="track"){
		 legendtext <<- legendtext
		 }else{
		 legendtext <<- c(input$text0,legendtext)
		 }
		 legendtext <<- legendtext[nchar(legendtext)>0]	 
		 if (fontsizeChr=="custom"){
		   if (length(legendtext)!=0 && input$seladdlegend==1 && input$selposlegend==1){
		   par(oma=c(0,0,0,0),mar = c(5,0.2,5,9.8),xpd=TRUE) 
		   }else{
		   par(mar = c(0.6,0.6,0.6,0.6)) 
		   }
		 }else{
		   if (length(legendtext)!=0 && input$seladdlegend==1 && input$selposlegend==1){
	       par(oma=c(0,0,0,0),mar = c(5,0.2,5,9.8),xpd=TRUE,cex=as.numeric(input$fontsizeChr)-0.05)
	       }else{
	       par(mar = c(0.6,0.6,0.6,0.6),cex=as.numeric(input$fontsizeChr)-0.05) 
	       }		 
		 }	 	 
		plotfig(input = input, trackindx = trackindx, data.L = data.L, data.L1 = data.L1, data.L2 = data.L2, data.C = data.C, data.T = data.T, hltTrack.List = hltTrack.List, hltdata.List = hltdata.List, legendtext = legendtext)
	    dev.off()
	  }, contentType = 'application/pdf')	  
	## *** Download SVG file ***
	output$downloadPlotSVG <- downloadHandler(
	  filename <- function() { paste('shinyCircos.svg') },
	  content <- function(file) {
	    svg(file, width = input$myWidth/72, height = input$myHeight/72)
		 legendtext <<- c()
		 for(k in 1:10){
         legendtext <<- c(legendtext,input[[paste("text",k,sep="")]])
		 }
		 if (input$linksTrack)  {
		 legendtext <<- c(legendtext,input$text11)
		 }
		 if (input$datatypeChr=="general" && input$trackChr!="track"){
		 legendtext <<- legendtext
		 }else{
		 legendtext <<- c(input$text0,legendtext)
		 }
		 legendtext <<- legendtext[nchar(legendtext)>0]
		 if (fontsizeChr=="custom"){
		   if (length(legendtext)!=0 && input$seladdlegend==1 && input$selposlegend==1){
		   par(oma=c(0,0,0,0),mar = c(5,0.2,5,9.8),xpd=TRUE) 
		   }else{
		   par(mar = c(0.6,0.6,0.6,0.6)) 
		   }
		 }else{
		   if (length(legendtext)!=0 && input$seladdlegend==1 && input$selposlegend==1){
	       par(oma=c(0,0,0,0),mar = c(5,0.2,5,9.8),xpd=TRUE,cex=as.numeric(input$fontsizeChr)-0.05)
	       }else{
	       par(mar = c(0.6,0.6,0.6,0.6),cex=as.numeric(input$fontsizeChr)-0.05) 
	       }		 
		 }	 
		plotfig(input = input, trackindx = trackindx, data.L = data.L, data.L1 = data.L1, data.L2 = data.L2, data.C = data.C, data.T = data.T, hltTrack.List = hltTrack.List, hltdata.List = hltdata.List, legendtext = legendtext)
	    dev.off()
	  }, contentType = 'image/svg')
	## *** Download sample data in csv format ***
	output$general.csv <- downloadfile("general.csv")
	output$cytoband.csv <- downloadfile("cytoband.csv")
	output$track1.csv <- downloadfile("track1.csv")
	output$track2.csv <- downloadfile("track2.csv")
	output$track3.csv <- downloadfile("track3.csv")
	output$track4.csv <- downloadfile("track4.csv")
	output$track5.csv <- downloadfile("track5.csv")
	output$track6.csv <- downloadfile("track6.csv")
	output$track7.csv <- downloadfile("track7.csv")
	output$track8.csv <- downloadfile("track8.csv")
	output$track9.csv <- downloadfile("track9.csv")
	output$track10.csv <- downloadfile("track10.csv")
	output$links.csv <- downloadfile("links.csv")
})
})


