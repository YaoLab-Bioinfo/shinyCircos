
options(shiny.maxRequestSize = 200*1024^2)
options(warn=-1)
library(circlize)
library(RColorBrewer)
library(GenomicRanges)
library(data.table)
library(grDevices)
source("plot.R")
print(sessionInfo())

shinyServer(function(input, output, session){
  ## *** Upload data ***
  observe({
    if(input$submit1>0){
      isolate({
        inFile <- input$uploadChr
        if(is.null(input$uploadChr)){
          data.C <<- NULL
        }else{
          data.C <<- data.frame(fread(inFile$datapath),stringsAsFactors = F)
          data.C.name <<- inFile$name
        }   
        if(!is.null(input$uploadChr) && !is.null(input$markTrackfile0)){
          data.CN <<- data.frame(fread(input$markTrackfile0$datapath),stringsAsFactors = F)
          data.CN.name <<- input$markTrackfile0$name
        }else{
          data.CN <<- NULL
          data.CN.name <<- NULL
        }	
        uploadtrack.export <<- c()		
        uploadTrackfile.export <<- c()  
        data.T <<- lapply(1:10,function(x){
          assign(paste("uploadtrack",x,sep=""),input[[paste("uploadtrack",x,sep="")]])	
          uploadtrack.export <<- c(uploadtrack.export,get(paste("uploadtrack",x,sep="")))
          trackfil <- input[[paste("uploadTrackfile",x,sep="")]]
          uploadTrackfile.export <<- c(uploadTrackfile.export,trackfil$name)		  
          if(get(paste("uploadtrack",x,sep="")) == 2 && !is.null(trackfil)){
            data.frame(fread(trackfil$datapath),stringsAsFactors = F)
          }
        })
        markTrackfile.export <<- c()
        data.N <<- lapply(1:10,function(x){
          assign(paste("uploadtrack",x,sep=""),input[[paste("uploadtrack",x,sep="")]])   
          trackfil <- input[[paste("markTrackfile",x,sep="")]]
          if(is.null(trackfil)){
            markTrackfile.export <<- c(markTrackfile.export,"")		  
          }else{
            markTrackfile.export <<- c(markTrackfile.export,trackfil$name)		  
          }		  
          if(get(paste("uploadtrack",x,sep="")) == 2 && !is.null(trackfil)){
            data.frame(fread(trackfil$datapath),stringsAsFactors = F)
          }
        })  		  		  
        trackindx <<- which(!unlist(lapply(data.T, is.null)))
        data.T <<- data.T[trackindx]
        if(length(data.T) == 0){
          data.T <<- NULL
        }		  
        data.N <<- data.N[trackindx]
        if(length(data.N) == 0){
          data.N <<- NULL
        }
        linksTrack.export <<- input$linksTrack	
        linksFile.export <<- input$linksFile	  
        if(!input$linksTrack){
          data.L <<- NULL
        }else if(input$linksTrack && !is.null(input$linksFile)){
          trackfil <- input$linksFile
          linksFile.name <<- trackfil$name	  
          data.L <<- data.frame(fread(trackfil$datapath),stringsAsFactors = F)
          if(ncol(data.L)==6 | ncol(data.L)==7){
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
        }
        ## *** Show uploading data *** 
        output$viewChr <- renderTable(digits=3,{
          head(data.C,n=3)
        })
        output$viewlabelChr <- renderTable(digits=3,{
          head(data.CN,n=3)
        })
        output$chrdat <- reactive({
          return(!is.null(input$uploadChr))
        })
        output$chrlabel <- reactive({
          return(!is.null(input$uploadChr) && !is.null(data.CN))
        }) 
        outputOptions(output, "chrdat", suspendWhenHidden = FALSE)
        outputOptions(output, "chrlabel", suspendWhenHidden = FALSE)
        lapply(1:length(data.T),function(v){
          viewname <<- paste("viewTrack", trackindx[v],sep="")
          output[[viewname]] <<- renderTable(digits=3,{
            head(data.T[[v]],n=3)
          })		
          viewnamelabel <<- paste("viewlabelTrack", trackindx[v],sep="")
          output[[viewnamelabel]] <<- renderTable(digits=3,{
            head(data.N[[v]],n=3)
          })				 
          output[[paste("marklabel",trackindx[v],sep="")]] <- reactive({
            return(!is.null(data.N[[v]]))
          })
          output[[paste("trackdat",trackindx[v],sep="")]] <- reactive({
            return(ncol(data.T[[v]])==4 | (ncol(data.T[[v]])>=5 && ("color" %in% colnames(data.T[[v]]))) | (ncol(data.T[[v]])>=5 && ("pch" %in% colnames(data.T[[v]]))) | (ncol(data.T[[v]])>=5 && ("cex" %in% colnames(data.T[[v]]))))
          })          
          output[[paste("stackmd",trackindx[v],sep="")]] <- reactive({
            return(ncol(data.T[[v]])==4 && colnames(data.T[[v]])[4]=="stack")
          })          
          output[[paste("labeldat",trackindx[v],sep="")]] <- reactive({
            return(!is.null(data.N[[v]]))
          }) 
          outputOptions(output, paste("marklabel",trackindx[v],sep=""), suspendWhenHidden = FALSE)  
          outputOptions(output, paste("trackdat",trackindx[v],sep=""), suspendWhenHidden = FALSE)
          outputOptions(output, paste("labeldat",trackindx[v],sep=""), suspendWhenHidden = FALSE)
          outputOptions(output, paste("stackmd",trackindx[v],sep=""), suspendWhenHidden = FALSE)   
        })
        output$viewLink <- renderTable(digits=3,{
          head(data.L,n=3)
        })
        output$errorinfo1 <- renderPrint({
          for(i in 1:10){
            if(input[[paste("uploadtrack",i,sep="")]]==2){
              validate(
                need(input[[paste("uploadTrackfile",i,sep="")]], paste("Warning: Data are empty for Track",i,". Please upload applicable data.",sep=""))		  
              )
            }
          }		
          validate(
            need(ncol(data.C)==3 | (ncol(data.C)==5 && is.character(data.C[,4]) && is.character(data.C[,5])), "Error: Data formatting error! Chromosome data should contain three (general type) or five columns (cytoband type). Please upload applicable chromosome data.")
          )
          validate(
            need(all(is.numeric(data.C[,2]),is.numeric(data.C[,3])), "Error: Data formatting error! The second and third column of chromosome data should be numeric vectors.") 
          )	
          validate(
            need(sum(is.na(data.C[,2:3]))==0, "Error: Data formatting error! The chromosome data contains missing value.") 
          )		  
          if(input$datatypeChr!="general"){	 		  
            validate(
              need(ncol(data.C)==5, "Error: The chromosome data type should be 'General' based on the uploaded data.")
            )
          }
          if(!is.null(data.CN)){
            validate(
              need(ncol(data.CN)==4, "Error: Label data for the chromosome track should contain four columns.")
            )
            validate(
              need(all(is.numeric(data.CN[,2]),is.numeric(data.CN[,3])),"Error: The second and third column of label data for the chromosome track should be numeric vectors.")
            )
            validate(
              need(sum(is.na(data.CN[,2:3]))==0, "Error: Data formatting error! The label data for the chromosome track contains missing value.") 
            )				
          }	  
          if(!is.null(data.T)){   
            for(i in 1:length(data.T)){
              dt.TT <- data.T[[i]]		
              tptrack <- input[[paste("typeTrack",trackindx[i],sep="")]]
              validate(
                need(ncol(dt.TT)>=4, paste("Error: Data formatting error for Track",trackindx[i],"!"," Please upload applicable data.",sep=""))
              )			
              validate(
                need(all(is.numeric(dt.TT[,2]),is.numeric(dt.TT[,3])), paste("Error: Data formatting error for Track",trackindx[i],"!"," The second and third column should be numeric vectors.",sep=""))
              )				
              if(tptrack=="point"){
                validate(
                  need(is.numeric(dt.TT[,4]) | colnames(dt.TT)[4]=="stack", paste("Error: Data formatting error for Track",trackindx[i],"!"," The fourth column should be a numeric vector or a character vector named as 'stack'.",sep=""))
                )
                dt.TTT <- dt.TT[,-1][,which(colnames(dt.TT[,-1])!="color")]
                dt.TTT <- dt.TTT[,which(colnames(dt.TTT)!="stack")]		  
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )					
              }else if(tptrack=="line"){
                validate(
                  need(is.numeric(dt.TT[,4]) | colnames(dt.TT)[4]=="stack", paste("Error: Data formatting error for Track",trackindx[i],"!"," The fourth column should be a numeric vector or a character vector named as 'stack'.",sep="")),
                  need(!all(("pch" %in% colnames(dt.TT)) | ("cex" %in% colnames(dt.TT))), paste("Error: Data formatting error for Track",trackindx[i],"!"," It should not contain columns named as 'pch' or 'cex'.",sep=""))			   
                )
                dt.TTT <- dt.TT[,-1][,which(colnames(dt.TT[,-1])!="color")]
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )									
              }else if(tptrack=="bar"){
                validate(
                  need(ncol(dt.TT)<=5, paste("Warning: Please upload data with four or five columns, the fifth column of which should be named as 'color'. Otherwise, only the first four or five columns of the data would be used to create barplot for Track",trackindx[i],".",sep="")),
                  need(!all(("pch" %in% colnames(dt.TT)) | ("cex" %in% colnames(dt.TT))), paste("Error: Data formatting error for Track",trackindx[i],"!"," It should not contain columns named as 'pch' or 'cex'.",sep=""))			   			   
                )
                dt.TTT <- dt.TT[,-1][,which(colnames(dt.TT[,-1])!="color")]
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )				
              }else if(tptrack=="rect"){
                validate(
                  need(ncol(dt.TT)<=5, paste("Warning: Please upload data with four columns. Otherwise, only the first four columns of the data would be used to create the rect plot for Track",trackindx[i],".",sep="")),
                  need(ncol(dt.TT)==4, paste("Error: Please upload data with four columns for Track",trackindx[i],"."," The 4th column should be a numeric vactor or a character vactor.",sep="")),
                  need(!all(("pch" %in% colnames(dt.TT)) | ("cex" %in% colnames(dt.TT))), paste("Error: Data formatting error for Track",trackindx[i],"!"," It should not contain columns named as 'pch' or 'cex'.",sep=""))			   
                )
                if(!is.numeric(dt.TT[,4])){
                  dt.TTT <- dt.TT[,2:3]
                }else{
                  dt.TTT <- dt.TT[,-1]
                }
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )				
              }else if(tptrack=="heatmap"){
                colums <- lapply(dt.TT[,-(1:3)],function(x){is.numeric(x)})
                validate(
                  need(all(unlist(colums)), paste("Error: Data formatting error for Track",trackindx[i],"!"," Apart from the first column, other columns of the data to create heatmap should be numeric vectors.",sep="")),
                  need(!all(("pch" %in% colnames(dt.TT)) | ("cex" %in% colnames(dt.TT))), paste("Error: Data formatting error for Track",trackindx[i],"!"," It should not contain columns named as 'pch' or 'cex'.",sep=""))			   
                )
                dt.TTT <- dt.TT[,-1]
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )				
              }else if(tptrack=="ideogram"){
                validate(
                  need(ncol(dt.TT)==5, paste("Error: Data formatting error for Track",trackindx[i],"!"," Data to create ideogram should contain five columns. Please upload applicable data for Track",trackindx[i],"."," See example data 'chromosome_ideogram.csv' for more details.",sep="")),
                  need(!all(("pch" %in% colnames(dt.TT)) | ("cex" %in% colnames(dt.TT))), paste("Error: Data formatting error for Track",trackindx[i],"!"," It should not contain columns named as 'pch' or 'cex'.",sep=""))			   
                )
                dt.TTT <- dt.TT[,2:3]
                validate(
                  need(sum(is.na(dt.TTT))==0, paste("Error: Data formatting error for Track",trackindx[i],"!"," The track data contains missing value.",sep=""))
                )				
              }						
            }
          }
          if(!is.null(data.N)){
            for(k in 1:length(data.T)){
              dt.NN <- data.N[[k]]
              if(!is.null(dt.NN)){
                validate(
                  need(ncol(dt.NN)==4, paste("Error: Label data for Track",trackindx[k]," should contain four columns.",sep=""))			
                )
                validate(			
                  need(all(is.numeric(dt.NN[,2]),is.numeric(dt.NN[,3])), paste("Error: Label data formatting error for Track",trackindx[k],"!"," The second and third column should be numeric vectors.",sep=""))
                )
                validate(
                  need(sum(is.na(dt.NN[,2:3]))==0, paste("Error: Data formatting error for Track",trackindx[k],"!"," The label data for the chromosome track contains missing value.",sep=""))
                )					
              }
            }
          }			
          if(linksTrack.export && !is.null(linksFile.export)){
            validate(
              need(ncol(data.L)==6 | (ncol(data.L)==7 && colnames(data.L)[7]=="color"), "Error: Data formatting error! Data to create links between different genomic regions should be composed of 6 or 7 columns, the 7th column of which should be named as 'color'. Please upload applicable data.")
            )
            validate(
              need(all(is.numeric(data.L[,2]),is.numeric(data.L[,3]),is.numeric(data.L[,5]),is.numeric(data.L[,6])), "Error: Data formatting error! The second, third, fifth and sixth columns of the data to create links between different genomic regions should be numeric vectors. Please upload applicable data.")			
            )
            data.LL <- data.L[,c(2:3,5:6)]
            validate(
              need(sum(is.na(data.LL))==0, "Error: Data formatting error! The track data contains missing value.")
            )			
          }
          validate(
            need(is.null(data.C), "Please go to the 'Circos visualization' menu to generate the Circos plot!")
          )			  
        })
        outputOptions(output, "errorinfo1", suspendWhenHidden = FALSE)      
      }) 
    }else{NULL}
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
      if(input$clearText_buttonLinks == 0) return(NULL)
      isolate({updateTextInput(session, "hltDataLinks", label = ",", value = "")})
    })	
    ## *** Plot circos images *** 
    observe({	
      if(input$submit2>0){	  
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
            rectcoldiscusTrack <<- c()
            borderTrack <<- c()
			innergapTrack <<- c()
            gridsborderTrack <<- c()
            colgridsborderTrack <<- c()
            directionTrack <<- c()
            colorlineTrack <<- c()
            symbolTrack <<- c()
            pointsizeTrack <<- c()
            baselineTrack <<- c()
            colhmapTrack <<- c()
			heatmapcols <<- c()
			heatmapcol <<- c()			
            lineshmapTrack <<- c()
            heightlinesTrack <<- c()
            marginlinesTrack <<- c()
            heightTrack <<- c()
            marginTrack <<- c()
            bgcolTrack <<- c()
            typeTrack <<- c()
            coltypeTk <<- c()
            colorcusTrack <<- c()
            poslabels <<- c()
            labeltext <<- c()
            heightlabels <<- c()
            marginlabels <<- c()
            fillareaTrack <<- c()
            borderareaTrack <<- c()
            selreaTrack <<- c()
            for(k in 1:length(data.T)){
              data.TT <- data.T[[k]]
              ## *** The highlight regions ***
              assign("hltTrack",input[[paste("highlightTrack",trackindx[k],sep="")]])
              hltTrack.List[[k]] <<- hltTrack
              assign("hltdata",input[[paste("hltData",trackindx[k],sep="")]])
              hltdata.List[[k]] <<- hltdata
              hltregion.List[[k]] <<- ""
              if(nchar(input[[paste("hltData",trackindx[k],sep="")]])>0){
                tmp <- matrix(strsplit(hltdata, "\n")[[1]])
                myColnames <- c("chr","start","end","color")
                data <- matrix(0, length(tmp), length(myColnames))
                colnames(data) <- myColnames
                for(p in 1:length(tmp)){
                  myRow <- strsplit(tmp[p], ",")[[1]]					
                  if(length(myRow)==4){                                        
                    data[p,] <- myRow
                  }
                }			      
                data.ht <- as.data.frame(t(data),stringsAsFactors=F)
                data.ht1 <- unlist(lapply(data.ht,function(x){
                  all(x==0)
                })
                )
                output$errorinfo2 <- renderPrint({
                  for(f in 1:k){				
                    if(input[[paste("uploadtrack",trackindx[f],sep="")]]==2 && input[[paste("highlightTrack",trackindx[f],sep="")]]==1 && nchar(input[[paste("hltData",trackindx[f],sep="")]])>0){						
                      validate(
                        need(all(data.ht1==FALSE), paste("Error: Data to highlight regions for Track",trackindx[f]," is not in correct format. Each row of the input data should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color.",sep=""))		  
                      )
                    }
                  }
                })
                outputOptions(output, "errorinfo2", suspendWhenHidden = FALSE)				
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
              rectcoldiscusTrack <<- c(rectcoldiscusTrack,input[[paste("rectcoldiscusTrack",trackindx[k],sep="")]])
              borderTrack <<- c(borderTrack,input[[paste("borderTrack",trackindx[k],sep="")]])
			  innergapTrack <<- c(innergapTrack,input[[paste("innergapTrack",trackindx[k],sep="")]])
              gridsborderTrack <<- c(gridsborderTrack,input[[paste("gridsborderTrack",trackindx[k],sep="")]])
              colgridsborderTrack <<- c(colgridsborderTrack,input[[paste("colgridsborderTrack",trackindx[k],sep="")]])
              directionTrack <<- c(directionTrack,input[[paste("directionTrack",trackindx[k],sep="")]])
              colorlineTrack <<- c(colorlineTrack,input[[paste("colorlineTrack",trackindx[k],sep="")]])
              symbolTrack <<- c(symbolTrack,input[[paste("symbolTrack",trackindx[k],sep="")]])
              pointsizeTrack <<- c(pointsizeTrack,input[[paste("pointsizeTrack",trackindx[k],sep="")]])
              baselineTrack <<- c(baselineTrack,input[[paste("baselineTrack",trackindx[k],sep="")]])
              colhmapTrack <<- c(colhmapTrack,input[[paste("colhmapTrack",trackindx[k],sep="")]])              			  
              heatmapcols <<- c(heatmapcols,paste(input[[paste("lowColor",trackindx[k],sep="")]],input[[paste("midColor",trackindx[k],sep="")]],input[[paste("highColor",trackindx[k],sep="")]],sep="."))
			  heatmapcol <<- c(heatmapcol,input[[paste("heatmapcol",trackindx[k],sep="")]])
			  lineshmapTrack <<- c(lineshmapTrack,input[[paste("lineshmapTrack",trackindx[k],sep="")]])
              heightlinesTrack <<- c(heightlinesTrack,input[[paste("heightlinesTrack",trackindx[k],sep="")]])
              marginlinesTrack <<- c(marginlinesTrack,input[[paste("marginlinesTrack",trackindx[k],sep="")]])
              heightTrack <<- c(heightTrack,input[[paste("heightTrack",trackindx[k],sep="")]])
              marginTrack <<- c(marginTrack,input[[paste("marginTrack",trackindx[k],sep="")]])
              bgcolTrack <<- c(bgcolTrack,input[[paste("bgcolTrack",trackindx[k],sep="")]])
              typeTrack <<- c(typeTrack,input[[paste("typeTrack",trackindx[k],sep="")]])
              coltypeTk <<- c(coltypeTk,input[[paste("coltypeTrack",trackindx[k],sep="")]])
              colorcusTrack <<- c(colorcusTrack,input[[paste("colorcusTrack",trackindx[k],sep="")]])
              labeltext <<- c(labeltext,input[[paste("labels",trackindx[k],sep="")]])
              poslabels <<- c(poslabels,input[[paste("poslabels",trackindx[k],sep="")]])
              heightlabels <<- c(heightlabels,input[[paste("heightlabels",trackindx[k],sep="")]])
              marginlabels <<- c(marginlabels,input[[paste("marginlabels",trackindx[k],sep="")]])
              fillareaTrack <<- c(fillareaTrack,input[[paste("fillareaTrack",trackindx[k],sep="")]])
              borderareaTrack <<- c(borderareaTrack,input[[paste("borderareaTrack",trackindx[k],sep="")]])
              selreaTrack <<- c(selreaTrack,input[[paste("selreaTrack",trackindx[k],sep="")]])
              legendtext <<- c(legendtext,input[[paste("text",trackindx[k],sep="")]])
			}
          }
          labeltextchr <<- input$labels0
          poslabelschr <<- input$poslabels0
          heightlabelschr <<- input$heightlabels0
          marginlabelschr <<- input$marginlabels0
          transparencyhltLinks <<- input$transparencyhltLinks
          transparencyLinks <<- input$transparencyLinks
          selcolorLinks <<- input$selcolorLinks
		  colformatLinks <<- input$colformatLinks
          colorLinks <<- input$colorLinks
		  gracolinks <<- c(input$lowColinks,input$midColinks,input$highColinks)
          linksTrack <<- input$linksTrack
          marginLinks <<- input$marginLinks
          cexlabel <<- input$cexlabel
          unitChr <<- input$unitChr
		  outAxis <<- input$outAxis
          labelChr <<- input$labelChr
          fontSize <<- input$fontSize
          trackChr <<- input$trackChr
          datatypeChr <<- input$datatypeChr
		  heightChr <<- input$heightChr
          if(input$linksTrack)  {
            legendtext <<- c(legendtext,input$text11)
          }
          if(datatypeChr=="general" && trackChr!="track"){
            legendtext <<- legendtext
          }else{
            legendtext <<- c(input$text0,legendtext)
          }
          legendtext <<- legendtext[nchar(legendtext)>0]
          colorChr <<- gsub("\\s","",strsplit(input$colorChr,",")[[1]])
          gap.width <<- gsub("\\s","",strsplit(input$gapChr,",")[[1]])	
          plotfig(input = input, output = output, trackindx = trackindx, data.L = data.L, data.L1 = data.L1, data.L2 = data.L2, data.C = data.C, barBoundary = barBoundary, coldir1Track = coldir1Track, 
                  coldir2Track = coldir2Track, data.T = data.T, data.N = data.N, data.CN = data.CN, hltTrack.List = hltTrack.List, hltdata.List = hltdata.List, heightSize = heightSize, widthSize = widthSize, addlegend = addlegend, poslegend = poslegend, colorChr = colorChr, heightChr = heightChr,
                  gap.width = gap.width, legendtext = legendtext, labeltext = labeltext, poslabels = poslabels, heightlabels = heightlabels, marginlabels = marginlabels, fillareaTrack = fillareaTrack, cexlabel = cexlabel, unitChr = unitChr, outAxis = outAxis, labelChr = labelChr, fontSize = fontSize, colorTrack = colorTrack, borderareaTrack = borderareaTrack, selreaTrack = selreaTrack, transparencyHlt = transparencyHlt,
                  trackChr = trackChr, datatypeChr = datatypeChr, labeltextchr = labeltextchr, heightlabelschr = heightlabelschr, marginlabelschr = marginlabelschr, poslabelschr = poslabelschr, transparencyhltLinks = transparencyhltLinks, transparencyTrack = transparencyTrack, transparencyLinks = transparencyLinks,
                  colformatLinks = colformatLinks, colorLinks = colorLinks, gracolinks=gracolinks, linksTrack = linksTrack, typeTrack = typeTrack, coltypeTk = coltypeTk, colorcusTrack = colorcusTrack, marginLinks = marginLinks, selcolorLinks = selcolorLinks, colrectTrack = colrectTrack, rectTrack = rectTrack, rectcolTrack = rectcolTrack, rectcoldisTrack = rectcoldisTrack, rectcoldiscusTrack = rectcoldiscusTrack, 
                  borderTrack = borderTrack, innergapTrack = innergapTrack, gridsborderTrack = gridsborderTrack, colgridsborderTrack = colgridsborderTrack, directionTrack = directionTrack, colorlineTrack = colorlineTrack, symbolTrack = symbolTrack, pointsizeTrack = pointsizeTrack, baselineTrack = baselineTrack, colhmapTrack = colhmapTrack, heatmapcols = heatmapcols, heatmapcol = heatmapcol, lineshmapTrack = lineshmapTrack, heightlinesTrack = heightlinesTrack, marginlinesTrack = marginlinesTrack, heightTrack = heightTrack, marginTrack = marginTrack , bgcolTrack = bgcolTrack)
        })
      }else{NULL}
    })
    ## *** Download PDF file ***
    output$shinyCircos.pdf <- downloadHandler(
      filename <- function(){ paste('shinyCircos.pdf') },
      content <- function(file){
        pdf(file, width = input$myWidth/72, height = input$myHeight/72)
        print(figurecp)
        dev.off()
      }, contentType = 'application/pdf')	  
    ## *** Download SVG file ***
    output$shinyCircos.svg <- downloadHandler(
      filename <- function(){ paste('shinyCircos.svg') },
      content <- function(file){
        svg(file, width = input$myWidth/72, height = input$myHeight/72)
        print(figurecp)		 
        dev.off()
      }, contentType = 'image/svg')
    ## *** Download Source code file ***  
    output$script.R <- downloadHandler(
      filename <- function() { paste('script.R') },
      content <- function(file) {
        source("writeCmd.R")
        file.remove("script.R")	  
        writeLines(out,con=file)
      }, contentType = NULL) 	  
    ## *** Download sample data in csv format ***
    output$chromosome_general.csv <- downloadfile("example_data/chromosome_general.csv")
    output$chromosome_cytoband.csv <- downloadfile("example_data/chromosome_cytoband.csv")
    output$point.csv <- downloadfile("example_data/point.csv")
    output$line.csv <- downloadfile("example_data/line.csv")
    output$barplot.csv <- downloadfile("example_data/barplot.csv")
    output$heatmap.csv <- downloadfile("example_data/heatmap.csv")
    output$chromosome_ideogram.csv <- downloadfile("example_data/chromosome_ideogram.csv")
    output$rect_discrete.csv <- downloadfile("example_data/rect_discrete.csv")
    output$point_multicolumn.csv <- downloadfile("example_data/point_multicolumn.csv")
    output$barplot_bidirectional.csv <- downloadfile("example_data/barplot_bidirectional.csv")
    output$rect_gradual.csv <- downloadfile("example_data/rect_gradual.csv")
    output$line_multicolumn.csv <- downloadfile("example_data/line_multicolumn.csv")
    output$line_color.csv <- downloadfile("example_data/line_color.csv")
    output$barplot_color.csv <- downloadfile("example_data/barplot_color.csv")
    output$point_color.csv <- downloadfile("example_data/point_color.csv")
    output$point_pch.csv <- downloadfile("example_data/point_pch.csv")
    output$point_color_pch.csv <- downloadfile("example_data/point_color_pch.csv")
    output$point_cex.csv <- downloadfile("example_data/point_cex.csv")
    output$point_pch_cex.csv <- downloadfile("example_data/point_pch_cex.csv")
    output$point_color_cex.csv <- downloadfile("example_data/point_color_cex.csv")
    output$point_color_pch_cex.csv <- downloadfile("example_data/point_color_pch_cex.csv")		
    output$gene_label.csv <- downloadfile("example_data/gene_label.csv")
    output$stack_point.csv <- downloadfile("example_data/stack_point.csv")
    output$stack_line.csv <- downloadfile("example_data/stack_line.csv")
    output$links.csv <- downloadfile("example_data/links.csv")
    output$links_discrete_color.csv <- downloadfile("example_data/links_discrete_color.csv")
    output$links_gradual_color.csv <- downloadfile("example_data/links_gradual_color.csv")	
  })
})


