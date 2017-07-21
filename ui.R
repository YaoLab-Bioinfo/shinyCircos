shinyUI(fluidPage(theme="newstyle.css",
                  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
	titlePanel(HTML('<p><font size="6">shinyCircos: an R/Shiny application for generation of circos plots interavtively</font></p>'),
	## *** Add some custom CSS; customize the look of the header ***
		tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
		tags$style(type="text/css", "select { max-width: 200px; }"),
		tags$style(type="text/css", "textarea { max-width: 185px; }"),
		tags$style(type="text/css", ".jslider { max-width: 200px; }"),
		tags$style(type='text/css', ".well { max-width: 330px; }"),
		tags$style(type='text/css', ".span4 { max-width: 330px; }")
		) 
	),
	sidebarLayout(
  	sidebarPanel(
	    ## *** About panel***
		conditionalPanel(condition="input.tabs1=='About'",
			h4("About")
		),
		## *** Data upload panel***
		conditionalPanel(condition="input.tabs1=='Data upload'",
				fileInput("uploadChr", HTML("<table><tr><td><strong>Upload chromosomes data</strong></td>
<td>
<div class='help-tip'>
	<p>Upload chromosomes data to plot the outermost track which is a compulsory part of a circos plot.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
				radioButtons("datatypeChr", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>Chromosomes data can be either general data with three columns or cytoband data with five columns. 
     The first three columns of either type of data should be the chromosome ID, 
      the start and end coordinates of different genomic regions. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("General" = "general", "Cytoband" = "")),
				HTML('<br>'),
				HTML("<table><tr><td><strong>Upload data for inner tracks</strong></td>
<td>
<div class='help-tip'>
	<p>Data of zero or more tracks can be uploaded. For now, a maximum of 10 tracks are allowed. For any track, the first three columns of the uploaded data should be chromosome ID, the start and end coordinates of genomic regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
				checkboxInput("seltrack1", HTML("<font color='red'>Track1</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack1",
				radioButtons("uploadtrack1", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack1 == '2'",
				fileInput("uploadTrackfile1", "Upload track1 data:", multiple = FALSE),
				selectInput("typeTrack1", "plot type:", choices = c("point", "line", "bar", "rect"), selected="point")
				)
				),
				checkboxInput("seltrack2", HTML("<font color='red'>Track2</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack2",
				radioButtons("uploadtrack2", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack2 == '2'",
				fileInput("uploadTrackfile2", "Upload track2 data:", multiple = FALSE),
				selectInput("typeTrack2", "plot type:", choices = c("point", "line", "bar", "rect"), selected="line")
				)
				),
				checkboxInput("seltrack3", HTML("<font color='red'>Track3</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack3",
				radioButtons("uploadtrack3", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack3 == '2'",
				fileInput("uploadTrackfile3", "Upload track3 data:", multiple = FALSE),
				selectInput("typeTrack3", "plot type:", choices = c("point", "line", "bar", "rect"), selected="bar")
				)
				),
				checkboxInput("seltrack4", HTML("<font color='red'>Track4</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack4",
				radioButtons("uploadtrack4", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack4 == '2'",
				fileInput("uploadTrackfile4", "Upload track4 data:", multiple = FALSE),
				selectInput("typeTrack4", "plot type:", choices = c("point", "line", "bar", "rect"), selected="bar")
				)
				),
				checkboxInput("seltrack5", HTML("<font color='red'>Track5</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack5",
				radioButtons("uploadtrack5", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack5 == '2'",
				fileInput("uploadTrackfile5", "Upload track5 data:", multiple = FALSE),
				selectInput("typeTrack5", "plot type:", choices = c("point", "line", "bar", "rect"), selected="rect")
				)
				),
				checkboxInput("seltrack6", HTML("<font color='red'>Track6</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack6",
				radioButtons("uploadtrack6", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack6 == '2'",
				fileInput("uploadTrackfile6", "Upload track6 data:", multiple = FALSE),
				selectInput("typeTrack6", "plot type:", choices = c("point", "line", "bar", "rect"), selected="point")
				)
				),
				checkboxInput("seltrack7", HTML("<font color='red'>Track7</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack7",
				radioButtons("uploadtrack7", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack7 == '2'",
				fileInput("uploadTrackfile7", "Upload track7 data:", multiple = FALSE),
				selectInput("typeTrack7", "plot type:", choices = c("point", "line", "bar", "rect"), selected="bar")
				)
				),
				checkboxInput("seltrack8", HTML("<font color='red'>Track8</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack8",
				radioButtons("uploadtrack8", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack8 == '2'",
				fileInput("uploadTrackfile8", "Upload track8 data:", multiple = FALSE),
				selectInput("typeTrack8", "plot type:", choices = c("point", "line", "bar", "rect"), selected="bar")
				)
				),
				checkboxInput("seltrack9", HTML("<font color='red'>Track9</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack9",
				radioButtons("uploadtrack9", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack9 == '2'",
				fileInput("uploadTrackfile9", "Upload track9 data:", multiple = FALSE),
				selectInput("typeTrack9", "plot type:", choices = c("point", "line", "bar", "rect"), selected="rect")
				)
				),
				checkboxInput("seltrack10", HTML("<font color='red'>Track10</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack10",
				radioButtons("uploadtrack10", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack10 == '2'",
				fileInput("uploadTrackfile10", "Upload track10 data:", multiple = FALSE),
				selectInput("typeTrack10", "plot type:", choices = c("point", "line", "bar", "rect"), selected="line")
			    )
				),	
				HTML('<br>'),
				HTML("<table><tr><td><strong>Upload data to create links</strong></td>
<td>
<div class='help-tip'>
	<p>Data to create links between different genomic regions should be composed of 6 columns. The first three columns of each row
     represent the coordinate of a genomic region while the last three columns of each row represent the coordinate of another
     genomic region. A link will be created between the two genomic regions in each row. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
				checkboxInput("linksTrack", "Links data", FALSE),
				conditionalPanel(condition="input.linksTrack",
				fileInput("linksFile", "Upload region data:", multiple = FALSE)
				),
				HTML('<br>'),
				HTML('<p>All uploaded data should bes in .csv format.</p>'),
			HTML('<br>'),
			actionButton("submit1", HTML("<table><tr><td><strong>Go!</strong></td>
<td>
<div class='help-tip'>
	<p>Whenever the data are updated, please click 'Go!'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), width="49px")			
		),
		## *** Circos visualization panel***
		conditionalPanel(condition="input.tabs1=='Circos visualization'",
			h4("Plot options"),
			checkboxInput("optionsChr", HTML("<font color='red'>Chromosome</font>"), FALSE),
			conditionalPanel(condition="input.optionsChr",
			conditionalPanel(condition="input.datatypeChr=='general'",							  
			radioButtons("trackChr", "Chromosome band", c("Show" = "track", "Hide" = "")),
			conditionalPanel(condition="input.trackChr=='track'",
			textInput("colorChr", HTML("<table><tr><td><strong>color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for each chromosome/sector. Character vector of arbitrary length representing colors is accepted and adjusted automatically to the number of sectors. For example, 'grey' or 'grey,red,green,blue'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")    
			)
			),
			radioButtons("labelChr", "Chromosome IDs", c("Show" = "labels", "Hide" = "axis")),
			radioButtons("unitChr", "Size Units of genomic regions", c("Show" = "unit", "Hide" = "")),
			textInput("gapChr", HTML("<table><tr><td><strong>Gap width(s):</strong></td>
<td>
<div class='help-tip'>
	<p>Gaps between neighbouring sectors. Numeric vector of arbitrary length is accepted and adjusted automatically to the number of sectors. 
   For example, '1' or '1,2,3,1'. The first value corresponds to the gap between the first and the second sector.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="1"),          
			radioButtons("fontsizeChr", "Font size", c("Small" = "1", "Medium" = "1.1","Large"="1.2","Custom"="custom")),
			conditionalPanel(condition="input.fontsizeChr=='custom'",
			numericInput("cexAxis", "Axis text size:", value=1, min=0.1, max=3, step=0.1),
			conditionalPanel(condition="input.labelChr=='labels'",
			numericInput("cexAxislabel", "Axis label size:", value=1, min=0.1, max=3, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack1", HTML("<font color='red'>Track1</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack1 & input.uploadtrack1 == '2'",
			conditionalPanel(condition="input.typeTrack1=='bar'",
			radioButtons("directionTrack1", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be unidirectional or bidirectional. For bidirectional plot,  
the fourth column of the uploaded data will be divided into two groups based on the boundary value specified by the user.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack1=='2'",
			numericInput("barBoundary1", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track1", "Outside color:", value="red"),
			textInput("coldir2Track1", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack1!='rect'",
			conditionalPanel(condition="input.typeTrack1!='bar' | input.directionTrack1=='1'",
			radioButtons("coltypeTrack1", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack1=='2'",	
			textInput("colorTrack1", NULL, value="darkorange")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack1=='rect'",
			radioButtons("rectTrack1", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack1=='1'",
			selectInput("colrectTrack1", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack1", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack1!='rect'",
			textInput("colorlineTrack1", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack1", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>Background color for the plotting regions. It can be null or a single value or a vector. 
If it is a vector, colors of arbitrary length can be accepted and adjusted automatically to the same length of sectors. 
For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack1", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack1", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack1", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack1!='rect' & output.trackdat1",
			radioButtons("highlightTrack1", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack1==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. 
For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData1", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button1','Clear data'),
			numericInput("transparencyHlt1", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack2", HTML("<font color='red'>Track2</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack2 & input.uploadtrack2 == '2'",
			conditionalPanel(condition="input.typeTrack2=='bar'",
			radioButtons("directionTrack2", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be unidirectional or bidirectional. Bidirectional plot can be used only when track's data 
includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack2=='2'",
			numericInput("barBoundary2", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track2", "Outside color:", value="red"),
			textInput("coldir2Track2", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack2!='rect'",
			conditionalPanel(condition="input.typeTrack2!='bar' | input.directionTrack2=='1'",
			radioButtons("coltypeTrack2", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack2=='2'",	
			textInput("colorTrack2", NULL, value="blue")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack2=='rect'",
			radioButtons("rectTrack2", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack2=='1'",
			selectInput("colrectTrack2", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack2", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack2!='rect'",
			textInput("colorlineTrack2", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack2", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack2", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack2", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack2", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack2!='rect' & output.trackdat2",
			radioButtons("highlightTrack2", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack2==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData2", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button2','Clear data'),
			numericInput("transparencyHlt2", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack3", HTML("<font color='red'>Track3</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack3 & input.uploadtrack3 == '2'",
			conditionalPanel(condition="input.typeTrack3=='bar'",
			radioButtons("directionTrack3", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack3=='2'",
			numericInput("barBoundary3", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track3", "Outside color:", value="red"),
			textInput("coldir2Track3", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack3!='rect'",
			conditionalPanel(condition="input.typeTrack3!='bar' | input.directionTrack3=='1'",
			radioButtons("coltypeTrack3", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack3=='2'",	
			textInput("colorTrack3", NULL, value="cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack3=='rect'",
			radioButtons("rectTrack3", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack3=='1'",
			selectInput("colrectTrack3", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack3", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack3!='rect'",
			textInput("colorlineTrack3", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack3", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack3", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack3", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack3", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack3!='rect' & output.trackdat3",
			radioButtons("highlightTrack3", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack3==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData3", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button3','Clear data'),
			numericInput("transparencyHlt3", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack4", HTML("<font color='red'>Track4</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack4 & input.uploadtrack4 == '2'",
			conditionalPanel(condition="input.typeTrack4=='bar'",
			radioButtons("directionTrack4", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack4=='2'",
			numericInput("barBoundary4", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track4", "Outside color:", value="red"),
			textInput("coldir2Track4", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack4!='rect'",
			conditionalPanel(condition="input.typeTrack4!='bar' | input.directionTrack4=='1'",
			radioButtons("coltypeTrack4", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack4=='2'",	
			textInput("colorTrack4", NULL, value="yellow")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack4=='rect'",
			radioButtons("rectTrack4", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack4=='1'",
			selectInput("colrectTrack4", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack4", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack4!='rect'",
			textInput("colorlineTrack4", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack4", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. 
For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack4", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack4", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack4", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack4!='rect' & output.trackdat4",
			radioButtons("highlightTrack4", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Input genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack4==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four columns separated by commas indicating the chromosome ID, start coordinate, end coordinate and specified color. For example, 'Chr1,1,100000000,red'. Hex color codes like '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData4", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button4','Clear data'),
			numericInput("transparencyHlt4", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack5", HTML("<font color='red'>Track5</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack5 & input.uploadtrack5 == '2'",
			conditionalPanel(condition="input.typeTrack5=='bar'",
			radioButtons("directionTrack5", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack5=='2'",
			numericInput("barBoundary5", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track5", "Outside color:", value="red"),
			textInput("coldir2Track5", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack5!='rect'",
			conditionalPanel(condition="input.typeTrack5!='bar' | input.directionTrack5=='1'",
			radioButtons("coltypeTrack5", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack5=='2'",	
			textInput("colorTrack5", NULL, value="purple")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack5=='rect'",
			radioButtons("rectTrack5", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="2"),
			conditionalPanel(condition="input.rectTrack5=='1'",
			selectInput("colrectTrack5", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack5", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack5!='rect'",
			textInput("colorlineTrack5", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack5", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack5", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack5", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack5", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack5!='rect' & output.trackdat5",
			radioButtons("highlightTrack5", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack5==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData5", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button5','Clear data'),
			numericInput("transparencyHlt5", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack6", HTML("<font color='red'>Track6</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack6 & input.uploadtrack6 == '2'",
			conditionalPanel(condition="input.typeTrack6=='bar'",
			radioButtons("directionTrack6", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack6=='2'",
			numericInput("barBoundary6", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track6", "Outside color:", value="red"),
			textInput("coldir2Track6", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack6!='rect'",
			conditionalPanel(condition="input.typeTrack6!='bar' | input.directionTrack6=='1'",
			radioButtons("coltypeTrack6", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack6=='2'",	
			textInput("colorTrack6", NULL, value="red")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack6=='rect'",
			radioButtons("rectTrack6", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack6=='1'",
			selectInput("colrectTrack6", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack6", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack6!='rect'",
			textInput("colorlineTrack6", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack6", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack6", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack6", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack6", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack6!='rect' & output.trackdat6",
			radioButtons("highlightTrack6", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack6==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData6", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button6','Clear data'),
			numericInput("transparencyHlt6", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack7", HTML("<font color='red'>Track7</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack7 & input.uploadtrack7 == '2'",
			conditionalPanel(condition="input.typeTrack7=='bar'",
			radioButtons("directionTrack7", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack7=='2'",
			numericInput("barBoundary7", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track7", "Outside color:", value="deeppink"),
			textInput("coldir2Track7", "Inside color:", value="mediumblue")
			)
			),
			conditionalPanel(condition="input.typeTrack7!='rect'",
			conditionalPanel(condition="input.typeTrack7!='bar' | input.directionTrack7=='1'",
			radioButtons("coltypeTrack7", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack7=='2'",	
			textInput("colorTrack7", NULL, value="seagreen")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack7=='rect'",
			radioButtons("rectTrack7", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack7=='1'",
			selectInput("colrectTrack7", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack7", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack7!='rect'",
			textInput("colorlineTrack7", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack7", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack7", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack7", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack7", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack7!='rect' & output.trackdat7",
			radioButtons("highlightTrack7", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack7==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData7", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button7','Clear data'),
			numericInput("transparencyHlt7", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack8", HTML("<font color='red'>Track8</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack8 & input.uploadtrack8 == '2'",
			conditionalPanel(condition="input.typeTrack8=='bar'",
			radioButtons("directionTrack8", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack8=='2'",
			numericInput("barBoundary8", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track8", "Outside color:", value="red"),
			textInput("coldir2Track8", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack8!='rect'",
			conditionalPanel(condition="input.typeTrack8!='bar' | input.directionTrack8=='1'",
			radioButtons("coltypeTrack8", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack8=='2'",	
			textInput("colorTrack8", NULL, value="gold")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack8=='rect'",
			radioButtons("rectTrack8", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack8=='1'",
			selectInput("colrectTrack8", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack8", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack8!='rect'",
			textInput("colorlineTrack8", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack8", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack8", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack8", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack8", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack8!='rect' & output.trackdat8",
			radioButtons("highlightTrack8", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack8==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData8", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button8','Clear data'),
			numericInput("transparencyHlt8", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack9", HTML("<font color='red'>Track9</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack9 & input.uploadtrack9 == '2'",
			conditionalPanel(condition="input.typeTrack9=='bar'",
			radioButtons("directionTrack9", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack9=='2'",
			numericInput("barBoundary9", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track9", "Outside color:", value="red"),
			textInput("coldir2Track9", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack9!='rect'",
			conditionalPanel(condition="input.typeTrack9!='bar' | input.directionTrack9=='1'",
			radioButtons("coltypeTrack9", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack9=='2'",	
			textInput("colorTrack9", NULL, value="olivedrab")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack9=='rect'",
			radioButtons("rectTrack9", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack9=='1'",
			selectInput("colrectTrack9", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack9", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack9!='rect'",
			textInput("colorlineTrack9", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack9", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack9", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack9", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack9", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack9!='rect' & output.trackdat9",
			radioButtons("highlightTrack9", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack9==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData9", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button9','Clear data'),
			numericInput("transparencyHlt9", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("optionsTrack10", HTML("<font color='red'>Track10</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack10 & input.uploadtrack10 == '2'",
			conditionalPanel(condition="input.typeTrack10=='bar'",
			radioButtons("directionTrack10", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be shown in unidirectional or bidirectional type. Bidirectional type can be used only when track's data includes four columns and data will be divided into two groups by boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack10=='2'",
			numericInput("barBoundary10", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track10", "Outside color:", value="red"),
			textInput("coldir2Track10", "Inside color:", value="cyan")
			)
			),
			conditionalPanel(condition="input.typeTrack10!='rect'",
			conditionalPanel(condition="input.typeTrack10!='bar' | input.directionTrack10=='1'",
			radioButtons("coltypeTrack10", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users.
For data with four columns, a single character representing a color should be provided. For data with five columns, a character vector containing 2 elements representing two different colors should be provided. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack10=='2'",	
			textInput("colorTrack10", NULL, value="pink")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack10=='rect'",
			radioButtons("rectTrack10", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with discrete or gradual colors. For discrete data, the fourth column should be a discrete character vector with no more than 50 groups. For gradual data, the fourth column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack10=='1'",
			selectInput("colrectTrack10", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			numericInput("transparencyTrack10", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1),
			conditionalPanel(condition="input.typeTrack10!='rect'",
			textInput("colorlineTrack10", HTML("<table><tr><td><strong>Baselines color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a single character representing a color.  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			textInput("bgcolTrack10", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95"),
			numericInput("heightTrack10", HTML("<table><tr><td><strong>Track height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginTrack10", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("borderTrack10", "Add borders", c("Show" = "add", "Hide" = ""),selected=""),
			conditionalPanel(condition="input.typeTrack10!='rect' & output.trackdat10",
			radioButtons("highlightTrack10", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack10==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData10", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button10','Clear data'),
			numericInput("transparencyHlt10", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			conditionalPanel(condition="input.linksTrack",
			checkboxInput("optionsLinks", HTML("<font color='red'>Links</font>"), FALSE),
			conditionalPanel(condition="input.optionsLinks",
			radioButtons("colorLinks", HTML("<table><tr><td>Data color</td>
<td>
<div class='help-tip'>
	<p>Links are filled with random color assigned by the system or colors specified by the user as 'red' or '#FF0000'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.colorLinks==2",
			textInput("selcolorLinks", NULL, value="yellowgreen")
			),
			numericInput("transparencyLinks", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.5, min=0, max=1, step=0.1),
			numericInput("marginLinks", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			radioButtons("highlightLinks", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightLinks==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'Chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltDataLinks", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_buttonLinks','Clear data'),
			numericInput("transparencyhltLinks", HTML("<table><tr><td>color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			)
			)
			),
			checkboxInput("plotSize", HTML("<font color='red'>Adjust plot size</font>"), FALSE),
			conditionalPanel(condition="input.plotSize",
				numericInput("myHeight", "Plot height:", value=750),
				numericInput("myWidth", "Plot width:", value=750)
			),
			HTML('<br>'),
 		  	actionButton("submit2", HTML("<table><tr><td><strong>Go!</strong></td>
<td>
<div class='help-tip'>
	<p>Whenever any option is updated, please click 'Go!'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), width="49px")
		),
		conditionalPanel(condition="input.tabs1=='Help'",
		                 h4("Help")
		)
	),
	mainPanel(
		tabsetPanel(
			## *** Welcome panel ***
			tabPanel("About",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Software references</font></li></ul></p>'),
				HTML('<p>1. R Development Core Team. <i><a href="http://www.r-project.org/">R</a>:  A Language and Environment for Statistical Computing.</i> R Foundation for Statistical Computing, Vienna (2016) <br>
				2. RStudio and Inc. <i><a href="http://www.rstudio.com/shiny/">shiny</a>: Web Application Framework for R.</i> R package version 0.13.2 (2016) <br>
				3. Gu, Z. <i><a href="http://cran.r-project.org/web/packages/circlize/index.html">circlize</a>: Circular Visualization.</i> R package version 0.3.10 (2017) <br>
				4. Neuwirth, E. <i><a href="http://cran.r-project.org/web/packages/RColorBrewer/index.html">RColorBrewer</a>: ColorBrewer palettes.</i> R package version 1.1-2 (2014) <br>
				5. Lawrence, M. <i><a href="http://bioconductor.org/packages/GenomicRanges/">GenomicRanges</a>: Representation and manipulation of genomic intervals and variables defined along a genome.</i> R package version 1.24.3 (2016) <br>
				6. R Core Team and contributors worldwide. <i><a href="http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/00Index.html">grDevices</a>: Graphics devices and support for base and grid graphics.</i> R package version 3.3.1 (2016) <br></p>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Further references</font></li></ul></p>'),
				h6("This application was created by ", a("Wen Yao", href="https://www.researchgate.net/profile/Wen_Yao"), " and ", a("Yiming Yu", href="https://www.researchgate.net/profile/Yiming_Yu4"), 
				" . Please send bugs and feature requests to Wen Yao (ywhzau at gmail.com) or Yiming Yu (yimingyyu at gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
			),
			## *** Data upload panel ***
			tabPanel("Data upload",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Download example data</font></li></ul></p>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example chromosomes data</font></li></ul></p>'),
			    downloadButton("downloadgeneralchrData", "Download general data"),
				downloadButton("downloadcytobandData", "Download cytoband data"),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example tracks data</font></li></ul></p>'),
				downloadButton("downloadtrackData1", "Download track1 data"),
				downloadButton("downloadtrackData2", "Download track2 data"),
				downloadButton("downloadtrackData3", "Download track3 data"),
				downloadButton("downloadtrackData4", "Download track4 data"),
				downloadButton("downloadtrackData5", "Download track5 data"),
				downloadButton("downloadtrackData6", "Download track6 data"),
				downloadButton("downloadtrackData7", "Download track7 data"),
				downloadButton("downloadtrackData8", "Download track8 data"),
				downloadButton("downloadtrackData9", "Download track9 data"),
				downloadButton("downloadtrackData10", "Download track10 data"),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example links data</font></li></ul></p>'),				
				downloadButton("downloadlinkData", "Download links data"),
				HTML('<br>'),
				HTML('<br>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Glimpse of data uploaded</font></li></ul></p>'),
				conditionalPanel(condition="input.submit1>0",
				conditionalPanel(condition="output.chrdat",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Chromosomes data</font></li></ul></p>'),				
				tableOutput("viewChr")
				),
				conditionalPanel(condition="input.uploadtrack1 == '2' | input.uploadtrack2 == '2' | input.uploadtrack3 == '2' | input.uploadtrack4 == '2' | input.uploadtrack5 == '2' | input.uploadtrack6 == '2' | input.uploadtrack7 == '2' | input.uploadtrack8 == '2' | input.uploadtrack9 == '2' | input.uploadtrack10 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Tracks data</font></li></ul></p>'),				
				conditionalPanel(condition="input.uploadtrack1 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track1</font></li></ul></p>'),
				tableOutput("viewTrack1")
				),
				conditionalPanel(condition="input.uploadtrack2 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track2</font></li></ul></p>'),				
				tableOutput("viewTrack2")
				),
				conditionalPanel(condition="input.uploadtrack3 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track3</font></li></ul></p>'),				
				tableOutput("viewTrack3")
				),
				conditionalPanel(condition="input.uploadtrack4 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track4</font></li></ul></p>'),				
				tableOutput("viewTrack4")
				),
				conditionalPanel(condition="input.uploadtrack5 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track5</font></li></ul></p>'),				
				tableOutput("viewTrack5")
				),
				conditionalPanel(condition="input.uploadtrack6 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track6</font></li></ul></p>'),				
				tableOutput("viewTrack6")
				),
				conditionalPanel(condition="input.uploadtrack7 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track7</font></li></ul></p>'),				
				tableOutput("viewTrack7")
				),
				conditionalPanel(condition="input.uploadtrack8 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track8</font></li></ul></p>'),				
				tableOutput("viewTrack8")
				),
				conditionalPanel(condition="input.uploadtrack9 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track9</font></li></ul></p>'),				
				tableOutput("viewTrack9")
				),
				conditionalPanel(condition="input.uploadtrack10 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track10</font></li></ul></p>'),				
				tableOutput("viewTrack10")
				)
				),
				conditionalPanel(condition="input.linksTrack > 0",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Links data</font></li></ul></p>'),				
				tableOutput("viewLink")
				)
				)
			),
			## *** Circos visualization panel ***
			tabPanel("Circos visualization", 
				downloadButton("downloadPlotPDF", "Download pdf-file"),
				downloadButton("downloadPlotSVG", "Download svg-file"),
				plotOutput("circosfigure", height='100%', width='100%')
			),
			## *** FAQ panel***
			tabPanel("Help",
				includeHTML("README.html")
			),			
			id="tabs1"
		)
	),position="left"    
))
)


