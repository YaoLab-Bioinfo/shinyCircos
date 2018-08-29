options(warn=-1)
library(RLumShiny)

shinyUI(fluidPage(theme="newstyle.css",
                  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
	titlePanel(HTML('<p><font size="6">shinyCircos: an R/Shiny application for interactive creation of Circos plot</font></p>'),
	## *** Add some custom CSS; customize the look of the header ***
		tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
		tags$style(type="text/css", "select { max-width: 200px; }"),
		tags$style(type="text/css", "textarea { max-width: 185px; }"),
		tags$style(type="text/css", ".jslider { max-width: 200px; }"),
		tags$style(type='text/css', ".well { max-width: 330px; }"),
		tags$style(type='text/css', ".span4 { max-width: 330px; }"),
		tags$style(HTML(".shiny-output-error-validation {color: red;}"))
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
				fileInput("uploadChr", HTML("<table><tr><td><strong>Upload chromosome data</strong></td>
<td>
<div class='help-tip'>
	<p>Upload chromosome data for the outermost track which is a compulsory part of a circos plot.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
				tags$script('$( "#uploadChr" ).on( "click", function() { this.value = null; });'),
				radioButtons("datatypeChr", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>Chromosomes data can be either general data with three columns or cytoband data with five columns. 
     The first three columns of either type of data should be the chromosome ID, 
      the start and end coordinates of different genomic regions. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("General" = "general", "Cytoband" = "")),
				fileInput("markTrackfile0", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
				tags$script('$( "#markTrackfile0" ).on( "click", function() { this.value = null; });'),										  
				HTML("<table><tr><td><strong>Upload data for inner tracks</strong></td>
<td>
<div class='help-tip'>
	<p>Data of zero or more tracks can be uploaded. For now, a maximum of 10 tracks are allowed. 
For any track, the first three columns of the uploaded data should be chromosome ID, the start 
and end coordinates of genomic regions. For plot of point, line and bar, the uploaded data can
contain one or more columns to indicate the 'color', 'cex' or 'pch' used in the plot. 
'cex' is used to control the size of data points while 'pch' is used to specify the
symbols used in plot of point. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
				checkboxInput("seltrack1", HTML("<font color='red'>Track1</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack1",
				radioButtons("uploadtrack1", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack1 == '2'",
				fileInput("uploadTrackfile1", "Upload track1 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile1" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack1", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="point"),
				fileInput("markTrackfile1", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile1" ).on( "click", function() { this.value = null; });')										  
				)
				),
				checkboxInput("seltrack2", HTML("<font color='red'>Track2</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack2",
				radioButtons("uploadtrack2", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack2 == '2'",
				fileInput("uploadTrackfile2", "Upload track2 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile2" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack2", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="line"),
				fileInput("markTrackfile2", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile2" ).on( "click", function() { this.value = null; });')										  
				)
				),
				checkboxInput("seltrack3", HTML("<font color='red'>Track3</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack3",
				radioButtons("uploadtrack3", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack3 == '2'",
				fileInput("uploadTrackfile3", "Upload track3 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile3" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack3", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="bar"),
				fileInput("markTrackfile3", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile3" ).on( "click", function() { this.value = null; });')										  			
				)
				),
				checkboxInput("seltrack4", HTML("<font color='red'>Track4</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack4",
				radioButtons("uploadtrack4", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack4 == '2'",
				fileInput("uploadTrackfile4", "Upload track4 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile4" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack4", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="heatmap"),
				fileInput("markTrackfile4", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile4" ).on( "click", function() { this.value = null; });')										  			
				)
				),
				checkboxInput("seltrack5", HTML("<font color='red'>Track5</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack5",
				radioButtons("uploadtrack5", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack5 == '2'",
				fileInput("uploadTrackfile5", "Upload track5 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile5" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack5", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="bar"),
				fileInput("markTrackfile5", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile5" ).on( "click", function() { this.value = null; });')										  				
				)
				),
				checkboxInput("seltrack6", HTML("<font color='red'>Track6</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack6",
				radioButtons("uploadtrack6", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack6 == '2'",
				fileInput("uploadTrackfile6", "Upload track6 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile6" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack6", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="rect"),
				fileInput("markTrackfile6", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile6" ).on( "click", function() { this.value = null; });')										  				
				)
				),
				checkboxInput("seltrack7", HTML("<font color='red'>Track7</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack7",
				radioButtons("uploadtrack7", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack7 == '2'",
				fileInput("uploadTrackfile7", "Upload track7 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile7" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack7", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="point"),
				fileInput("markTrackfile7", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile7" ).on( "click", function() { this.value = null; });')										  				
				)
				),
				checkboxInput("seltrack8", HTML("<font color='red'>Track8</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack8",
				radioButtons("uploadtrack8", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack8 == '2'",
				fileInput("uploadTrackfile8", "Upload track8 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile8" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack8", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="bar"),
				fileInput("markTrackfile8", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile8" ).on( "click", function() { this.value = null; });')										  				
				)
				),
				checkboxInput("seltrack9", HTML("<font color='red'>Track9</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack9",
				radioButtons("uploadtrack9", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack9 == '2'",
				fileInput("uploadTrackfile9", "Upload track9 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile9" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack9", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="rect"),
				fileInput("markTrackfile9", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
                tags$script('$( "#markTrackfile9" ).on( "click", function() { this.value = null; });')										  				
				)
				),
				checkboxInput("seltrack10", HTML("<font color='red'>Track10</font>"), FALSE),
			    conditionalPanel(condition="input.seltrack10",
				radioButtons("uploadtrack10", NULL, c("NULL" = "1", "Upload" = "2"), "1"),
				conditionalPanel(condition="input.uploadtrack10 == '2'",
				fileInput("uploadTrackfile10", "Upload track10 data:", multiple = FALSE),
				tags$script('$( "#uploadTrackfile10" ).on( "click", function() { this.value = null; });'),
				selectInput("typeTrack10", "Plot type:", choices = c("point", "line", "bar", "rect", "heatmap", "ideogram"), selected="line"),
				fileInput("markTrackfile10", HTML("<table><tr><td><strong>Upload label data</strong></td>
<td>
<div class='help-tip'>
	<p>Label data are used to mark genes or genomic regions which is a optional part of a circos plot. The four columns of label data should be chromosome ID, 
start coordinate, end coordinate and label text. Label text can be numbers or character strings. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), multiple = FALSE),
				tags$script('$( "#markTrackfile10" ).on( "click", function() { this.value = null; });')				
			    )
				),	
				HTML('<br>'),
				HTML("<table><tr><td><strong>Upload data to create links</strong></td>
<td>
<div class='help-tip'>
	<p>Data to create links between different genomic regions should be composed of 6 or 7 columns. 
The first three columns of each row represent the coordinate of a genomic region 
	while the 4th to 6th columns of each row represent the coordinate of another genomic region. 
For data with 7 columns, the 7th column should be categorical
	characters indicating varying colors or numbers indicating gradual colors used for different links. The name of the 7th column should be 'color'. 
A link will be created between the two genomic regions in each row. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
				checkboxInput("linksTrack", "Links data", FALSE),
				conditionalPanel(condition="input.linksTrack",
				fileInput("linksFile", "Upload region data:", multiple = FALSE),
				tags$script('$( "#linksFile" ).on( "click", function() { this.value = null; });')
				),
				HTML('<br>'),
				HTML('<p>Data uploaded should use any of the separator in the set [,\\t |;:].</p>'),
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
			textInput("colorChr", HTML("<table><tr><td><strong>Color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for each chromosome/sector. Character vector of arbitrary length representing 
colors is accepted and adjusted automatically to the number of sectors. 
For example, 'grey' or 'grey,red,green,blue'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")    
			)
			),
			radioButtons("labelChr", "Chromosome IDs", c("Show" = "labels", "Hide" = "axis")),
			radioButtons("unitChr", "Size units of genomic regions", c("Show" = "unit", "Hide" = "")),
			textInput("gapChr", HTML("<table><tr><td><strong>Gap width(s):</strong></td>
<td>
<div class='help-tip'>
	<p>Gaps between neighbouring sectors. Numeric vector of arbitrary length is accepted and 
adjusted automatically to the number of sectors. For example, '1' or '1,2,3,1'. The first 
value corresponds to the gap between the first and the second sector.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="1"),   
			conditionalPanel(condition="output.chrlabel",										  
			radioButtons("labels0", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
            conditionalPanel(condition="input.labels0==1 & input.trackChr=='track'",							  
			radioButtons("poslabels0", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="inner"),
			numericInput("heightlabels0", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels0", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			),											  
			conditionalPanel(condition="input.datatypeChr!='general' | input.trackChr=='track'",
			textInput("text0", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL)
			)
			),
			checkboxInput("optionsTrack1", HTML("<font color='red'>Track1</font>"), FALSE),
			conditionalPanel(condition="input.optionsTrack1 & input.uploadtrack1 == '2'",
			conditionalPanel(condition="input.typeTrack1=='bar'",
			radioButtons("directionTrack1", HTML("<table><tr><td><strong>Bar direction</strong></td>
<td>
<div class='help-tip'>
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack1=='2'",
			numericInput("barBoundary1", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track1", "Outer color:", value="red"),
			textInput("coldir2Track1", "Inner color:", value="cyan")
			)
			),										  
			conditionalPanel(condition="input.typeTrack1!='rect' & input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram'",
			conditionalPanel(condition="input.typeTrack1!='bar' | input.directionTrack1=='1'",
			radioButtons("coltypeTrack1", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
	                                            "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack1=='2'",	
			textInput("colorTrack1", NULL, value="red,blue")
			),
            conditionalPanel(condition="input.coltypeTrack1=='3'",	
			textInput("colorcusTrack1", NULL, value="a:red;b:blue;c:cyan")
			)			
			)
			),
			conditionalPanel(condition="input.typeTrack1=='line' & !output.stackmd1",
			radioButtons("fillareaTrack1", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack1=='rect'",
			radioButtons("rectTrack1", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the 4th column 
should be a categorical character vector with no more than 50 groups. For gradual data, the 4th 
column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),										  
			conditionalPanel(condition="input.rectTrack1=='2'",
			radioButtons("rectcolTrack1", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack1=='2'",
			textInput("rectcoldisTrack1", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack1=='3'",
			textInput("rectcoldiscusTrack1", NULL, value="a:red;b:blue;c:cyan")
			)
			),							  			
			conditionalPanel(condition="input.rectTrack1=='1'",
			selectInput("colrectTrack1", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack1=='line' & !output.stackmd1 & input.fillareaTrack1=='add'",
			radioButtons("selreaTrack1", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack1=='2'",
			textInput("borderareaTrack1", NULL, value="orange")
			)
			),			
			conditionalPanel(condition="input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram'",
			numericInput("transparencyTrack1", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),
            conditionalPanel(condition="input.typeTrack1=='point' & output.stackmd1",			
			textInput("symbolTrack1", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack1", "Point size:", value=0.6, min=0, max=1.5, step=0.05)									 
			),				
			conditionalPanel(condition="input.typeTrack1!='rect' & input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram' & !output.stackmd1",
			textInput("baselineTrack1", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),						  
			conditionalPanel(condition="input.typeTrack1!='rect' & input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram' & !(input.typeTrack1=='line' & output.stackmd1)",			
			textInput("colorlineTrack1", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram'",
			textInput("bgcolTrack1", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length 
adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),			
			conditionalPanel(condition="input.typeTrack1=='heatmap'",			
			radioButtons("heatmapcol1", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol1=='1'",			
			selectInput("colhmapTrack1", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol1=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor1", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor1", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor1", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),
			radioButtons("lineshmapTrack1", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2"),
			conditionalPanel(condition="input.lineshmapTrack1=='1'",
			numericInput("heightlinesTrack1", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack1", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),
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
			conditionalPanel(condition="input.typeTrack1=='heatmap'",
			radioButtons("gridsborderTrack1", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack1=='add'",
            textInput("colgridsborderTrack1", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),
			conditionalPanel(condition="input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram'",							  
			radioButtons("borderTrack1", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
			conditionalPanel(condition="output.marklabel1",
			radioButtons("labels1", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels1==1",							  
			radioButtons("poslabels1", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels1", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels1", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),						  
										  
										  
			textInput("text1", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack1!='rect' & input.typeTrack1!='heatmap' & input.typeTrack1!='ideogram' & output.trackdat1 & !output.stackmd1",
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
For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData1", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button1','Clear data'),
			numericInput("transparencyHlt1", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack2=='2'",
			numericInput("barBoundary2", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track2", "Outer color:", value="red"),
			textInput("coldir2Track2", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack2!='rect' & input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram'",
			conditionalPanel(condition="input.typeTrack2!='bar' | input.directionTrack2=='1'",
			radioButtons("coltypeTrack2", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
										  "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack2=='2'",	
			textInput("colorTrack2", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack2=='3'",	
			textInput("colorcusTrack2", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack2=='line' & !output.stackmd2",
			radioButtons("fillareaTrack2", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack2=='rect'",
			radioButtons("rectTrack2", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
			                                column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack2=='2'",
			radioButtons("rectcolTrack2", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack2=='2'",
			textInput("rectcoldisTrack2", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack2=='3'",
			textInput("rectcoldiscusTrack2", NULL, value="a:red;b:blue;c:cyan")
			)				
			),
			conditionalPanel(condition="input.rectTrack2=='1'",
			selectInput("colrectTrack2", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack2=='line' & !output.stackmd2 & input.fillareaTrack2=='add'",
			radioButtons("selreaTrack2", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack2=='2'",
			textInput("borderareaTrack2", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram'",
			numericInput("transparencyTrack2", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
		    ),
            conditionalPanel(condition="input.typeTrack2=='point' & output.stackmd2",			
			textInput("symbolTrack2", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack2", "Point size:", value=0.6, min=0, max=1.5, step=0.05)
			),				
			conditionalPanel(condition="input.typeTrack2!='rect' & input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram' & !output.stackmd2",
			textInput("baselineTrack2", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),			
			conditionalPanel(condition="input.typeTrack2!='rect' & input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram' & !(input.typeTrack2=='line' & output.stackmd2)",											  
			textInput("colorlineTrack2", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram'",
			textInput("bgcolTrack2", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary 
length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack2=='heatmap'",			
			radioButtons("heatmapcol2", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol2=='1'",			
			selectInput("colhmapTrack2", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol2=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor2", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor2", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor2", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack2", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack2=='1'",
			numericInput("heightlinesTrack2", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack2", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),
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
			conditionalPanel(condition="input.typeTrack2=='heatmap'",
			radioButtons("gridsborderTrack2", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack2=='add'",
            textInput("colgridsborderTrack2", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram'",										  
			radioButtons("borderTrack2", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel2",			
			radioButtons("labels2", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels2==1",							  
			radioButtons("poslabels2", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels2", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels2", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text2", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack2!='rect' & input.typeTrack2!='heatmap' & input.typeTrack2!='ideogram' & output.trackdat2 & !output.stackmd2",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData2", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button2','Clear data'),
			numericInput("transparencyHlt2", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack3=='2'",
			numericInput("barBoundary3", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track3", "Outer color:", value="red"),
			textInput("coldir2Track3", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack3!='rect' & input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram'",
			conditionalPanel(condition="input.typeTrack3!='bar' | input.directionTrack3=='1'",
			radioButtons("coltypeTrack3", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
	                                            "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack3=='2'",	
			textInput("colorTrack3", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack3=='3'",	
			textInput("colorcusTrack3", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack3=='line' & !output.stackmd3",
			radioButtons("fillareaTrack3", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack3=='rect'",
			radioButtons("rectTrack3", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack3=='2'",
			radioButtons("rectcolTrack3", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack3=='2'",
			textInput("rectcoldisTrack3", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack3=='3'",
			textInput("rectcoldiscusTrack3", NULL, value="a:red;b:blue;c:cyan")
			)
			),
			conditionalPanel(condition="input.rectTrack3=='1'",
			selectInput("colrectTrack3", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack3=='line' & !output.stackmd3 & input.fillareaTrack3=='add'",
			radioButtons("selreaTrack3", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack3=='2'",
			textInput("borderareaTrack3", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram'",
			numericInput("transparencyTrack3", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),
            conditionalPanel(condition="input.typeTrack3=='point' & output.stackmd3",			
			textInput("symbolTrack3", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack3", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),			
			conditionalPanel(condition="input.typeTrack3!='rect' & input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram' & !output.stackmd3",
			textInput("baselineTrack3", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),				
			conditionalPanel(condition="input.typeTrack3!='rect' & input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram' & !(input.typeTrack3=='line' & output.stackmd3)",											  
			textInput("colorlineTrack3", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram'",
			textInput("bgcolTrack3", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of 
arbitrary length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack3=='heatmap'",			
			radioButtons("heatmapcol3", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol3=='1'",			
			selectInput("colhmapTrack3", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol3=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor3", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor3", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor3", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack3", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack3=='1'",
			numericInput("heightlinesTrack3", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack3", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack3=='heatmap'",
			radioButtons("gridsborderTrack3", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack3=='add'",
            textInput("colgridsborderTrack3", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram'",										  
			radioButtons("borderTrack3", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel3",						
			radioButtons("labels3", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
		    conditionalPanel(condition="input.labels3==1",							  
			radioButtons("poslabels3", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels3", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels3", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text3", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack3!='rect' & input.typeTrack3!='heatmap' & input.typeTrack3!='ideogram' & output.trackdat3 & !output.stackmd3",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData3", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button3','Clear data'),
			numericInput("transparencyHlt3", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack4=='2'",
			numericInput("barBoundary4", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track4", "Outer color:", value="red"),
			textInput("coldir2Track4", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack4!='rect' & input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram'",
			conditionalPanel(condition="input.typeTrack4!='bar' | input.directionTrack4=='1'",
			radioButtons("coltypeTrack4", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
	                                            "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack4=='2'",	
			textInput("colorTrack4", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack4=='3'",	
			textInput("colorcusTrack4", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack4=='line' & !output.stackmd4",
			radioButtons("fillareaTrack4", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack4=='rect'",
			radioButtons("rectTrack4", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack4=='2'",
			radioButtons("rectcolTrack4", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack4=='2'",
			textInput("rectcoldisTrack4", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack4=='3'",
			textInput("rectcoldiscusTrack4", NULL, value="a:red;b:blue;c:cyan")
			)
			),
			conditionalPanel(condition="input.rectTrack4=='1'",
			selectInput("colrectTrack4", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack4=='line' & !output.stackmd4 & input.fillareaTrack4=='add'",
			radioButtons("selreaTrack4", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack4=='2'",
			textInput("borderareaTrack4", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram'",
			numericInput("transparencyTrack4", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
		    ),
            conditionalPanel(condition="input.typeTrack4=='point' & output.stackmd4",			
			textInput("symbolTrack4", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack4", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack4!='rect' & input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram' & !output.stackmd4",
			textInput("baselineTrack4", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
			),			
			conditionalPanel(condition="input.typeTrack4!='rect' & input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram' & !(input.typeTrack4=='line' & output.stackmd4)",	
			textInput("colorlineTrack4", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram'",
			textInput("bgcolTrack4", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length 
adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack4=='heatmap'",			
			radioButtons("heatmapcol4", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol4=='1'",			
			selectInput("colhmapTrack4", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol4=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor4", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor4", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor4", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack4", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack4=='1'",
			numericInput("heightlinesTrack4", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack4", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack4=='heatmap'",
			radioButtons("gridsborderTrack4", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack4=='add'",
            textInput("colgridsborderTrack4", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram'",										  
			radioButtons("borderTrack4", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel4",						
			radioButtons("labels4", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu./p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
            ),										  
			conditionalPanel(condition="input.labels4==1",							  
			radioButtons("poslabels4", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels4", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels4", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text4", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack4!='rect' & input.typeTrack4!='heatmap' & input.typeTrack4!='ideogram' & output.trackdat4 & !output.stackmd4",
			radioButtons("highlightTrack4", HTML("<table><tr><td><strong>Highlight regions</strong></td>
<td>
<div class='help-tip'>
	<p>Genomic regions to be highlighted with specified colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Show" = "1", "Hide" = "2"), selected="2"),
			conditionalPanel(condition="input.highlightTrack4==1",
			HTML("<table><tr><td>Paste data below:</td>
<td>
<div class='help-tip'>
	<p>Each row should contain four columns separated by commas indicating the chromosome ID, start coordinate, 
end coordinate and specified color. For example, 'chr1,1,100000000,red'. Hex color codes like '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData4", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button4','Clear data'),
			numericInput("transparencyHlt4", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack5=='2'",
			numericInput("barBoundary5", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track5", "Outer color:", value="red"),
			textInput("coldir2Track5", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack5!='rect' & input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram'",
			conditionalPanel(condition="input.typeTrack5!='bar' | input.directionTrack5=='1'",
			radioButtons("coltypeTrack5", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
	                                            "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack5=='2'",	
			textInput("colorTrack5", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack5=='3'",	
			textInput("colorcusTrack5", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack5=='line' & !output.stackmd5",
			radioButtons("fillareaTrack5", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack5=='rect'",
			radioButtons("rectTrack5", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="2"),
			conditionalPanel(condition="input.rectTrack5=='2'",
			radioButtons("rectcolTrack5", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack5=='2'",
			textInput("rectcoldisTrack5", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack5=='3'",
			textInput("rectcoldiscusTrack5", NULL, value="a:red;b:blue;c:cyan")
			)
			),
			conditionalPanel(condition="input.rectTrack5=='1'",
			selectInput("colrectTrack5", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", 
			                                               "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", 
			                                               "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack5=='line' & !output.stackmd5 & input.fillareaTrack5=='add'",
			radioButtons("selreaTrack5", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack5=='2'",
			textInput("borderareaTrack5", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram'",
			numericInput("transparencyTrack5", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),	
            conditionalPanel(condition="input.typeTrack5=='point' & output.stackmd5",			
			textInput("symbolTrack5", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack5", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack5!='rect' & input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram' & !output.stackmd5",
			textInput("baselineTrack5", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),			
			conditionalPanel(condition="input.typeTrack5!='rect' & input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram' & !(input.typeTrack5=='line' & output.stackmd5)",											  
			textInput("colorlineTrack5", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
			                                  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram'",
			textInput("bgcolTrack5", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted 
automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack5=='heatmap'",			
			radioButtons("heatmapcol5", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol5=='1'",			
			selectInput("colhmapTrack5", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol5=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor5", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor5", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor5", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack5", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack5=='1'",
			numericInput("heightlinesTrack5", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack5", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack5=='heatmap'",
			radioButtons("gridsborderTrack5", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack5=='add'",
            textInput("colgridsborderTrack5", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram'",										  
			radioButtons("borderTrack5", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel5",						
			radioButtons("labels5", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
		    conditionalPanel(condition="input.labels5==1",							  
			radioButtons("poslabels5", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels5", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels5", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text5", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack5!='rect' & input.typeTrack5!='heatmap' & input.typeTrack5!='ideogram' & output.trackdat5 & !output.stackmd5",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData5", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button5','Clear data'),
			numericInput("transparencyHlt5", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack6=='2'",
			numericInput("barBoundary6", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track6", "Outer color:", value="red"),
			textInput("coldir2Track6", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack6!='rect' & input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram'",
			conditionalPanel(condition="input.typeTrack6!='bar' | input.directionTrack6=='1'",
			radioButtons("coltypeTrack6", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
										  "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack6=='2'",	
			textInput("colorTrack6", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack6=='3'",	
			textInput("colorcusTrack6", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack6=='line' & !output.stackmd6",
			radioButtons("fillareaTrack6", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack6=='rect'",
			radioButtons("rectTrack6", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
			                                column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="2"),
			conditionalPanel(condition="input.rectTrack6=='2'",
			radioButtons("rectcolTrack6", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack6=='2'",
			textInput("rectcoldisTrack6", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack6=='3'",
			textInput("rectcoldiscusTrack6", NULL, value="a:red;b:blue;c:cyan")
			)
			),
			conditionalPanel(condition="input.rectTrack6=='1'",
			selectInput("colrectTrack6", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", "yellow", 
			                                               "navy", "seagreen", "maroon", "olivedrab", "gold", "lightblue", 
			                                               "navy.yellow", "purple.seagreen", "navy.orange", "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack6=='line' & !output.stackmd6 & input.fillareaTrack6=='add'",
			radioButtons("selreaTrack6", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack6=='2'",
			textInput("borderareaTrack6", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram'",
			numericInput("transparencyTrack6", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),
            conditionalPanel(condition="input.typeTrack6=='point' & output.stackmd6",			
			textInput("symbolTrack6", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack6", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack6!='rect' & input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram' & !output.stackmd6",
			textInput("baselineTrack6", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),			
			conditionalPanel(condition="input.typeTrack6!='rect' & input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram' & !(input.typeTrack6=='line' & output.stackmd6)",											  
			textInput("colorlineTrack6", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
			                                  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram'",
			textInput("bgcolTrack6", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length 
adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack6=='heatmap'",			
			radioButtons("heatmapcol6", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol6=='1'",			
			selectInput("colhmapTrack6", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol6=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor6", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor6", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor6", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack6", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack6=='1'",
			numericInput("heightlinesTrack6", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack6", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack6=='heatmap'",
			radioButtons("gridsborderTrack6", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack6=='add'",
            textInput("colgridsborderTrack6", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram'",										  
			radioButtons("borderTrack6", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel6",						
			radioButtons("labels6", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels6==1",							  
			radioButtons("poslabels6", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels6", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels6", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text6", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack6!='rect' & input.typeTrack6!='heatmap' & input.typeTrack6!='ideogram' & output.trackdat6 & !output.stackmd6",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData6", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button6','Clear data'),
			numericInput("transparencyHlt6", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack7=='2'",
			numericInput("barBoundary7", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track7", "Outer color:", value="deeppink"),
			textInput("coldir2Track7", "Inner color:", value="mediumblue")
			)
			),			
			conditionalPanel(condition="input.typeTrack7!='rect' & input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram'",
			conditionalPanel(condition="input.typeTrack7!='bar' | input.directionTrack7=='1'",
			radioButtons("coltypeTrack7", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
										  "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack7=='2'",	
			textInput("colorTrack7", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack7=='3'",	
			textInput("colorcusTrack7", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack7=='line' & !output.stackmd7",
			radioButtons("fillareaTrack7", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack7=='rect'",
			radioButtons("rectTrack7", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
			                                column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack7=='2'",
			radioButtons("rectcolTrack7", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack7=='2'",
			textInput("rectcoldisTrack7", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack7=='3'",
			textInput("rectcoldiscusTrack7", NULL, value="a:red;b:blue;c:cyan")
			)
			),									
			conditionalPanel(condition="input.rectTrack7=='1'",
			selectInput("colrectTrack7", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack7=='line' & !output.stackmd7 & input.fillareaTrack7=='add'",
			radioButtons("selreaTrack7", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack7=='2'",
			textInput("borderareaTrack7", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram'",
			numericInput("transparencyTrack7", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),	
            conditionalPanel(condition="input.typeTrack7=='point' & output.stackmd7",			
			textInput("symbolTrack7", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack7", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),			
			conditionalPanel(condition="input.typeTrack7!='rect' & input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram' & !output.stackmd7",
			textInput("baselineTrack7", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),				
			conditionalPanel(condition="input.typeTrack7!='rect' & input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram' & !(input.typeTrack7=='line' & output.stackmd7)",											  
			textInput("colorlineTrack7", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram'",
			textInput("bgcolTrack7", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length adjusted 
automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack7=='heatmap'",			
			radioButtons("heatmapcol7", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol7=='1'",			
			selectInput("colhmapTrack7", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol7=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor7", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor7", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor7", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack7", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack7=='1'",
			numericInput("heightlinesTrack7", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack7", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack7=='heatmap'",
			radioButtons("gridsborderTrack7", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack7=='add'",
            textInput("colgridsborderTrack7", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram'",										  
			radioButtons("borderTrack7", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel7",						
			radioButtons("labels7", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels7==1",							  
			radioButtons("poslabels7", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels7", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels7", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text7", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack7!='rect' & input.typeTrack7!='heatmap' & input.typeTrack7!='ideogram' & output.trackdat7 & !output.stackmd7",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData7", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button7','Clear data'),
			numericInput("transparencyHlt7", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="2"),
			conditionalPanel(condition="input.directionTrack8=='2'",
			numericInput("barBoundary8", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track8", "Outer color:", value="deeppink"),
			textInput("coldir2Track8", "Inner color:", value="mediumblue")
			)
			),			
			conditionalPanel(condition="input.typeTrack8!='rect' & input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram'",
			conditionalPanel(condition="input.typeTrack8!='bar' | input.directionTrack8=='1'",
			radioButtons("coltypeTrack8", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
										  "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack8=='2'",	
			textInput("colorTrack8", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack8=='3'",	
			textInput("colorcusTrack8", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack8=='line' & !output.stackmd8",
			radioButtons("fillareaTrack8", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack8=='rect'",
			radioButtons("rectTrack8", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
			                                column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack8=='2'",
			radioButtons("rectcolTrack8", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack8=='2'",
			textInput("rectcoldisTrack8", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack8=='3'",
			textInput("rectcoldiscusTrack8", NULL, value="a:red;b:blue;c:cyan")
			)
			),									  
			conditionalPanel(condition="input.rectTrack8=='1'",
			selectInput("colrectTrack8", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack8=='line' & !output.stackmd8 & input.fillareaTrack8=='add'",
			radioButtons("selreaTrack8", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack8=='2'",
			textInput("borderareaTrack8", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram'",
			numericInput("transparencyTrack8", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),	
            conditionalPanel(condition="input.typeTrack8=='point' & output.stackmd8",			
			textInput("symbolTrack8", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack8", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack8!='rect' & input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram' & !output.stackmd8",
			textInput("baselineTrack8", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),			
			conditionalPanel(condition="input.typeTrack8!='rect' & input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram' & !(input.typeTrack8=='line' & output.stackmd8)",											  
			textInput("colorlineTrack8", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
			                                  Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram'",
			textInput("bgcolTrack8", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary 
length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack8=='heatmap'",			
			radioButtons("heatmapcol8", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol8=='1'",			
			selectInput("colhmapTrack8", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol8=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor8", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor8", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor8", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack8", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack8=='1'",
			numericInput("heightlinesTrack8", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack8", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack8=='heatmap'",
			radioButtons("gridsborderTrack8", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack8=='add'",
            textInput("colgridsborderTrack8", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram'",										  
			radioButtons("borderTrack8", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel8",						
			radioButtons("labels8", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels8==1",							  
			radioButtons("poslabels8", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels8", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels8", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text8", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack8!='rect' & input.typeTrack8!='heatmap' & input.typeTrack8!='ideogram' & output.trackdat8 & !output.stackmd8",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData8", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button8','Clear data'),
			numericInput("transparencyHlt8", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack9=='2'",
			numericInput("barBoundary9", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track9", "Outer color:", value="red"),
			textInput("coldir2Track9", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack9!='rect' & input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram'",
			conditionalPanel(condition="input.typeTrack9!='bar' | input.directionTrack9=='1'",
			radioButtons("coltypeTrack9", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
	                                            "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack9=='2'",	
			textInput("colorTrack9", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack9=='3'",	
			textInput("colorcusTrack9", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack9=='line' & !output.stackmd9",
			radioButtons("fillareaTrack9", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack9=='rect'",
			radioButtons("rectTrack9", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
			                                column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack9=='2'",
			radioButtons("rectcolTrack9", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack9=='2'",
			textInput("rectcoldisTrack9", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack9=='3'",
			textInput("rectcoldiscusTrack9", NULL, value="a:red;b:blue;c:cyan")
			)
			),									  
			conditionalPanel(condition="input.rectTrack9=='1'",
			selectInput("colrectTrack9", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                               "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                               "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                               "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack9=='line' & !output.stackmd9 & input.fillareaTrack9=='add'",
			radioButtons("selreaTrack9", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack9=='2'",
			textInput("borderareaTrack9", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram'",
			numericInput("transparencyTrack9", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),
            conditionalPanel(condition="input.typeTrack9=='point' & output.stackmd9",			
			textInput("symbolTrack9", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack9", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack9!='rect' & input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram' & !output.stackmd9",
			textInput("baselineTrack9", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y axis coordinates of baselines. Numeric vector of arbitrary length is also accepted. 
For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
	        ),			
			conditionalPanel(condition="input.typeTrack9!='rect' & input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram' & !(input.typeTrack9=='line' & output.stackmd9)",											  
			textInput("colorlineTrack9", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines, which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram'",
			textInput("bgcolTrack9", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary 
length adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack9=='heatmap'",			
			radioButtons("heatmapcol9", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol9=='1'",			
			selectInput("colhmapTrack9", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol9=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor9", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor9", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor9", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack9", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack9=='1'",
			numericInput("heightlinesTrack9", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack9", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack9=='heatmap'",
			radioButtons("gridsborderTrack9", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack9=='add'",
            textInput("colgridsborderTrack9", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram'",										  
			radioButtons("borderTrack9", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel9",						
			radioButtons("labels9", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
			conditionalPanel(condition="input.labels9==1",							  
			radioButtons("poslabels9", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels9", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels9", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),
			textInput("text9", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack9!='rect' & input.typeTrack9!='heatmap' & input.typeTrack9!='ideogram' & output.trackdat9 & !output.stackmd9",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, start coordinate, 
end coordinate and the specified color. For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData9", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button9','Clear data'),
			numericInput("transparencyHlt9", HTML("<table><tr><td>Color transparency:</td>
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
	<p>Bars can be unidirectional or bidirectional. For bidirectional bars, the 4th column which 
contains the data values will be divided into two groups based on the boundary value.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Unidirectional" = "1", "Bidirectional" = "2"), selected="1"),
			conditionalPanel(condition="input.directionTrack10=='2'",
			numericInput("barBoundary10", "Boundary value:", value=0, step=0.01),     
			textInput("coldir1Track10", "Outer color:", value="red"),
			textInput("coldir2Track10", "Inner color:", value="cyan")
			)
			),			
			conditionalPanel(condition="input.typeTrack10!='rect' & input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram'",
			conditionalPanel(condition="input.typeTrack10!='bar' | input.directionTrack10=='1'",
			radioButtons("coltypeTrack10", HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
To customize color for data with multiple columns, users should provide a character string representing one or multiple 
colors separated by commas. For example, 'red' or 'red,orange,blue'.
To customize color for data with multiple groups, the column indicating different groups should be named as 'color' or 'stack'.
Users should provide a character strings assigning colors to each group. 
For example, 'a:red;b:green;c:blue', in which 'a b c' represent different data groups. 
Color for data groups without assigned color would be set as 'grey'. 
Hex color codes as '#FF0000' are also supported. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Custom for data with multi-column" = "2", 
										  "Custom for data with multi-group" = "3"), selected="1"),
			conditionalPanel(condition="input.coltypeTrack10=='2'",	
			textInput("colorTrack10", NULL, value="red,blue")
			),
			conditionalPanel(condition="input.coltypeTrack10=='3'",	
			textInput("colorcusTrack10", NULL, value="a:red;b:blue;c:cyan")
			)
			)
			),
			conditionalPanel(condition="input.typeTrack10=='line' & !output.stackmd10",
			radioButtons("fillareaTrack10", HTML("<table><tr><td><strong>Fill area</strong></td>
<td>
<div class='help-tip'>
	<p>Fill the area below the lines with colors.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
            ),			
			conditionalPanel(condition="input.typeTrack10=='rect'",
			radioButtons("rectTrack10", HTML("<table><tr><td><strong>Data type</strong></td>
<td>
<div class='help-tip'>
	<p>The rects are filled with gradual or discrete colors. For discrete data, the fourth column 
should be a categorical character vector with no more than 50 groups. For gradual data, the fourth 
column should be a numeric vector. See example data for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Gradual" = "1", "Discrete" = "2"), selected="1"),
			conditionalPanel(condition="input.rectTrack10=='2'",
			radioButtons("rectcolTrack10", HTML("<table><tr><td><strong>Select color</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used to plot the data, which can be random assigned by the application or specified by the users. 
If 'Specific' was chosen, all data will be filled by a specified color. 
If 'Custom' was chosen, the 4th column of the uploaded data should be a categorical character vector with no more than 50 groups.
Users should provide values as 'a:red;b:green;c:blue', in which 'a b c' represent different
data category indicated by the 4th column of the uploaded data. 
Color for data without customed color will be set to NULL. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2", "Custom" = "3"), selected="1"),
			conditionalPanel(condition="input.rectcolTrack10=='2'",
			textInput("rectcoldisTrack10", NULL, value="red")
			),
			conditionalPanel(condition="input.rectcolTrack10=='3'",
			textInput("rectcoldiscusTrack10", NULL, value="a:red;b:blue;c:cyan")
			)
			),									  
			conditionalPanel(condition="input.rectTrack10=='1'",
			selectInput("colrectTrack10", NULL, choices = c("blue", "red", "green", "cyan", "purple", "pink", "orange", 
			                                                "yellow", "navy", "seagreen", "maroon", "olivedrab", "gold", 
			                                                "lightblue", "navy.yellow", "purple.seagreen", "navy.orange", 
			                                                "navy.cyan", "blue.red", "green.red"))
			)
			),
			conditionalPanel(condition="input.typeTrack10=='line' & !output.stackmd10 & input.fillareaTrack10=='add'",
			radioButtons("selreaTrack10", HTML("<table><tr><td><strong>Area color</strong></td>
<td>
<div class='help-tip'>
	<p>Filled color to the area, which can be identical with lines color or specified by the users. If 'Specific' was chosen, all data will be filled by a specified color as 'orange'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Identical with lines" = "1", "Specific" = "2"),selected="1"),
		    conditionalPanel(condition="input.selreaTrack10=='2'",
			textInput("borderareaTrack10", NULL, value="orange")
			)
			),				
			conditionalPanel(condition="input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram'",
			numericInput("transparencyTrack10", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=1, min=0, max=1, step=0.1)
			),
            conditionalPanel(condition="input.typeTrack10=='point' & output.stackmd10",			
			textInput("symbolTrack10", HTML("<table><tr><td><strong>Symbol type:</strong></td>
<td>
<div class='help-tip'>
	<p>Symbols used for different points. Applicable value can be a number in [0-25] or a numeric vector of arbitrary length
adjusted automatically to the number of data categories. Type ?pch in R console for more details.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="16"),
			numericInput("pointsizeTrack10", "Point size:", value=0.6, min=0, max=1.5, step=0.05)										  
			),				
			conditionalPanel(condition="input.typeTrack10!='rect' & input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram' & !output.stackmd10",
			textInput("baselineTrack10", HTML("<table><tr><td><strong>Y coordinates of baselines:</strong></td>
<td>
<div class='help-tip'>
	<p>Decimal numbers in [0, 1] to adjust y-axis coordinates of baselines. Numeric vector of arbitrary length is 
accepted. For example, '0.5' or '0.25,0.5,0.75'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="0.25,0.75")
			),			
			conditionalPanel(condition="input.typeTrack10!='rect' & input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram' & !(input.typeTrack10=='line' & output.stackmd10)",	
			textInput("colorlineTrack10", HTML("<table><tr><td><strong>Baselines color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the baselines which can be null or a character vector of arbitrary length 
adjusted automatically to the number of baselines. For example, 'grey' or 'red,green'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey")
			),
			conditionalPanel(condition="input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram'",
			textInput("bgcolTrack10", HTML("<table><tr><td><strong>Background color(s):</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the background of the plot which can be null or a color vector of arbitrary length 
adjusted automatically to the number of sectors. For example, 'grey95' or 'grey95,grey,pink,yellow'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="grey95")
			),
			conditionalPanel(condition="input.typeTrack10=='heatmap'",			
			radioButtons("heatmapcol10", HTML("<table><tr><td><strong>Colors</strong></td>
<td>
<div class='help-tip'>
	<p>Colors to be used for the heatmap, which can be assigned by the application or specified by the users.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Typical" = "1", "Custom" = "2"), selected="1"),			
			conditionalPanel(condition="input.heatmapcol10=='1'",			
			selectInput("colhmapTrack10", NULL, choices = c("blue.white.red", "green.black.red", "green.yellow.red", 
	                                                      "purple.yellow.red", "blue.green.red", "blue.yellow.green", 
	                                                      "cyan.white.deeppink1"), selected="blue.white.red")
			),
			conditionalPanel(condition="input.heatmapcol10=='2'",
			 fluidRow(
			   column(4,jscolorInput("lowColor10", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColor10", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColor10", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')
			),			
			radioButtons("lineshmapTrack10", HTML("<table><tr><td><strong>Add position lines</strong></td>
<td>
<div class='help-tip'>
	<p>Add genomic position lines between tracks, which can be used to identify the correspondance between heatmaps and regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"),selected="2"),
			conditionalPanel(condition="input.lineshmapTrack10=='1'",
			numericInput("heightlinesTrack10", HTML("<table><tr><td><strong>Position lines height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlinesTrack10", HTML("<table><tr><td><strong>Position lines margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the position lines.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)
			)
			),			
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
			conditionalPanel(condition="input.typeTrack10=='heatmap'",
			radioButtons("gridsborderTrack10", HTML("<table><tr><td><strong>Add cell borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the heatmap grids, which can separate cells from each other.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected=""),
			conditionalPanel(condition="input.gridsborderTrack10=='add'",
            textInput("colgridsborderTrack10", HTML("<table><tr><td><strong>Borders color:</strong></td>
<td>
<div class='help-tip'>
	<p>The color to be used for the borders of heatmap grids. For example, 'white' or 'red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value="black")
			)
			),										  
            conditionalPanel(condition="input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram'",										  
			radioButtons("borderTrack10", HTML("<table><tr><td><strong>Add borders</strong></td>
<td>
<div class='help-tip'>
	<p>Add borders to the plotting regions.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "add", "No" = ""),selected="")
			),
            conditionalPanel(condition="output.marklabel10",						
			radioButtons("labels10", HTML("<table><tr><td><strong>Add labels</strong></td>
<td>
<div class='help-tip'>
	<p>Add labels to mark genes or genomic regions for this track using data uploaded in the 'Data upload' menu.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Yes" = "1", "No" = "2"), selected="2")
			),
            conditionalPanel(condition="input.labels10==1",							  
			radioButtons("poslabels10", "Labels position", c("Outer" = "outer", "Inner" = "inner"),selected="outer"),
			numericInput("heightlabels10", HTML("<table><tr><td><strong>Labels height:</strong></td>
<td>
<div class='help-tip'>
	<p>Height of the labels, which should be greater than 0.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.06, min=0, max=0.8, step=0.01),
			numericInput("marginlabels10", HTML("<table><tr><td><strong>Labels margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the labels.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005)			
			),										  
			textInput("text10", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
			conditionalPanel(condition="input.typeTrack10!='rect' & input.typeTrack10!='heatmap' & input.typeTrack10!='ideogram' & output.trackdat10 & !output.stackmd10",
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
	<p>Each row should contain four components separated by commas including the chromosome ID, 
start coordinate, end coordinate and the specified color. For example, 'chr1,1,100000000,red'. 
Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltData10", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_button10','Clear data'),
			numericInput("transparencyHlt10", HTML("<table><tr><td>Color transparency:</td>
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
			radioButtons("colformatLinks", HTML("<table><tr><td>Data format</td>
<td>
<div class='help-tip'>
	<p>The format of links data specified by the user. For data with 6 columns, user should select 'Data without color column'. 
	For data with 7 columns, user should select 'Data with multi-group' if the 'color' column represents different categories indicated by a character string as 'a, b, c'. 
	For data with 7 columns, user should select 'Data with gradual values' if the 'color' column represents gradual values indicated by numbers as '1, 2, 3'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Data without 'color' column" = "1","Data with multi-group" = "2", "Data with gradual values" = "3"), selected="1"),							  
			conditionalPanel(condition="input.colformatLinks==1 | input.colformatLinks==2",	
			radioButtons("colorLinks", HTML("<table><tr><td>Data color</td>
<td>
<div class='help-tip'>
	<p>Links are filled with colors random assigned by the system or colors specified by the user. 
For data with 6 columns, format of specified color as 'red' or 'green' is supported. 
For data with 7 columns, format of specified color as 'a:red;b:green;c:blue' is supported. 
'a', 'b', 'c' represents different categories indicated by the 7th column. Color for data groups without assigned color would be set as 'grey'.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), c("Random" = "1", "Specific" = "2"), selected="1"),
			conditionalPanel(condition="input.colorLinks==2",
			textInput("selcolorLinks", NULL, value="yellowgreen")
			),
			numericInput("transparencyLinks", HTML("<table><tr><td>Color transparency:</td>
<td>
<div class='help-tip'>
	<p>A decimal number in [0, 1] to adjust the color transparency. The higher the value, the deeper the color.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.5, min=0, max=1, step=0.1)
			),
			conditionalPanel(condition="input.colformatLinks==3",
		    HTML("<table><tr><td><strong>Data color</strong></td>
<td>
<div class='help-tip'>
	<p>For data with 'color' column indicated by numbers, links are filled with colors specified by the user.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),										  
			fluidRow(
			   column(4,jscolorInput("lowColinks", label = HTML('<p><font size="1.8"><strong>Low Color</strong></font></p>'), value = "#0016DB")), 
			   column(4, jscolorInput("midColinks", label = HTML('<p><font size="1.8"><strong>Middle Color</strong></font></p>'), value = "#FFFFFF")),
			   column(4, jscolorInput("highColinks", label = HTML('<p><font size="1.8"><strong>High Color</strong></font></p>'), value = "#FFFF00"))),					
			   HTML('<br>')          
			),			
			numericInput("marginLinks", HTML("<table><tr><td><strong>Track margin:</strong></td>
<td>
<div class='help-tip'>
	<p>Margin size of the track.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=0.01, min=0, max=0.8, step=0.005),
			textInput("text11", HTML("<table><tr><td><strong>Legend text</strong></td>
<td>
<div class='help-tip'>
	<p>The text to appear in the legend.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "), value=NULL),
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
	<p>Each row should contain four components separated by commas including the 
chromosome ID, start coordinate, end coordinate and the specified color. 
For example, 'chr1,1,100000000,red'. Hex color codes as '#FF0000' are also supported.</p>
	                                      </div></td></tr>
	                                      </table>
	                                      "),
			tags$textarea(id="hltDataLinks", rows=10, cols=30, ""),
			HTML('<br>'),
			HTML('<p>Data separated by commas.</p>'),
			actionButton('clearText_buttonLinks','Clear data'),
			numericInput("transparencyhltLinks", HTML("<table><tr><td>Color transparency:</td>
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
			checkboxInput("adfontSize", HTML("<font color='red'>Adjust font size</font>"), FALSE),
			conditionalPanel(condition="input.adfontSize",
			radioButtons("fontSize", NULL, c("Small" = "1", "Medium" = "1.1","Large"="1.2","Custom"="custom")),
			conditionalPanel(condition="input.fontSize=='custom'",
			numericInput("cexlabel", NULL, value=1, min=0.1, max=3, step=0.1)
			)
			),			
			checkboxInput("addlegend", HTML("<font color='red'>Add legend</font>"), FALSE),
			conditionalPanel(condition="input.addlegend",
			radioButtons("seladdlegend", NULL, c("Yes" = "1", "No" = "2"), selected="1"),
			conditionalPanel(condition="input.seladdlegend=='1'",
			radioButtons("selposlegend", "Legend position", c("Right" = "1", "Center" = "2"), selected="1")
			)		
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
		conditionalPanel(condition="input.tabs1=='Gallery'",
		                 h4("Gallery")
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
				HTML('<p>1. R Development Core Team. <i><a href="http://www.r-project.org/" target="_blank">R</a>:  A Language and Environment for Statistical Computing.</i> R Foundation for Statistical Computing, Vienna (2016) <br>
				2. RStudio and Inc. <i><a href="http://www.rstudio.com/shiny/" target="_blank">shiny</a>: Web Application Framework for R.</i> R package version 1.0.0 (2016) <br>
				3. Gu, Z. <i><a href="http://cran.r-project.org/web/packages/circlize/index.html" target="_blank">circlize</a>: Circular Visualization.</i> R package version 0.4.1 (2017) <br>
				4. Neuwirth, E. <i><a href="http://cran.r-project.org/web/packages/RColorBrewer/index.html" target="_blank">RColorBrewer</a>: ColorBrewer palettes.</i> R package version 1.1-2 (2014) <br>
				5. Lawrence, M. <i><a href="http://bioconductor.org/packages/GenomicRanges/" target="_blank">GenomicRanges</a>: Representation and manipulation of genomic intervals and variables defined along a genome.</i> R package version 1.24.3 (2016) <br>
				6. Dowle, M. <i><a href="http://cran.r-project.org/web/packages/data.table/index.html" target="_blank">data.table</a>: Extension of Data.frame.</i> R package version 1.9.6 (2015) <br>				
				7. Burow, C. <i><a href="https://cran.r-project.org/web/packages/RLumShiny/index.html" target="_blank">RLumShiny</a>: "Shiny" Applications for the R Package "Luminescence".</i> R package version 0.1.1 (2016) <br>								
				8. R Core Team and contributors worldwide. <i><a href="http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/00Index.html" target="_blank">grDevices</a>: Graphics devices and support for base and grid graphics.</i> R package version 3.3.3 (2016) <br></p>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Further references</font></li></ul></p>'),
				h6("This application was created by ", a("Wen Yao", href="https://www.researchgate.net/profile/Wen_Yao", target="_blank"), " and ", a("Yiming Yu", href="https://www.researchgate.net/profile/Yiming_Yu6", target="_blank"), 
				". Please send bugs and feature requests to Wen Yao (ywhzau at gmail.com) or Yiming Yu (yimingyyu at gmail.com). This application uses the ", 
				a("shiny package from RStudio", href="http://www.rstudio.com/shiny/", target="_blank"), "."),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Please cite</font></li></ul></p>'),
				h6("Yiming Yu, Yidan Ouyang, and Wen Yao. shinyCircos: an R/Shiny application for interactive creation of Circos plot. ", 
				em(strong("Bioinformatics."))," 2017 Nov. 24. ", a("doi:10.1093/bioinformatics/btx763", href="https://academic.oup.com/bioinformatics/advance-article-abstract/doi/10.1093/bioinformatics/btx763/4657077", target="_blank")),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%";><font size="5" color="red">!!Note!!</font></li></ul></p>'),
				h5("Users are encouraged to install and use shinyCircos on local personal computers. 
				Please check the help menu of the shinyCircos application or  ", a("https://github.com/venyao/shinyCircos", href="https://github.com/venyao/shinyCircos", target="_blank"), " for the installation of shinyCircos on local computers.")
			),
			## *** Data upload panel ***
			tabPanel("Data upload",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Download example data</font></li></ul></p>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example chromosomes data</font></li></ul></p>'),
			    downloadButton("chromosome_general.csv", "general data"),
				downloadButton("chromosome_cytoband.csv", "cytoband data"),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example tracks data</font></li></ul></p>'),
				downloadButton("point.csv", "point data"),
				downloadButton("line.csv", "line data"),
				downloadButton("barplot.csv", "barplot data"),
				downloadButton("heatmap.csv", "heatmap data"),
				downloadButton("chromosome_ideogram.csv", "ideogram data"),
				downloadButton("rect_discrete.csv", "rect (discrete) data"),
				downloadButton("point_multicolumn.csv", "point (multicolumn) data"),
				downloadButton("barplot_bidirectional.csv", "barplot (bidirectional) data"),
				downloadButton("rect_gradual.csv", "rect (gradual) data"),
				downloadButton("line_multicolumn.csv", "line (multicolumn) data"),
				downloadButton("line_color.csv", "line (color) data"),
				downloadButton("barplot_color.csv", "barplot (color) data"),
				downloadButton("point_color.csv", "point (color) data"),
				downloadButton("point_pch.csv", "point (pch) data"),
				downloadButton("point_color_pch.csv", "point (color+pch) data"),
				downloadButton("point_cex.csv", "point (cex) data"),
				downloadButton("point_pch_cex.csv", "point (pch+cex) data"),
				downloadButton("point_color_cex.csv", "point (color+cex) data"),
				downloadButton("point_color_pch_cex.csv", "point (color+pch+cex) data"),
				downloadButton("gene_label.csv", "label data"),
				downloadButton("stack_point.csv", "point (stack) data"),
				downloadButton("stack_line.csv", "line (stack) data"),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Example links data</font></li></ul></p>'),				
				downloadButton("links.csv", "links data"),
				downloadButton("links_discrete_color.csv", "links (discrete color) data"),
				downloadButton("links_gradual_color.csv", "links (gradual color) data"),				
				HTML('<br>'),
				HTML('<br>'),
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Glimpse of data uploaded</font></li></ul></p>'),
				textOutput("errorinfo1"),
				conditionalPanel(condition="input.submit1>0",
				conditionalPanel(condition="output.chrdat",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Chromosomes data</font></li></ul></p>'),				
				tableOutput("viewChr"),
				conditionalPanel(condition="output.chrlabel",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Label</font></li></ul></p>'),				
				tableOutput("viewlabelChr")	
                )				
				),
				conditionalPanel(condition="input.uploadtrack1 == '2' | input.uploadtrack2 == '2' | input.uploadtrack3 == '2' | input.uploadtrack4 == '2' | input.uploadtrack5 == '2' | input.uploadtrack6 == '2' | input.uploadtrack7 == '2' | input.uploadtrack8 == '2' | input.uploadtrack9 == '2' | input.uploadtrack10 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Tracks data</font></li></ul></p>'),				
				conditionalPanel(condition="input.uploadtrack1 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track1</font></li></ul></p>'),
				tableOutput("viewTrack1"),				
				conditionalPanel(condition="output.labeldat1",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label1</font></li></ul></p>'),				
				tableOutput("viewlabelTrack1")
				)
				),
				conditionalPanel(condition="input.uploadtrack2 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track2</font></li></ul></p>'),				
				tableOutput("viewTrack2"),
				conditionalPanel(condition="output.labeldat2",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label2</font></li></ul></p>'),				
				tableOutput("viewlabelTrack2")
				)				
				),
				conditionalPanel(condition="input.uploadtrack3 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track3</font></li></ul></p>'),				
				tableOutput("viewTrack3"),
				conditionalPanel(condition="output.labeldat3",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label3</font></li></ul></p>'),				
				tableOutput("viewlabelTrack3")
				)				
				),
				conditionalPanel(condition="input.uploadtrack4 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track4</font></li></ul></p>'),				
				tableOutput("viewTrack4"),
				conditionalPanel(condition="output.labeldat4",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label4</font></li></ul></p>'),				
				tableOutput("viewlabelTrack4")
				)				
				),
				conditionalPanel(condition="input.uploadtrack5 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track5</font></li></ul></p>'),				
				tableOutput("viewTrack5"),
				conditionalPanel(condition="output.labeldat5",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label5</font></li></ul></p>'),				
				tableOutput("viewlabelTrack5")
				)				
				),
				conditionalPanel(condition="input.uploadtrack6 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track6</font></li></ul></p>'),				
				tableOutput("viewTrack6"),
				conditionalPanel(condition="output.labeldat6",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label6</font></li></ul></p>'),				
				tableOutput("viewlabelTrack6")
				)				
				),
				conditionalPanel(condition="input.uploadtrack7 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track7</font></li></ul></p>'),				
				tableOutput("viewTrack7"),
				conditionalPanel(condition="output.labeldat7",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label7</font></li></ul></p>'),				
				tableOutput("viewlabelTrack7")
				)				
				),
				conditionalPanel(condition="input.uploadtrack8 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track8</font></li></ul></p>'),				
				tableOutput("viewTrack8"),
				conditionalPanel(condition="output.labeldat8",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label8</font></li></ul></p>'),				
				tableOutput("viewlabelTrack8")
				)				
				),
				conditionalPanel(condition="input.uploadtrack9 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track9</font></li></ul></p>'),				
				tableOutput("viewTrack9"),
				conditionalPanel(condition="output.labeldat9",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label9</font></li></ul></p>'),				
				tableOutput("viewlabelTrack9")
				)				
				),
				conditionalPanel(condition="input.uploadtrack10 == '2'",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Track10</font></li></ul></p>'),				
				tableOutput("viewTrack10"),
				conditionalPanel(condition="output.labeldat10",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:6px 6px; background-repeat: no-repeat; background-position: 6px 50%"><font size="2">Label10</font></li></ul></p>'),				
				tableOutput("viewlabelTrack10")
				)				
				)
				),
				conditionalPanel(condition="input.linksTrack > 0",
				HTML('<p <ul><li style="list-style-type: none; background-image: url(bullet.jpg); padding-left: 18px; background-size:9px 9px; background-repeat: no-repeat; background-position: 0px 50%"><font size="4">Links data</font></li></ul></p>'),				
				tableOutput("viewLink")
				)
				)
			),
			## *** Circos visualization panel ***
			tabPanel("Circos visualization", tags$head(
			tags$style("
				#lowColor1, #highColor1, #midColor1, #lowColor2, #highColor2, #midColor2, #lowColor3, #highColor3, #midColor3,
				#lowColor4, #highColor4, #midColor4, #lowColor5, #highColor5, #midColor5, #lowColor6, #highColor6, #midColor6,
				#lowColor7, #highColor7, #midColor7, #lowColor8, #highColor8, #midColor8, #lowColor9, #highColor9, #midColor9,
				#lowColor10, #highColor10, #midColor10, #lowColinks, #midColinks, #highColinks {width:100%}
				")
		    ),
				downloadButton("shinyCircos.pdf", "Download pdf-file"),
				downloadButton("shinyCircos.svg", "Download svg-file"),
				downloadButton("script.R", "Download the R scripts to reproduce the Circos plot"),
				textOutput("errorinfo2"),			
				textOutput("errorinfo3"),
				textOutput("errorinfo4"),
				textOutput("errorinfo5"),			
				textOutput("errorinfo6"),
				textOutput("errorinfo7"),	
                textOutput("errorinfo8"),
                textOutput("errorinfo9"),
                textOutput("errorinfo10"),
                textOutput("errorinfo11"),					
				plotOutput("circosfigure", height='100%', width='100%')
			),
			## *** FAQ panel***
			tabPanel("Gallery",
			    includeHTML("Gallery.html")
			),		
			tabPanel("Help",
				includeHTML("README.html")
			),					
			id="tabs1"
		)
	),position="left"    
))
)


