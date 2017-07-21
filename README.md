---
output:
  word_document: default
  html_document: default
---
shinyCircos
========

This is the repository for the Shiny application presented in "shinyCircos: an R/shiny application for creation of Circos plot interactively" (Yu et al. 2017).

*****

#	Use shinyCircos online

shinyCircos is deployed at https://circos.shinyapps.io/shinycircos/ for online use.  
shinyCircos is idle until you activate it by accessing this URL.  
So it may take some time when you access this URL for the first time.   
Once it was activated, shinyCircos could be used smoothly and easily.

*****

#	Launch shinyCircos directly from R and GitHub

**Step 1: Install R and RStudio**

Before running the app you will need to have R and RStudio installed (tested with R 3.3.3 and RStudio 1.0.143).  
Please check CRAN (https://cran.r-project.org/) for the installation of R.  
Please check https://www.rstudio.com/ for the installation of RStudio.  

**Step 2: Install the R Shiny package and other packages required by shinyCircos**

Start an R session using RStudio and run these lines:  
```
install.packages("shiny")  
install.packages("circlize")  
install.packages("RColorBrewer")  
# try http:// if https:// URLs are not supported   
source("https://bioconductor.org/biocLite.R")  
biocLite("GenomicRanges")
```

**Step 3: Start the app**  

Start an R session using RStudio and run these lines:  
```
shiny::runGitHub("shinyCircos", "venyao")  
```

Your web browser will open the app.

*****

#	Deploy shinyCircos on local or web Linux server

**Step 1: Install R**  

Please check CRAN (https://cran.r-project.org/) for the installation of R.

**Step 2: Install the R Shiny package and other packages required by shinyCircos**  

Start an R session and run these lines in R:  
```
install.packages("shiny")  
install.packages("circlize")  
install.packages("RColorBrewer")  
## try http:// if https:// URLs are not supported  
source("https://bioconductor.org/biocLite.R")  
biocLite("GenomicRanges")  
```

For more information, please check the following pages:  
https://cran.r-project.org/web/packages/shiny/index.html  
https://github.com/rstudio/shiny  
https://shiny.rstudio.com/  

**Step 3: Install Shiny-Server**

Please check the following pages for the installation of shiny-server.  
https://www.rstudio.com/products/shiny/download-server/  
https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source  

**Step 4: Upload files of shinyCircos**

Put the directory containing the code and data of shinyCircos to /srv/shiny-server.  

**Step 5: Configure shiny server (/etc/shiny-server/shiny-server.conf)**

```
# Define the user to spawn R Shiny processes
run_as shiny;

# Define a top-level server which will listen on a port
server {  
  # Use port 3838  
  listen 3838;  
  # Define the location available at the base URL  
  location /shinycircos {  
    # Directory containing the code and data of shinyCircos  
    app_dir /srv/shiny-server/shinyCircos;  
    # Directory to store the log files  
    log_dir /var/log/shiny-server;  
  }  
}  
```

**Step 6: Change the owner of the shinyCircos directory**

```
$ chown -R shiny /srv/shiny-server/shinyCircos  
```

**Step 7: Start Shiny-Server**

```
$ start shiny-server  
```

Now, the shinyCircos app is available at http://IPAddressOfTheServer:3838/shinycircos/.  


