# QualiMon
Live monitoring of LC-MS data
QualiMon - LaMas is a shiny app made for local use only.
Evaluate the quality of every injection as acquisition is completed.
Associated manuscript 'Continuous Quality Monitoring for Non-target MS-based Analysis' have been submitted for peer review.


### 1. Installation
```
if (!require("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("MetaboComp/QualiMon")
```

### 2. Launching the app
```
QualiMon::launchApp()
```

### 3. Tutorial
The tutorial for using the app can be found [here](https://github.com/MetaboComp/QualiMon/blob/master/QualimonTutorial_v1.pdf)


### 4. System requirements
#### Software dependencies and versions tested:
Operating system:
- Windows 10 (due to proteowizard dependency)

Software:
-	Proteowizard (v 3)
-	RStudio – RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for Windows
Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.8 Chrome/69.0.3497.128 Safari/537.36
-	R – v4.1.0

R-packages:
-	Shiny – v1.7.2
-	Shinydashboard – v0.7.2
-	shinydashboardPlus – v2.0.4.9000
-	RSQLite – v2.2.15
-	DBI – v1.1.3
-	stringr – v1.4.0
-	stringi – v1.7.8
-	plotly – v4.10.0.9001
-	heatmaply – v1.3.0
-	shinyWidgets – v0.7.2
-	shinyFiles – v0.9.2
-	data.table – v1.14.2
-	future – v1.27.0
-	xcms – v3.16.1
-	openxlsx – v4.2.5
-	slackr – v3.2.0
-	promises – v1.2.0.1
-	cpc – v0.1.0
-	magrittr – v2.0.3
-	stats – v4.1.0
-	utils – v4.1.0
-	mzR – v2.28.0
-	ipc – v0.1.3
-	MSnbase – v2.20.4
-	IPO – v.1.20.0
-	Fontawesome – v0.3.0
