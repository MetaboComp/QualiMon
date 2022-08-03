#' launchApp - Function for running QualiMon
#'
#' @import shiny shinydashboardPlus RSQLite DBI stringr stringi plotly shinyFiles data.table future openxlsx slackr promises cpc heatmaply magrittr ipc fontawesome
#'
#' @importFrom shinydashboard sidebarMenu menuItem menuSubItem tabBox tabItems tabItem dashboardBody renderInfoBox valueBox valueBoxOutput
#' @importFrom shinyWidgets awesomeCheckbox prettySwitch pickerInput updatePickerInput
#' @importFrom stats complete.cases cor dnorm loess predict pt qt sd
#' @importFrom utils read.csv2 read.table write.table
#' @importFrom IPO findIsotopes.CAMERA
#' @importFrom MSnbase readMSData
#' @importFrom mzR tic
#' @importFrom xcms findChromPeaks CentWaveParam chromPeaks rtime intensity chromatogram loadRaw xcmsSource filepaths featureValues featureDefinitions
#'
#' @export launchApp
#' @return shiny application object
#'

launchApp <- function() {
  # shiny::runApp(appDir = system.file("shiny", package="QualiMon"))
  shinyApp(ui=ui, server=server)
}
