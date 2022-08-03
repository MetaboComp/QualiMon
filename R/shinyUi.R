#' Shiny app ui object
#'
#' @author Anton Ribbenstedt
#'
#'


# Main App UI ---
#' @export
ui<-dashboardPage(
  skin="yellow",
  dashboardHeader(title="QualiMon"),

  dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("Tutorial", tabName="tutorial", icon=icon("book")),
      menuItem("Setup", tabName="setup", icon=icon("desktop"), startExpanded=F,
               menuSubItem("Find LaMas & New DB", tabName="findLamas", icon=icon("search")),
               menuSubItem("Set-up Wizard", tabName="configWiz", icon=icon("magic")),
               menuSubItem("Find S&H limits", tabName="findLimits", icon=icon("grip-lines"))
      ),
      menuItem("Monitor data", tabName="monitorData", icon=icon("signal"), startExpanded=F,
               menuSubItem("Live monitor", tabName="liveMonitor", icon=icon("signal")),
               menuSubItem("LaMa chromatograms", tabName="chroms", icon=icon("chart-area"))
      ),
      menuItem("Review old data", tabName="oldData", icon=icon("chart-line")),
      menuItem("Run batch", tabName="runBatch", icon=icon("boxes"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem("tutorial", tutorialUI("tutorial")),
      tabItem("findLamas", findLamasUI("findLamas")), #findLamasUI("findLamas")
      tabItem("configWiz", configWizUI("configWiz")),
      tabItem("runBatch", runBatchUI("runBatch")),
      tabItem("findLimits", findLimitsUI("findLimits")),
      tabItem("liveMonitor", monitorUI("monitor")),
      tabItem("oldData", examineDataUI("examineData")),
      tabItem("chroms", chromatogramUI("chroms"))
    )
  )
)
