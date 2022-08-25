#' Shiny app server function
#'
#' @author Anton Ribbenstedt
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#'

# Main app server ---
#' @export
server <- function(input, output, session){
  r<-reactiveValues()

  plan(multisession)

  r$configWiz<-reactiveValues()
  r$monitor<-reactiveValues()
  #r$mainTabs<-reactive({input$mainTabs})
  r$examineData<-reactiveValues()
  observe({
    r$configWiz$roots<-getVolumes()()
  })
  r$chroms <- reactiveValues()
  r$findLamas <- reactiveValues()
  r$runBatch <- reactiveValues()




  ###Running module servers
  configWizServer("configWiz", r)
  monitorServer("monitor", r)
  examineDataServer("examineData", r)
  chromatogramServer("chroms", r)
  tutorialServer("tutorial", r)
  findLamasServer("findLamas", r)
  runBatchServer("runBatch", r)
  findLimitsServer("findLimits", r)


  ###Session end
  onSessionStart = isolate({
    r$runBatch$running <- F
    r$monitor$start <- F
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}
