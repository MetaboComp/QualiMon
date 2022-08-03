tutorialUI<-function(id){
  ns<-NS(id)
  tagList(
    h3("Welcome to QualiMon!"),
    uiOutput(ns('tutorial_start')),
    br(),
    h4("Step 1. XCMS processing before finding LaMa"),
    uiOutput(ns('tutorial_step1')),
    h4("Step 2. Finding LaMas in XCMS data"),
    uiOutput(ns("tutorial_step2"))
  )
}

tutorialServer<-function(id,r){
  moduleServer(
    id,
    function(input, output, session){
      ns<-session$ns

      output$tutorial_start <- renderUI({
        tagList(
         tags$p("In this tutorial you will learn how to use QualiMon to monitor the quality of your LC-MS data in \"real-time\". Using QualiMon is fairly straight forward, once setup, but to do that we need to set everything up."),
        )
      })

      output$tutorial_step1 <- renderUI({
        tagList(
          tags$p("The first step of setting up QualiMon is finding a number of Landmarks (LaMas) in data generated using the chromatography and polarity of the method you will want to monitor. To do this you will need to use XCMS and process a set of at least 10 samples which match the chrom. and pol. of the method you're setting up."),
          tags$p("Unfortunately, XCMS requires data-specific input to do a good job and so falls outside of the scope of this tutorial. A good guide for learning this procedure can be found ", tags$a(href="https://jorainer.github.io/metabolomics2018/xcms-preprocessing.html", "here.")),
          tags$p("Having gone through all the XCMS processing steps up until \"fillChromPeaks\" (which shoudn't be applied) for your dataset, save the object as a .rds file. This is done by using the commando \"saveRDS(object_name, \"file-name.rds\")\". This .rds-file will now be used to find a number of LaMa candidates for your setup.")
        )
      })

      output$tutorial_step2 <- renderUI({
        tagList(
          tags$p("To find LaMas in a dataset with the chromatography and polarity of your LC-MS method, go to the \"Find LaMas\" tab. Click the \"Select .rds file\" button and find your .rds file."),
          tags$p("Having chosen the file you will have to specify a number of instrument-specific parameters:"),
          tags$ul(
            tags$li(tags$b("Split: "), "The column name character that separates m/z and RT (e.g. 120.05692@220) of each feature."),
            tags$li(tags$b("Min RT to check: "),"Vaguely represents the dead volume of the LC system. The number of minutes from time 0 where you think that it would be useless to track LaMas due to various instrumental influences."),
            tags$li(tags$b("Prefilter intensity: "), "The prefilter intensity setting used for the XCMS processing of the data. This is instrument specific and determination of this parameter is done during XCMS processing."),
            tags$li(tags$b("Mode: "), "In QualiMon a 2-letter naming convention is used for chromatography and polarization."),
                    tags$ul(
                      tags$li(tags$b("RP: "),"Reversed phase Positive ionization"),
                      tags$li(tags$b("RN: "),"Reversed phase Negative ionization"),
                      tags$li(tags$b("HP: "),"HILIC Positive ionization"),
                      tags$li(tags$b("HN: "),"HILIC Negative ionization")
                    )
          )
        )
      })
    }
  )
}
