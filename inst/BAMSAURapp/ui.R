#ui for BAMSAURapp
ui <- fluidPage(
  tags$h2("Dental Age-at-Death Estimation",
          tags$style(
            "h2 {
            font-family: Calibri (bold);
            font-weight: 400;
            line-height: 1.0;
            color: #1b8bd1;
            }")
            ),
  navbarPage("BAMSAUR",

#1. Nonadult age-at-death estimation
    tabPanel("Nonadult",
             tags$br(),
             sidebarLayout(
               sidebarPanel(width = 5,
                            selectInput("pop1", "Select reference sample", choices = c("MB11", "other"), selected = "MB11"),
                            conditionalPanel(
                              condition = "input.pop1 == 'other'",
                            helpText("Indicate whether or not your file columns have headers"),
                            checkboxInput("header1", "Header", TRUE),
                            fluidRow(
                              column(5, radioButtons("sep1", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = ""),
                                                     selected = ",")),
                              column(5, radioButtons("dec1", "Decimal",
                                                     choices = c(Comma = ",",
                                                                 Point = "."),
                                                     selected = ","))
                            ),
                            fileInput("in1", "Upload file", buttonLabel = "Browse",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-seperated-values, text/plain",
                                                 ".csv")),
                            tags$hr()
                            ),
                            numericInput("wear1", label = "Insert wear score", value = 0, min = 0),
                            radioButtons("model1", label = "Select model", choices = c("linear", "quadratic", "cubic", "mars"), selected = "quadratic"),
                            radioButtons("interval1", label = "Select age interval type", choices = c("prediction", "confidence")),
                            checkboxInput("mars.int1", label = "MARS intervals", value = TRUE),
                            sliderInput("level1", label = "Select age interval level(%)", min = 1, max = 100, value = 68, step = 1),
                            conditionalPanel(
                              condition = "input.model1 == 'mars'",
                              checkboxInput("addopt", "Additional options"),
                              conditionalPanel(
                                condition = "input.addopt == true",
                                radioButtons("varmod.method1", label = "Variance method", choices = c("const","lm","rlm","earth","power"), selected = "earth"),
                                sliderInput("nfold1", label = "Number of folds", min = 3, max = 100, value = 49, step = 1),
                                sliderInput("ncross1", label =  "Number of cross validations", min = 3, max = 100, value = 3, step = 1)
                              )
                            ),
                            conditionalPanel(
                              condition = "input.pop1 == 'MB11'",
                              checkboxInput("addplot", "Show plot")
                            ),
                            hr(),
                            actionButton("calc1", "Calculate")
               ),
               mainPanel(width = 6,
                         tags$br(),
                         fluidRow(
                         column(6,
                                tags$h4("Age estimate"), textOutput("estimate1")),
                         column(6,
                         tags$h4("Age range"), textOutput("range1")),
                         tags$br(),

                         tags$style("#range1{
                                    color: #1b8bd1;
                                    font-size: 18px;
                                    }"),
                        tags$style("#estimate1{
                          color: #1b8bd1;
                          font-size: 18px;
                          }")),
      conditionalPanel(
        condition = "input.addplot == true",
        plotOutput("plot"))
               ))),

#3. Sample evaluation
    tabPanel("Sample evaluation",
             tags$br(),
      sidebarLayout(
        sidebarPanel(
          helpText("Indicate whether or not your file columns have headers"),
          checkboxInput("header", "Header", TRUE),
          helpText("Indicate the separation method for your file columns and the type of decimal"),
          fluidRow(
            column(5, radioButtons("sep", "Separator",
                                   choices = c(Comma = ",",
                                               Semicolon = ";",
                                               Tab = ""),
                                   selected = ",")),
            column(5, radioButtons("dec", "Decimal",
                                   choices = c(Comma = ",",
                                               Point = "."),
                                   selected = ","))
          ),
          fileInput("in3", "Upload file (.txt)", buttonLabel = "Browse",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-seperated-values, text/plain",
                               ".csv")),
          tags$hr(),
          helpText("Settings for LOOCV"),
          radioButtons(inputId = "interval3", label = "Select age interval type", choices = c("prediction", "confidence")),
          sliderInput(inputId = "level3", label = "Select age interval level(%)", min = 1, max = 100, value = 68, step = 1),
          helpText("Additional options for the evaluation of a MARS model"),
          checkboxInput("addopt3", "Additional options"),
          conditionalPanel(
            condition = "input.addopt3 == true",
            radioButtons("varmod.method3", label = "Variance method", choices = c("const","lm","rlm","earth","power"), selected = "earth"),
            sliderInput("nfold3", label = "Number of folds", min = 3, max = 100, value = 49, step = 1),
            helpText("Warning: increasing the ncross will increase the computation time"),
            sliderInput("ncross3", label =  "Number of cross validations", min = 3, max = 100, value = 3, step = 1)
            ),
            actionButton("eval3", "Evaluate")
          ),
        mainPanel(
          "Computation may take up to a minute",
          tabsetPanel(
            tabPanel("Summary",
              textOutput("upload3"),
                tags$style("#upload3{
                        font-size: 16px;
                        }"),
              tags$br(),
              verbatimTextOutput("out3"),
              tags$br(),
              helpText("PRESS = Predicted residual error sum of squares"),
              helpText("AIC = Akaike information criterion"),
              helpText("BIC = Bayes information criterion"),
              helpText("GCV = Generalised Cross Validation")
           ),
           tabPanel("Linear results",
              verbatimTextOutput("out3.lin"),
              dataTableOutput("linTable3"),
              downloadButton("evalLin", "Download table (.csv)")
           ),
           tabPanel("Quadratic results",
              verbatimTextOutput("out3.quad"),
              dataTableOutput("quadTable3"),
              downloadButton("evalQuad", "Download table (.csv)")
            ),
           tabPanel("Cubic results",
                    verbatimTextOutput("out3.cub"),
                    dataTableOutput("cubTable3"),
                    downloadButton("evalCub", "Download table (.csv)")
           ),
           tabPanel("MARS results",
                    verbatimTextOutput("out3.mars"),
                    dataTableOutput("marsTable3"),
                    downloadButton("evalMars", "Download table (.csv)")
           ),
           tabPanel("Linear plots",
              plotOutput("lin3"),
              plotOutput("lineval3")
            ),
           tabPanel("Quadratic plots",
            plotOutput("quad3"),
            plotOutput("quadeval3")
            ),
           tabPanel("Cubic plots",
                    plotOutput("cub3"),
                    plotOutput("cubeval3")
           ),
           tabPanel("MARS plots",
                    plotOutput("mars3"),
                    "MARS diagnostic plots can only be obtained in the R-console.
                    Use the following code:",
                    tags$br(),
                    "eval <- BAMSAUR.bff(data)",
                    tags$br(),
                    "mars <- eval$mars",
                    tags$br(),
                    "plot(mars)",
                    plotOutput("marseval3")
           )

        )
      )

    )),
#4 Wear calculator
    navbarMenu("Wear calculator",
               tabPanel("FDI numbering",
             tags$br(),
             tags$h4("Insert dental wear scores (FDI numbering system)"),
             helpText("Teeth not scored must be left blank"),
             checkboxInput("dec4", "Deciduous", value = FALSE),
             checkboxInput("per4", "Permanent", value = FALSE),
             tags$hr(),
             conditionalPanel(
               condition = "input.dec4 == true",
               tags$h4("Deciduous dentition"),
               tags$img(src = "FDInon.gif"),
               fluidRow(
                 column(2,
               numericInput("T51", "T51", min = 0, max = 8, value = NA, width = "100px"),
               numericInput("T52", "T52", min = 0, max = 8, value = NA, width = "100px"),
               numericInput("T53", "T53", min = 0, max = 8, value = NA, width = "100px"),
               numericInput("T54", "T54", min = 0, max = 8, value = NA, width = "100px"),
               numericInput("T55", "T55", min = 0, max = 8, value = NA, width = "100px")
                 ),
               column(2,
                numericInput("T61", "T61", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T62", "T62", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T63", "T63", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T64", "T64", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T65", "T65", min = 0, max = 8, value = NA, width = "100px")
               ),
               column(2,
                numericInput("T71", "T71", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T72", "T72", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T73", "T73", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T74", "T74", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T75", "T75", min = 0, max = 8, value = NA, width = "100px")
               ),
               column(2,
                numericInput("T81", "T81", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T82", "T82", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T83", "T83", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T84", "T84", min = 0, max = 8, value = NA, width = "100px"),
                numericInput("T85", "T85", min = 0, max = 8, value = NA, width = "100px")
               )
               )),
             conditionalPanel(
               condition = "input.per4 == true",
               tags$h4("Permanent dentition"),
               tags$img(src = "FDIadult.png"),
               fluidRow(
                 column(2,
               numericInput("T11", "T11", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T12", "T12", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T13", "T13", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T14", "T14", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T15", "T15", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T16", "T16", min = 8, max = 16, value = NA, width = "100px"),
               numericInput("T17", "T17", min = 8, max = 16, value = NA, width = "100px")
               ),
               column(2,
                      numericInput("T21", "T21", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T22", "T22", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T23", "T23", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T24", "T24", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T25", "T25", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T26", "T26", min = 8, max = 16, value = NA, width = "100px"),
                      numericInput("T27", "T27", min = 8, max = 16, value = NA, width = "100px")
             ),
             column(2,
                    numericInput("T31", "T31", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T32", "T32", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T33", "T33", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T34", "T34", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T35", "T35", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T36", "T36", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T37", "T37", min = 8, max = 16, value = NA, width = "100px")
             ),
            column(2,
                    numericInput("T41", "T41", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T42", "T42", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T43", "T43", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T44", "T44", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T45", "T45", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T46", "T46", min = 8, max = 16, value = NA, width = "100px"),
                    numericInput("T47", "T47", min = 8, max = 16, value = NA, width = "100px")
               ))),
            tags$hr(),
            tags$h5("Calculate average score"),
            actionButton("calc41", "Calculate"),
            textOutput("out41")
            ),
            tabPanel("Universal",
                     tags$br(),
                     tags$h4("Insert dental wear scores (Universal numbering system)"),
                     tags$img(src = "Usys.png"),
                      helpText("Teeth not scored must be left blank"),
                     checkboxInput("dec42", "Deciduous", value = FALSE),
                     checkboxInput("per42", "Permanent", value = FALSE),
                     tags$hr(),
                     conditionalPanel(
                       condition = "input.dec42 == true",
                       tags$h4("Deciduous dentition"),
                       fluidRow(
                         column(2,
                                numericInput("TE", "E", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TD", "D", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TC", "C", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TB", "B", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TA", "A", min = 0, max = 8, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("TF", "F", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TG", "G", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TH", "H", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TI", "I", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TJ", "J", min = 0, max = 8, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("TO", "O", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TN", "N", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TM", "M", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TL", "L", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TK", "K", min = 0, max = 8, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("TP", "P", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TQ", "Q", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TR", "R", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TS", "S", min = 0, max = 8, value = NA, width = "100px"),
                                numericInput("TT", "T", min = 0, max = 8, value = NA, width = "100px")
                         )
                       )),
                     conditionalPanel(
                       condition = "input.per42 == true",
                       tags$h4("Permanent dentition"),
                       fluidRow(
                         column(2,
                                numericInput("T2", "2", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T3", "3", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T4", "4", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T5", "5", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T6", "6", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T7", "7", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T8", "8", min = 8, max = 16, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("T9", "9", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T10", "10", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T11", "11", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T12", "12", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T13", "13", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T14", "14", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T15", "15", min = 8, max = 16, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("T18", "18", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T19", "19", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T20", "20", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T21", "21", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T22", "22", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T23", "23", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T24", "24", min = 8, max = 16, value = NA, width = "100px")
                         ),
                         column(2,
                                numericInput("T25", "25", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T26", "26", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T27", "27", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T28", "28", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T29", "29", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T30", "30", min = 8, max = 16, value = NA, width = "100px"),
                                numericInput("T31", "31", min = 8, max = 16, value = NA, width = "100px")
                        ))),
                     tags$hr(),
                     tags$h5("Calculate average score"),
                     actionButton("calc42", "Calculate"),
                     textOutput("out42")
                     )


            ),

#5. Reference samples info page
    tabPanel("Reference samples",
             textOutput("note4"),
             tags$br(),
              sidebarLayout(
               sidebarPanel(
                 selectInput("pop", "Select reference sample", choices = c("MB11", "other"), selected = "MB11")

               ),
               mainPanel(
                 tags$img(
                   width = 250,
                   height = 300,
                   src = "Middenbeemster.png"),
                 tags$br(),
                 textOutput("MB11"),
                 tags$br(),
                    tags$style("#note4{
                            color: #1b8bd1;
                            font-size: 14px;
                            }")
               )
             ))
  )
)

