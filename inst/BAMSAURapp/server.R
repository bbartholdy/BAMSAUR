#server for BAMSAURapp
server <- function(input, output) {

#1. Nonadult age-at-death estimation results
#Find a way to incorporate pop == other
output$out1 <- renderText({
  req(input$in1)
  df <- read.table(input$in3$datapath,
                   header = input$header,
                   sep = input$sep,
                   dec = input$dec)
  paste("Upload successful")
})
BAMSAUR.1 <-  eventReactive(input$calc1, {
                if(input$pop1 == "other"){inData1 <- read.table(input$in1$datapath,
                                header = input$header1,
                                sep = input$sep1,
                                dec = input$dec1)
                }
  BAMSAUR(wear = input$wear1, data = inData1, pop = input$pop1, model = input$model1, interval = input$interval1, level = input$level1/100, varmod.method = input$varmod.method1, nfold = input$nfold1, ncross = input$ncross1)})
  output$estimate1 <- renderText({ result1 <- BAMSAUR.1()
  paste(result1$estimate, " +- ", result1$`+- years`, " years")
  })
  output$range1 <- renderText({result1 <- BAMSAUR.1()
  paste(result1$lower, " - ", result1$upper, " years")
  })
  BAMplot.1 <- eventReactive(input$calc1, {BAMSAUR:::BAM.plot2(x = MBsimple$Wear, y = MBsimple$Age, model = input$model1, interval = input$interval1, level = input$level1/100)})
  output$plot <- renderPlot({result1 <- BAMSAUR.1()
  plot <- BAMplot.1()
  plot + ggplot2::geom_point(ggplot2::aes(x = input$wear1, y = result1$estimate), data = NULL, size = 2.5, colour = rgb(0.8,0.1,0))
  })

#2. Adult age-at-death estimation

#3. Sample evaluation
  output$upload3 <- renderText({
    req(input$in3)
    df <- read.table(input$in3$datapath,
                     header = input$header,
                     sep = input$sep,
                     dec = input$dec)
    paste("Upload successful")
  })
  BAMSAURbff.31 <- eventReactive(input$eval3, {
   inData3 <- read.table(input$in3$datapath,
               header = input$header,
               sep = input$sep,
               dec = input$dec)
  BAMSAUR:::BAMSAUR.bff2(inData3, interval = input$interval3, level = input$level3/100, varmod.method = input$varmod.method3, nfold = input$nfold3, ncross = input$ncross3)})
  output$out3 <- renderPrint({result3 <- BAMSAURbff.31()
  output$out3.lin <- renderPrint(result3$linear)
  output$out3.quad <- renderPrint(result3$quadratic)
  output$out3.cub <- renderPrint(result3$cubic)
  output$out3.mars <- renderPrint(result3$mars)
  output$linTable3 <- renderDataTable(result3$lin.data)
  output$quadTable3 <- renderDataTable(result3$quad.data)
  output$cubTable3 <- renderDataTable(result3$cub.data)
  output$marsTable3 <- renderDataTable(result3$mars.data)
  output$lin3 <- renderPlot(result3$lin.plot)
  output$lineval3 <- renderPlot(result3$linplot.eval)
  output$quad3 <- renderPlot(result3$quad.plot)
  output$quadeval3 <- renderPlot(result3$quadplot.eval)
  output$cub3 <- renderPlot(result3$cub.plot)
  output$cubeval3 <- renderPlot(result3$cubplot.eval)
  output$mars3 <- renderPlot(result3$mars.plot)
  })

#output for LOOCV: accuracy for set interval, % within 1 year, and % within 2 years

  linData3 <- reactive({result31 <- BAMSAURbff.31()
                     result31$lin.data})
  quadData3 <- reactive({result31 <- BAMSAURbff.31()
                    result31$quad.data})
  cubData3 <- reactive({result31 <- BAMSAURbff.31()
                    result31$cub.data})
  marsData3 <- reactive({result31 <- BAMSAURbff.31()
                    result31$mars.data})

  output$evalLin <- downloadHandler(
    filename = function() {
      paste("linTable3", ".csv", sep = "")
    },
      content = function(file) {
      write.csv(linData3(), file)
    }
  )
  output$evalQuad <- downloadHandler(
    filename = function() {
      paste("quadTable3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(quadData3(), file)
    }
  )
  output$evalCub <- downloadHandler(
    filename = function() {
      paste("cubTable3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cubData3(), file)
    }
  )
  output$evalMars <- downloadHandler(
    filename = function() {
      paste("marsTable3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(marsData3(), file)
    }
  )

#4. Wear data
Wear.41 <- eventReactive(input$calc41, {score <- c(input$T11, input$T12, input$T13, input$T14, input$T15, input$T16, input$T17,
                  input$T21, input$T22, input$T23, input$T24, input$T25, input$T26, input$T27,
                  input$T31, input$T32, input$T33, input$T34, input$T35, input$T36, input$T37,
                  input$T41, input$T42, input$T43, input$T44, input$T45, input$T46, input$T47,
                    input$T51, input$T52, input$T53, input$T54, input$T55,
                    input$T61, input$T62, input$T63, input$T64, input$T65,
                    input$T71, input$T72, input$T73, input$T74, input$T75,
                    input$T81, input$T82, input$T83, input$T84, input$T85)
  print(mean(score, na.rm = TRUE))})
output$out41 <- renderPrint({Wear.41()})

Wear.42 <- eventReactive(input$calc42, {score <- c(input$T2, input$T3, input$T4, input$T5, input$T6, input$T7, input$T8,
                    input$T9, input$T10, input$T11, input$T12, input$T13, input$T14, input$T15,
                    input$T18, input$T19, input$T20, input$T21, input$T22, input$T23, input$T24,
                    input$T25, input$T26, input$T27, input$T28, input$T29, input$T30, input$T31,
                    input$TA, input$TB, input$TC, input$TD, input$TE,
                    input$TF, input$TG, input$TH, input$TI, input$TJ,
                    input$TK, input$TL, input$TM, input$TN, input$TO,
                    input$TP, input$TQ, input$TR, input$TS, input$TT)
print(mean(score, na.rm = TRUE))})
output$out42 <- renderPrint({Wear.42()})


#5. Reference population info
  output$MB11 <- renderText("Middenbeemster is a post-Medieval Dutch skeletal collection housed at Leiden University.
                            The collection contains a number of individuals with documented ages-at-death obtained from a cemetery ledger.
                            The cemetery is associated with the Keyserkerk church, where the inhabitants of the Middenbeemster village and the surrounding Beemsterpolder were buried between AD 1612 and 1866.
                            The main occupation of the inhabitants was dairy farming, and their diet consisted mainly of wheat or rye bread, potatoes, eggs, and dairy products.")
  output$note4 <- renderText("If you would like to submit a reference collection, please contact Bjorn at b.p.bartholdy@arch.leidenuniv.nl")
  output$pic4 <- renderImage({

  })
}
