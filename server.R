source("functions.R", local = T)

shinyServer(function(input, output) {
  Sys.setenv("plotly_username" = "why94nb")
  Sys.setenv("plotly_api_key" = "uhobbp8rdn")
  observeEvent(input$goButton,{
      set.seed(input$seed)
      shiny::validate(
        need(input$drug, message = "Please select a drug effect")
      )
      if (length(input$drug) == 1){
        if (input$drug == 'Symptomatic'){druginput = "Symptomatic"}
        if (input$drug == 'Disease Modifying'){druginput = "Disease Modifying"}
      }
      else {druginput = "Both"}


      if (input$design == "Cross-over Design"){
          shiny::validate(
            need((input$firstduration*2+input$washout) %% input$frequency == 0, message = "Please select another frequency")
          )
          withProgress(message = "Simulation Running",detail = "It may take a while...",{
              xover <- xover_one(n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                                 ubMmse.use = isolate(input$bmmse[2]),emax1 = isolate(input$Emax2),
                                 et50 = isolate(input$et50), drug = druginput,
                                 et50wash = isolate(input$et50wash), edm = isolate(input$eDm2),
                                 firstduration = input$firstduration,washout = input$washout,
                                 dropout = isolate(input$dropout),frequency=isolate(input$frequency))
              pla <- xover$stat[xover$stat$DrugDose == "Placebo",]
              trt <- xover$stat[xover$stat$DrugDose == "Treatment",]
            })
            output$plot1 <- renderPlotly( xover$plot)
            plotly_IMAGE(xover$plot,out_file = "plot1.png")
            output$table1 <- DT::renderDataTable( round(t(as.matrix(xover$test)),3),
                                              options = list(scrollX = T))
            output$table2 <- DT::renderDataTable(xover$stat,rownames = F,
                              filter = 'bottom', extensions = 'Buttons', options = list(
                                      dom = 'Bfrtip',buttons = c('colvis','copy','print'),scrollX = T
                                    )
                                    )
            output$table3 <- DT::renderDataTable(xover$stat1,rownames = F,
                                                 filter = 'bottom', extensions = 'Buttons', options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = c('colvis','copy','print'),scrollX = T
                                                 ))
            output$download1 <- downloadHandler(
              filename = function(){
                paste('patientInfo-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(xover$stat,con)
              }
            )
            output$download2 <- downloadHandler(
              filename = function(){
                paste('longitudinalData-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(xover$stat1,con)
              }
            )

            output$summary2 <- renderPlotly(xover$plot1)
            plotly_IMAGE(xover$plot1,out_file = "plot2.png")

            r1 <- c(length(which(xover$stat$Gender == 1)),length(which(xover$stat$Gender == 0)))
            r2 <- c(length(which(pla$Gender == 1)),length(which(pla$Gender == 0)))
            r3 <- c(length(which(trt$Gender == 1)),length(which(trt$Gender == 0)))
            r <- rbind(r1,r2,r3)
            r4 <- c("Total","Placebo","Treatment")
            p1 <- plot_ly( x=r4, y=r[,1], name="Female",type = "bar",opacity = 0.6) %>%
              add_trace(x=r4,y=r[,2],name="Male",type="bar",opacity = 0.6) %>%
              layout(xaxis=list(title = ""),yaxis=list(title="No. of Obs"))
            plotly_IMAGE(p1,out_file = "plot3.png")
            output$summary1 <- renderPlotly(p1)

            ds <- data.frame(labels = c("0","1","2"),values =
                               c(length(which(pla$ApoE == 0)),length(which(pla$ApoE == 1)),
                                 length(which(pla$ApoE == 2))))
            p2 <- plot_ly(ds, labels = labels, values = values, type = "pie",
                    domain = list(x = c(0, 0.4), y = c(0.4, 1)),
                    name = "Placebo", showlegend = T,marker=list(colors =
                                                                   brewer.pal(3, "Set2")),opacity=0.8
            ) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(xover$stat$ApoE == 0)),
                                                     length(which(xover$stat$ApoE == 1)),
                                                     length(which(xover$stat$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(xover$stat$ApoE == 0)),
                            length(which(xover$stat$ApoE == 1)),
                            length(which(xover$stat$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.25,0.75), y = c(0, 0.6)),
                        name = "Total", showlegend = F,opacity=0.8) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(trt$ApoE == 0)),
                                                     length(which(trt$ApoE == 1)),
                                                     length(which(trt$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(trt$ApoE == 0)),
                            length(which(trt$ApoE == 1)),
                            length(which(trt$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                        name = "Treatment", showlegend = F,opacity=0.8)
            plotly_IMAGE(p2,out_file = "plot4.png")
            output$summary3 <- renderPlotly(p2)

            p3 <- plot_ly(y = trt$Age, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Age, type = "box",name = "Placebo") %>%
              add_trace(y = xover$stat$Age,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Age"))
            plotly_IMAGE(p3,out_file = "plot5.png")
            output$summary4 <- renderPlotly(p3)

            p4 <- plot_ly(y = trt$Bmmse, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Bmmse, type = "box",name = "Placebo") %>%
              add_trace(y = xover$stat$Bmmse,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Baseline MMSE"))
            plotly_IMAGE(p4,out_file = "plot6.png")
            output$summary5 <- renderPlotly(p4)
           }

      if (input$design == "Parallel Group Design"){
          withProgress(message = "Simulation Running",detail = "It may take a while...",{
            parallel <- parallel_one(n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                                     ubMmse.use = isolate(input$bmmse[2]),emax2 = isolate(input$Emax2),
                                     et50 = isolate(input$et50), drug = druginput,
                                     et50wash = isolate(input$et50wash), edm = c(0,isolate(input$eDm2)),
                                     duration = input$duration, dropout = isolate(input$dropout))
            pla <- parallel$stat[parallel$stat$DrugDose == "Placebo",]
            trt <- parallel$stat[parallel$stat$DrugDose == "Treatment",]
            })
            output$plot1 <- renderPlotly(parallel$plot)
            plotly_IMAGE(parallel$plot,out_file = "plot1.png")
            output$table1 <- DT::renderDataTable(round(as.matrix(parallel$test),3),
                                                 rownames = F,options = list(scrollX = T))
            output$table2 <- DT::renderDataTable(parallel$stat,
                                                 rownames = F,filter = 'bottom', extensions = 'Buttons', options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = c('colvis','copy','print'),scrollX = T
                                                 ))
            output$table3 <- DT::renderDataTable(parallel$stat1,rownames = F,
                                                 filter = 'bottom', extensions = 'Buttons', options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = c('colvis','copy','print'),scrollX = T
                                                 ))
            output$download1 <- downloadHandler(
              filename = function(){
                paste('patientInfo-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(parallel$stat,con)
              }
            )
            output$download2 <- downloadHandler(
              filename = function(){
                paste('longitudinalData-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(parallel$stat1,con)
              }
            )
            output$summary2 <- renderPlotly(parallel$plot1)
            plotly_IMAGE(parallel$plot1,out_file = "plot2.png")

            r1 <- c(length(which(parallel$stat$Gender == 1)),length(which(parallel$stat$Gender == 0)))
            r2 <- c(length(which(pla$Gender == 1)),length(which(pla$Gender == 0)))
            r3 <- c(length(which(trt$Gender == 1)),length(which(trt$Gender == 0)))
            r <- rbind(r1,r2,r3)
            r4 <- c("Total","Placebo","Treatment")
            p1 <- plot_ly( x=r4, y=r[,1], name="Female",type = "bar",opacity = 0.6) %>%
              add_trace(x=r4,y=r[,2],name="Male",type="bar",opacity = 0.6) %>%
              layout(xaxis=list(title = ""),yaxis=list(title="No. of Obs"))
            plotly_IMAGE(p1,out_file = "plot3.png")
            output$summary1 <- renderPlotly(p1)

            ds <- data.frame(labels = c("0","1","2"),values =
                               c(length(which(pla$ApoE == 0)),length(which(pla$ApoE == 1)),
                                 length(which(pla$ApoE == 2))))
            p2 <- plot_ly(ds, labels = labels, values = values, type = "pie",
                    domain = list(x = c(0, 0.4), y = c(0.4, 1)),
                    name = "Placebo", showlegend = T,marker=list(colors =
                                                                   brewer.pal(3, "Set2")),opacity=0.8
            ) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(parallel$stat$ApoE == 0)),
                                                     length(which(parallel$stat$ApoE == 1)),
                                                     length(which(parallel$stat$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(parallel$stat$ApoE == 0)),
                            length(which(parallel$stat$ApoE == 1)),
                            length(which(parallel$stat$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.25,0.75), y = c(0, 0.6)),
                        name = "Total", showlegend = F,opacity=0.8) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(trt$ApoE == 0)),
                                                     length(which(trt$ApoE == 1)),
                                                     length(which(trt$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(trt$ApoE == 0)),
                            length(which(trt$ApoE == 1)),
                            length(which(trt$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                        name = "Treatment", showlegend = F,opacity=0.8)
            plotly_IMAGE(p2,out_file = "plot4.png")
            output$summary3 <- renderPlotly(p2)

            p3 <- plot_ly(y = trt$Age, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Age, type = "box",name = "Placebo") %>%
              add_trace(y = parallel$stat$Age,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Age"))
            plotly_IMAGE(p3,out_file = "plot5.png")
            output$summary4 <- renderPlotly(p3)

            p4 <- plot_ly(y = trt$Bmmse, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Bmmse, type = "box",name = "Placebo") %>%
              add_trace(y = parallel$stat$Bmmse,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Baseline MMSE"))
            plotly_IMAGE(p4,out_file = "plot6.png")
            output$summary5 <- renderPlotly(p4)
          }

      if (input$design == "Delayed Start Design"){
          withProgress(message = "Simulation Running",detail = "It may take a while...",{
            delay <- delay_one(n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                               ubMmse.use = isolate(input$bmmse[2]),emax2 = isolate(input$Emax2),
                               et50 = isolate(input$et50), drug = druginput,
                               et50wash = isolate(input$et50wash), edm = rep(isolate(input$eDm2),2),
                               period = input$period, start = input$start,
                               dropout = isolate(input$dropout))
            pla <- delay$stat[delay$stat$DrugDose == "Placebo",]
            trt <- delay$stat[delay$stat$DrugDose == "Treatment",]
            })
            output$plot1 <- renderPlotly(delay$plot)
            plotly_IMAGE(delay$plot,out_file = "plot1.png")
            output$table1 <- DT::renderDataTable(round(t(as.matrix(delay$test)),3),
                                                 options = list(scrollX = T))
            output$table2 <- DT::renderDataTable(delay$stat,rownames = F,filter = 'bottom', extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('colvis','copy','print'),scrollX = T
            ))
            output$table3 <- DT::renderDataTable(delay$stat1,rownames = F,filter = 'bottom', extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = c('colvis','copy','print'),scrollX = T
            ))
            output$download1 <- downloadHandler(
              filename = function(){
                paste('patientInfo-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(delay$stat,con)
              }
            )
            output$download2 <- downloadHandler(
              filename = function(){
                paste('longitudinalData-',Sys.Date(),'.csv',sep='')
              },
              content = function(con){
                write.csv(delay$stat1,con)
              }
            )
            output$summary2 <- renderPlotly(delay$plot1)
            plotly_IMAGE(delay$plot1,out_file = "plot2.png")

            r1 <- c(length(which(delay$stat$Gender == 1)),length(which(delay$stat$Gender == 0)))
            r2 <- c(length(which(pla$Gender == 1)),length(which(pla$Gender == 0)))
            r3 <- c(length(which(trt$Gender == 1)),length(which(trt$Gender == 0)))
            r <- rbind(r1,r2,r3)
            r4 <- c("Total","Placebo","Treatment")
            p1 <- plot_ly( x=r4, y=r[,1], name="Female",type = "bar",opacity = 0.6) %>%
              add_trace(x=r4,y=r[,2],name="Male",type="bar",opacity = 0.6) %>%
              layout(xaxis=list(title = ""),yaxis=list(title="No. of Obs"))
            plotly_IMAGE(p1,out_file = "plot3.png")
            output$summary1 <- renderPlotly(p1)

            ds <- data.frame(labels = c("0","1","2"),values =
                               c(length(which(pla$ApoE == 0)),length(which(pla$ApoE == 1)),
                                 length(which(pla$ApoE == 2))))
            p2 <- plot_ly(ds, labels = labels, values = values, type = "pie",
                    domain = list(x = c(0, 0.4), y = c(0.4, 1)),
                    name = "Placebo", showlegend = T,marker=list(colors =
                                                                   brewer.pal(3, "Set2")),opacity=0.8
            ) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(delay$stat$ApoE == 0)),
                                                     length(which(delay$stat$ApoE == 1)),
                                                     length(which(delay$stat$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(delay$stat$ApoE == 0)),
                            length(which(delay$stat$ApoE == 1)),
                            length(which(delay$stat$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.25,0.75), y = c(0, 0.6)),
                        name = "Total", showlegend = F,opacity=0.8) %>%
              add_trace(data = data.frame(labels = c("0","1","2"),
                                          values = c(length(which(trt$ApoE == 0)),
                                                     length(which(trt$ApoE == 1)),
                                                     length(which(trt$ApoE == 2)))),
                        labels = c("0","1","2"),values =
                          c(length(which(trt$ApoE == 0)),
                            length(which(trt$ApoE == 1)),
                            length(which(trt$ApoE == 2))),type = "pie",
                        domain = list(x = c(0.6, 1), y = c(0.4, 1)),
                        name = "Treatment", showlegend = F,opacity=0.8)
            plotly_IMAGE(p2,out_file = "plot4.png")
            output$summary3 <- renderPlotly(p2)

            p3 <- plot_ly(y = trt$Age, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Age, type = "box",name = "Placebo") %>%
              add_trace(y = delay$stat$Age,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Age"))
            plotly_IMAGE(p3,out_file = "plot5.png")
            output$summary4 <- renderPlotly(p3)

            p4 <- plot_ly(y = trt$Bmmse, type = "box",name = "Treatment") %>%
              add_trace(y = pla$Bmmse, type = "box",name = "Placebo") %>%
              add_trace(y = delay$stat$Bmmse,type = "box", name = "Total") %>%
              layout(xaxis=list(title = ""),yaxis=list(title="Baseline MMSE"))
            plotly_IMAGE(p4,out_file = "plot6.png")
            output$summary5 <- renderPlotly(p4)
          }
          })


  observeEvent(input$goButton1,{
    set.seed(input$seed)
    shiny::validate(
      need(input$drug, message = "Please select a drug effect")
    )
    if (length(input$drug) == 1){
      if (input$drug == 'Symptomatic'){druginput = "Symptomatic"}
      if (input$drug == 'Disease Modifying'){druginput = "Disease Modifying"}
    }
    else {druginput = "Both"}
    if (input$design == "Cross-over Design"){
      shiny::validate(
        need((input$firstduration*2+input$washout) %% input$frequency == 0, message = "Please select another frequency")
      )
      withProgress(message = "Simulation Running",detail = "It may take a while...",{
      xover <- xover_sim(nSim = isolate(input$nSim),n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                         ubMmse.use = isolate(input$bmmse[2]),emax1 = isolate(input$Emax2),
                         et50 = isolate(input$et50), drug = druginput,
                         et50wash = isolate(input$et50wash), edm = isolate(input$eDm2),
                         firstduration = isolate(input$firstduration),washout = isolate(input$washout),
                         dropout = isolate(input$dropout),frequency=isolate(input$frequency))
      })

      output$simsum1 <- renderPlotly({xover$plot1})
      plotly_IMAGE(xover$plot1,out_file = "plot7.png")
      output$simsum4 <- renderPlotly({xover$plot2})
      plotly_IMAGE(xover$plot2,out_file = "plot8.png")

      p1 <- plot_ly(x = 1:input$nSim,y = xover$res[,1], error_y = list(symmetric =
                   F, arrayminus = xover$res[,1] - xover$res[,3],
                  array = xover$res[,4] - xover$res[,1]), mode = "markers") %>%
        layout(xaxis = list(title = "Simulations"),
               yaxis = list(title = "Contrast"))
      plotly_IMAGE(p1,out_file = "plot9.png")
      output$simsum2 <- renderPlotly(p1)

      prop <- sum(xover$res[, 'Pvalue'] < 0.05)
      ds <- data.frame(labels = c("P-value < 0.05", "P-value > 0.05"),values =
                         c(prop,nrow(xover$res)-prop))
      p2 <- plot_ly(ds,labels = labels, values = values, type = "pie")
      plotly_IMAGE(p2,out_file = "plot10.png")
      output$simsum3 <- renderPlotly(p2)
    }

    if (input$design == "Parallel Group Design"){
      withProgress(message = "Simulation Running",detail = "It may take a while...",{
        parallel <- parallel_sim(nSim = isolate(input$nSim),n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                                 ubMmse.use = isolate(input$bmmse[2]),emax2 = isolate(input$Emax2),
                                 et50 = isolate(input$et50), drug = druginput,
                                 et50wash = isolate(input$et50wash), edm = c(0,isolate(input$eDm2)),
                                 duration = isolate(input$duration), dropout = isolate(input$dropout))
      })

      output$simsum1 <- renderPlotly({parallel$plot1})
      plotly_IMAGE(parallel$plot1,out_file = "plot7.png")
      output$simsum4 <- renderPlotly({parallel$plot2})
      plotly_IMAGE(parallel$plot2,out_file = "plot8.png")

      p1 <- plot_ly(x = 1:input$nSim,y = parallel$res[,1], error_y = list(symmetric = F, arrayminus = parallel$res[,1] - parallel$res[,3],
                  array = parallel$res[,4] - parallel$res[,1]), mode = "markers") %>%
        layout(xaxis = list(title = "Simulations"),
               yaxis = list(title = "Contrast"))
      plotly_IMAGE(p1,out_file = "plot9.png")
      output$simsum2 <- renderPlotly(p1)

      prop <- sum(parallel$res[, 6] < 0.05)
      ds <- data.frame(labels = c("P-value < 0.05", "P-value > 0.05"),values =
                         c(prop,nrow(parallel$res)-prop))
      p2 <- plot_ly(ds,labels = labels, values = values, type = "pie")
      plotly_IMAGE(p2,out_file = "plot10.png")
      output$simsum3 <- renderPlotly(p2)
    }

    if (input$design == "Delayed Start Design"){
      withProgress(message = "Simulation Running",detail = "It may take a while...",{
        delay <- delay_sim(nSim = isolate(input$nSim),n.per.arm = isolate(input$n.per.arm),lbMmse.use = isolate(input$bmmse[1]),
                                 ubMmse.use = isolate(input$bmmse[2]),emax2 = isolate(input$Emax2),
                                 et50 = isolate(input$et50), drug = druginput,
                                 et50wash = isolate(input$et50wash), edm = rep(isolate(input$eDm2),2),
                           period = isolate(input$period), start = isolate(input$start),
                           dropout = isolate(input$dropout))
      })

      output$simsum1 <- renderPlotly({delay$plot1})
      plotly_IMAGE(delay$plot1,out_file = "plot7.png")
      output$simsum4 <- renderPlotly({delay$plot2})
      plotly_IMAGE(delay$plot2,out_file = "plot8.png")

      p1 <- plot_ly(x = 1:input$nSim, y = delay$res[,1], error_y = list(symmetric = F, arrayminus = delay$res[,1] - delay$res[,3],
                                                                  array = delay$res[,4] - delay$res[,1]), mode = "markers") %>%
        layout(xaxis = list(title = "Simulations"),
               yaxis = list(title = "Contrast"))
      plotly_IMAGE(p1,out_file = "plot9.png")
      output$simsum2 <- renderPlotly(p1)

      prop <- sum(delay$res[, 6] < 0.05)
      ds <- data.frame(labels = c("P-value < 0.05", "P-value > 0.05"),values =
                         c(prop,nrow(delay$res)-prop))
      p2 <- plot_ly(ds,labels = labels, values = values, type = "pie")
      plotly_IMAGE(p2,out_file = "plot10.png")
      output$simsum3 <- renderPlotly(p2)

      p3 <- plot_ly(x = 1:input$nSim, y = delay$res[,7], error_y = list(symmetric = F, arrayminus = delay$res[,7] - delay$res[,9],
                                                                  array = delay$res[,10] - delay$res[,7]), mode = "markers") %>%
        layout(xaxis = list(title = "Simulations"),
               yaxis = list(title = "Contrast"))
      plotly_IMAGE(p3,out_file = "plot11.png")
      output$simsum5 <- renderPlotly(p3)

      prop <- sum(delay$res[, 12] < 0.05)
      ds <- data.frame(labels = c("P-value < 0.05", "P-value > 0,05"),values =
                         c(prop,nrow(delay$res)-prop))
      p4 <- plot_ly(ds,labels = labels, values = values, type = "pie")
      plotly_IMAGE(p4,out_file = "plot12.png")
      output$simsum6 <- renderPlotly(p4)
    }

    if (input$design != "Delayed Start Design"){
      output$downloadreport <- downloadHandler(
        filename = function() {
          paste('my-report', sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },

        content = function(file) {
          src <- normalizePath('myreport.Rmd')
          file.copy(src, 'myreport.Rmd')
          library(rmarkdown)
          out <- render('myreport.Rmd', switch(
            input$format,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
    }
    else {
      output$downloadreport <- downloadHandler(
        filename = function() {
          paste('my-report', sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },

        content = function(file) {
          src <- normalizePath('myreport1.Rmd')
          file.copy(src, 'myreport1.Rmd')
          library(rmarkdown)
          out <- render('myreport1.Rmd', switch(
            input$format,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
    }
  })
})

