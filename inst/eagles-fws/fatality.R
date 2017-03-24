fatality <- function(input, output, session) {
  cur_dat <- reactive({
    if(input$sites == ""){Bay16}
    else{
      Bay16[Bay16$SITE == input$sites, ]
    }
  })
  cur_min <- reactive({cur_dat()$FLIGHT_MIN})
  cur_effort <- reactive({cur_dat()$EFFORT})
  cur_scale <- reactive({cur_dat()$SCALE})

  a <- reactive({mean(Bay16$FLIGHT_MIN) + cur_min()})

  b <- reactive({mean(Bay16$EFFORT) + cur_effort()})

  act <- reactive({cur_min()/cur_effort()})

  observeEvent(input$calculate,{
    if(input$sites != ""){
      out <- isolate({prediction(10000, a(), b())})
      fatality <- isolate({density(out$fatality*cur_scale())})
      q80 <- isolate({quantile(out$fatality, c(0.1, 0.9))})

      out2 <- isolate({prediction(10000, a()-mean(Bay16$FLIGHT_MIN), b()-mean(Bay16$EFFORT))})
      fatality2 <- isolate({density(out2$fatality*cur_scale())})
      q82 <- isolate({quantile(out2$fatality, c(0.1, 0.9))})

      output$fatal <- renderPlotly({
        plot_ly()%>%
          add_trace(x = ~fatality$x, y = ~fatality$y, type = "scatter", mode = "lines",
                    fill = "tozeroy",
                    name = "Incl. Prior Exposure", line = list(color = vir_col(3)[3]),
                    text = ~paste("Predicted fatalities<br>incorporating prior = ",
                                  round(fatality$x, 2),
                                  sep = ""),
                    hoverinfo = "text")%>%
          add_trace(x = ~fatality2$x, y = ~fatality2$y, type = "scatter", mode = "lines",
                    fill = "tozeroy",
                    line = list(color = vir_col(3)[2]), name = "Using Site Survey Only",
                    text = ~paste("Predicted fatalities<br>from site survey = ",
                                  round(fatality2$x, 2),
                                  sep = ""),
                    hoverinfo = "text")%>%
          #add_trace(x = ~c(q80[[1]], q80[[1]], q80[[2]], q80[[2]]), y = ~c(max(fatality2$y/fatality2$n), 0, 0, max(fatality2$y/fatality2$n)), type = "scatter", mode = "lines")%>%
          layout(##title = "Predicted Annual Eagle Fatalities",
                 xaxis = list(title = "Fatalities per Year",
                              range = c(0,20)),
                 yaxis = list(title = "Probability Density"),
                 legend = list(x = 0.7,
                               y = 1))
      })
    }
  })

}
