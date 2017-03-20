bayesian <- function(input, output, session) {
  cur_min <- reactive({Bay16$FLIGHT_MIN[Bay16$SITE == input$sites]})
  cur_effort <- reactive({Bay16$EFFORT[Bay16$SITE == input$sites]})

  a <- reactive({mean(Bay16$FLIGHT_MIN)+cur_min()})

  b <- reactive({mean(Bay16$EFFORT)+ cur_effort()})

  act <- reactive({cur_min()/cur_effort()})

  observeEvent(input$update,{
    act <- isolate({act()})
    obs <- isolate({density(rgamma(10000, shape = a(), rate = b()))})

    output$exposure <- renderPlotly({
      plot_ly()%>%
        add_trace(x = ~prior$x, y = prior$y,
                  type = "scatter", mode = "lines", fill = "tozeroy",
                  name = "Prior", line = list(color = "orange"),
                  text = ~paste("Prior probability of Exposure = ",
                                round(prior$x, 2),
                                "<br> is ",
                                round(prior$y, 2),
                                sep = ""),
                  hoverinfo = "text")%>%
        add_trace(x = ~c(act, act), y = ~c(0,max(c(prior$y,obs$y))),
                  type = "scatter", mode = "lines",
                  name = "Observed", line = list(color = "green"),
                  text = ~paste("Observed Exposure = ",
                                round(act,2),
                                sep = ""),
                  hoverinfo = "text")%>%
        add_trace(x = ~obs$x, y = ~obs$y,
                  type = "scatter", mode = "lines", fill = "tozeroy",
                  name = "Posterior", line = list(color = "blue"),
                  text = ~paste("Posterior probability of Exposure = ",
                                round(obs$x, 2),
                                "<br> is ",
                                round(obs$y, 2),
                                sep = ""),
                  hoverinfo = "text")%>%
        layout(title = "Eagle Exposure",
               xaxis = list(title = "Exposure (min/km3*hr)",
                            range = c(0,3)),
               yaxis = list(title = "Probability"))
    })

  })

  observeEvent(input$calculate,{
    out <- isolate({prediction(10000, a(), b())})
    fatality <- isolate({density(out$fatality)})
    q80 <- isolate({quantile(out$fatality, c(0.1, 0.9))})
    out2 <- isolate({prediction(10000, a()-mean(Bay16$FLIGHT_MIN), b()-mean(Bay16$EFFORT))})
    fatality2 <- isolate({density(out2$fatality)})
    q82 <- isolate({quantile(out2$fatality, c(0.1, 0.9))})

    output$fatal <- renderPlotly({
      plot_ly()%>%
        add_trace(x = ~fatality$x, y = ~fatality$y/fatality$n, type = "scatter", mode = "lines", fill = "tozeroy",
                  name = "Using Exposure Prior", line = list(color = "purple"),
                  text = ~paste("Posterior probability of<br>",
                                round(fatality$x, 2),
                                " fatalities is ",
                                round(fatality$y/fatality$n, 2),sep = ""),
                  hoverinfo = "text")%>%
        add_trace(x = ~fatality2$x, y = ~fatality2$y/fatality2$n, type = "scatter", mode = "lines", fill = "tozeroy",
                  line = list(color = "green"), name = "Using Site Survey Only",
                  text = ~paste("Posterior probability of<br>",
                                round(fatality2$x, 2),
                                " fatalities is ",
                                round(fatality2$y/fatality2$n, 2),
                                sep = ""),
                  hoverinfo = "text")%>%
        #add_trace(x = ~c(q80[[1]], q80[[1]], q80[[2]], q80[[2]]), y = ~c(max(fatality2$y/fatality2$n), 0, 0, max(fatality2$y/fatality2$n)), type = "scatter", mode = "lines")%>%
        layout(title = "Predicted Annual Eagle Fatalities",
               xaxis = list(title = "Fatalities per Year"),
               yaxis = list(title = "Probability"))
    })
  })

  output$prior <- renderPlotly({
  plot_ly()%>%
  add_trace(x = ~collision$x, y = ~collision$y/collision$n, type = "scatter", mode = "lines",
            fill = "tozeroy", name = "Prior", line = list(color = "red"),
            text = ~paste("Prior probability of Collision Rate = ",
                          round(collision$x, 3),
                          "<br> is ",
                          round(collision$y/collision$n, 3),
                          sep = ""),
            hoverinfo = "text")%>%
      layout(title = "Prior Collision Rates",
             xaxis = list(title = "Collision Rate (per Exposure)"),
             yaxis = list(title = "Probability"))
  })
}
