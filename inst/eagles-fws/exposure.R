exposure <- function(input, output, session) {
  cur_min <- reactive({Bay16$FLIGHT_MIN[Bay16$SITE == input$sites]})
  cur_effort <- reactive({Bay16$EFFORT[Bay16$SITE == input$sites]})
  cur_scale <- reactive({Bay16$SCALE[Bay16$SITE == input$sites]})

  a <- reactive({mean(Bay16$FLIGHT_MIN) + cur_min()})

  b <- reactive({mean(Bay16$EFFORT) + cur_effort()})

  act <- reactive({cur_min()/cur_effort()})

  observeEvent(input$update,{
    act <- isolate({act()})
    obs <- isolate({curve(dgamma(x, shape = a(), rate = b()),
                          from = quantile(rgamma(1000, shape = a(), rate = b()) , probs = 0),
                          to = quantile(rgamma(1000, shape = a(), rate = b()) , probs = 1))
        })

    output$exposure <- renderPlotly({
      plot_ly()%>%
        add_trace(x = ~c(act, act), y = ~c(0,max(c(prior$y,obs$y))),
                  type = "scatter", mode = "lines",
                  name = "Observed", line = list(color = vir_col(3)[2]),
                  text = ~paste("Observed Activity<br>at site = ",
                                round(act,2),
                                sep = ""),
                  hoverinfo = "text")%>%
        add_trace(x = ~prior$x, y = prior$y,
                  type = "scatter", mode = "lines", fill = "tozeroy",
                  name = "Prior", line = list(color = vir_col(3)[1]),
                  text = ~paste("Prior Activity<br>estimate = ",
                                round(prior$x, 2),
                                "<br> is ",
                                round(prior$y, 2),
                                sep = ""),
                  hoverinfo = "text")%>%
        add_trace(x = ~obs$x, y = ~obs$y,
                  type = "scatter", mode = "lines", fill = "tozeroy",
                  name = "Combined", line = list(color = vir_col(3)[3]),
                  text = ~paste("Combined Activity<br>estimate = ",
                                round(obs$x, 2),
                                "<br> is ",
                                round(obs$y, 2),
                                sep = ""),
                  hoverinfo = "text")%>%
        layout(#title = "Eagle Exposure",
               xaxis = list(title = "Eagle Activity (min/km<sup>3</sup>*hr)",
                            range = c(0,3)),
               yaxis = list(title = "Probability Density"))
    })

  })
}
