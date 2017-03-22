library(shiny)

source("exposure.R")
source("fatality.R")
source("helper_fxns.R")

shinyServer(function(input,output, session){
  exposure(input, output, session)
  fatality(input, output, session)

  output$defenders <- renderImage({
    width <- session$clientData$output_defenders_width
    if (width > 100) {
      width <- 100
    }
    list(src = "01_DOW_LOGO_COLOR_300-01.png",
         contentType = "image/png",
         alt = "Defenders of Wildlife", br(), "Endangered Species Program",
         width=width)
  }, deleteFile=FALSE)

  output$yesterday <- renderText({
    paste0(as.character(format(Sys.Date()-1, "%m/%d/%Y")), ".")
  })
})
