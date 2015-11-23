library(quantmod)
library(ggplot2)

ui <- fluidPage(
  titlePanel("CAPM for Desired Stock"),
  
  sidebarLayout(
    sidebarPanel(color = 'cadetblue4',
                 helpText("Input a ticker and see the stock's regression to market and expected return."),
                 
                 textInput("symb", label = h4("Input a Valid Stock Ticker"), value = "GE"),
                 textInput("symb2", label = h4("Input a Valid Stock Ticker"), value = "XOM"),
                 dateRangeInput("date", label = h4("Input Date Range for Returns"), start = '2007-01-01', end = as.character(Sys.Date())),
                 helpText("Date Format: [YYYY/MM/DD]")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Market Regression", plotOutput("plot"),h4(textOutput('beta'))),
        tabPanel("Expected Return",  plotOutput("SML"), textOutput("eRmath"), helpText("[eR = rF + Beta(mR - rF)]"))
        
      )
    ))
)




server <- function(input, output) {
  dataInput <- reactive({
    validate(
      need(input$symb != "", "Please type a ticker"),
      need(input$date[1] < input$date[2], 'Start date is either missing or is later than end date.')
    )
    
    options(download.file.method="wininet")
    prices <- getSymbols(input$symb, from = input$date[1], to = input$date[2], auto.assign = FALSE)
    prices2 <- getSymbols(input$symb2, from = input$date[1], to = input$date[2], auto.assign = FALSE)
    market <- getSymbols("^GSPC", from = input$date[1], to = input$date[2], auto.assign = FALSE)
    t <- getSymbols.FRED("TB1YR", auto.assign = FALSE)
    dataPrices <- merge.xts(Ad(prices), Ad(market), Ad(prices2), join = "inner")
    
    stock <- dataPrices[,1]
    mm <- dataPrices [,2]
    sReturns <- Delt(stock)[-1]
    mmReturns <- Delt(mm)[-1]
    reg <- lm((sReturns) ~ (mmReturns))
    
    
    
    stock2 <- dataPrices[,3]
    sReturns2 <- Delt(stock2)[-1]
    reg2 <- lm((sReturns2) ~ (mmReturns))
    
    
    
    
    rMM <- mean(mmReturns)*365
    rF <- as.vector(t[length(t)])/100
    rI <- rF +  reg$coefficients[2] * (rMM - rF)
    theData <- list(x = data.frame(cbind(as.vector(sReturns), as.vector(mmReturns), as.vector(sReturns2))), y = reg, z = rI[length(rI)], rF = rF, rMM = rMM, reg2 = reg2)
    
  })
  
  
  output$plot <- renderPlot({
    
    theData <- dataInput()
    qplot(theData$x[,2], theData$x[,1], ylab = "Asset Returns", xlab = "Market Returns") +    
      geom_abline(intercept =theData$y$coef[1], slope = theData$y$coef[2], color = "grey23") +
      geom_abline(intercept =theData$reg2$coef[1], slope = theData$reg2$coef[2], color = "green") +
      labs(title = "Market Regression")+
      geom_point(color = "cadetblue4") 
    
    
  })
  
  output$eRmath <- renderText({
    theData <- dataInput()
    return(paste("Expected Return:", round(theData$z*100, 2), "%", " = ", round(theData$rF*100, 2), "%", "+", round(theData$y$coefficients[2], 2), "(", round(100*theData$rMM,2), "%", "-", round(theData$rF*100, 2), "%",")"))
  })
  
  output$SML <- renderPlot({
    theData <- dataInput()
    qplot(theData$y$coef[2], theData$z, ylim = c(0, .15), xlim = c(0, 2), ylab = "Expected Return", xlab = "Beta") +
      geom_abline(intercept =theData$rF, slope = ((theData$z - theData$rF)/theData$y$coef[2])) +
      labs(title = "Security Market Line") +
      geom_point(color = "cadetblue4", size = 3.5)
    
  })
  
  output$beta <- renderText({
    theData <- dataInput()
    paste("    Beta:  ", round(theData$y$coef[2], 2))
    
  })
}


shinyApp(ui = ui, server = server)
