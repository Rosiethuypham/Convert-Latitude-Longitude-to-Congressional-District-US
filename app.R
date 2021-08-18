#Density paper Study 6
#Link: https://choicedensity.shinyapps.io/Density_S6/

library(shiny) 
library(tidyverse)
library(ggplot2)
library(readxl)

data <- read_excel("JAMS_S6_Cleaned.xlsx")

data.den <- dplyr::rename(data, fit = Spend1.1)

Den.lm <- lm(fit ~ Density_SqKm * Choice, data = data.den)

summary(Den.lm)

ui <- fluidPage(
    
    titlePanel(
        title = h2("Population Density Moderates the Impact of Assortment Size on Customer Spending: Model Predictions", align="left"),
        windowTitle = "Population Density Moderates the Impact of Assortment Size on Customer Spending: Model Predictions"
    ),
    
    sidebarLayout(position = "right",
                  sidebarPanel(
                      htmlOutput("predstatement3"),
                      br(),
                      sliderInput("Choice", "Product assortment size:",
                                  min = 6, max = 30,
                                  value = 18, step = 1),
                      sliderInput("Density_SqKm", "Population density (people per squared km):",
                                  min = 5, max = 30000,
                                  value = 15000, step = 500),
                  ),
                  mainPanel(
                      htmlOutput("predstatement1"),
                      br(),
                      plotOutput("plot2"),
                      br(),
                      br(),
                      p("The model prediction is generated from Study 6 in the manuscript entitled 'Population Density and Value from Large Assortments'. Under review at the",
                        span("Journal of the Academy of Marketing Science.", style = "font-style:italic"))
                  )
    )
)

server <- function(input, output){
    Den.lm <- reactive({
        Den.lm <- lm(fit ~ Density_SqKm * Choice, data = data.den)
    })
    
    pred.data <- reactive({
        pred.data <- data.frame(Density_SqKm = seq(5, 30000, 500), Choice = input$Choice)
    })
    
    Den.lm.pred <- reactive({
        Den.lm.pred <- cbind(pred.data(), 
                             predict(Den.lm(), pred.data(), 
                                     interval = "confidence", 
                                     type = c("response", "terms")))
    })
    
    user.pred.data <- reactive({
        user.pred.data <- data.frame(Density_SqKm = input$Density_SqKm, Choice = input$Choice)
    })
    
    user.pred <- reactive({
        user.pred <- cbind(user.pred.data(), 
                           predict(Den.lm(), user.pred.data(), 
                                   interval = "confidence", 
                                   type = c("response", "terms")))
    })
    
    user.pred.per.ch <- reactive({
        user.pred.per.ch <- cbind(user.pred.data(),
                                  predict(Per.Ch(), user.pred.data(),
                                          interval = "confidence", 
                                          type = c("response", "terms")))
        
    })
    
    output$predstatement1 <- renderText({
        print(paste0("<div style = \"font-size:18px\">Based on the input values you selected,the model predicts that customers will spend <b><font color = \"red\">", round(user.pred()$fit, digits = 2),"</font color></b> USD on a pair of headphones.</div>"))
        
    })
    
    output$predstatement3 <- renderText({
        print(paste0("<div style = \"font-size:14px\">Please choose a value on each sliding bar below.</div>"))
        
    })
    
    output$plot2 <- renderPlot({
        ggplot(data = data.den, aes(x = Density_SqKm , y = fit)) +
            xlim(5, 30000) +
            ylim(0, 200) +
            geom_segment(data = user.pred(), aes(x = 5, y = fit, xend = Density_SqKm, yend = fit), linetype = "dashed") +
            geom_segment(data = user.pred(), aes(x = Density_SqKm, y = 0, xend = Density_SqKm, yend = fit), linetype = "dashed") +
            geom_ribbon(data = Den.lm.pred(), aes(ymin = lwr, ymax = upr), alpha = .3, fill = "gray") +
            geom_line(data = Den.lm.pred(), color = "black", size = 1) +
            geom_point(data = user.pred(), color = "red", size = 7) +
            labs(title="Assortment sizes and Population Density as predictors of Customer Spending", x="Population Density", y="Customer Spending (USD)")
    })
    
}

shinyApp(ui, server)

