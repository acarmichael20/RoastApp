library(tidyverse)
library(lubridate)
library(shiny)
library(shinysurveys) #adds ability to have numberInput with placeholder values



# Define UI for application that calculates oven roast cooking time
ui <- navbarPage("Roasticulator",
                 tabPanel("Application",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(
                                      inputId = "AsYouLikeIt",
                                      label = h4("How do you like your oven roast?"),
                                      choices = c("Rare", "Medium", "Well"),
                                      selected = "Medium"
                                  ),
                                  numberInput("Weight",
                                              label = h4("Roast Weight (kg)"),
                                              placeholder = "1.23",
                                              value = ""
                                  ),
                                  textInput("FinishTime",
                                            label = h4("Planned supper time"),
                                            placeholder = "5:00",
                                            value = ""
                                  )
                              ),
                              column(6,
                                     mainPanel(
                                         htmlOutput("plotTitle"),
                                         plotOutput("Plot"),
                                         htmlOutput("Instructions")
                                         
                                     )
                              )
                          )
                 ),
                 tabPanel("About",
                          includeMarkdown("About.Rmd")
                    
                 )
)


# Define server logic required
server <- function(input, output) {
    
    TimeToCook <- reactive({
        
        switch( input$AsYouLikeIt,
                "Rare" = ( 0.5 * input$Weight ) + 1.25,
                "Medium" = ( 0.5 * input$Weight ) + 1.375,
                "Well" =( 0.5 * input$Weight ) + 1.5
        )
        
    })
    
    
    FinishTemp <- reactive({
        
        switch( input$AsYouLikeIt,
                "Rare" = "60&deg;C",
                "Medium" = "68&deg;C",
                "Well" = "74&deg;C"
        )
        
    })
    
    
    EndTime <- reactive({
        
        as.POSIXct(input$FinishTime, format = "%H:%M")
        
    })
    
    hour <- reactive({
        
        floor(TimeToCook() / 1)
        
    })
    
    min <- reactive({
        
        round( ((TimeToCook() %% 1) * 60), digits = 0)
        
    })
    
    CookTime <- reactive({
        
        hours(hour()) + minutes(min())
        
    })
    
    Times <- reactive({
        
        Times = c( format(EndTime(), "%H:%M"),     # End Time
                   format(EndTime()-minutes(15), "%H:%M"),     # Rest Time
                   format(EndTime()-minutes(15)-CookTime(), "%H:%M"),      # Cook at 275
                   format(EndTime()-minutes(15)-CookTime() - minutes(10), "%H:%M"),     # Cook at 450
                   format(EndTime()-minutes(15)-CookTime() - minutes(10) - minutes(10), "%H:%M")     # Preheat
        )
        
        return(Times)
        
    })
    
    
    instrucTitle <- paste("<b>","Instructions","</b>","<br>")
    
    instruc <- reactive({
        c(
            paste("Turn oven to 450&deg;F at", Times()[5]),
            "While oven is preheating, place roast in shallow roasting pan",
            "Season roast to taste with salt and pepper or your prefered seasoning",
            "Once oven has reached 450&deg;F, place roast in oven to sear for 10 minutes",
            paste("At", Times()[3], "turn oven to 275&deg;F and cook uncovered until", Times()[2]),
            paste("remove from oven when roast internal temperature is", FinishTemp()),
            paste("Transfer roast to plate, cover with foil or large bowl for 15 minutes or until", Times()[1]),
            paste("At", Times()[1],"uncover roast, carve and serve.")
        )
        
    })
    
    
    instruc2 <- reactive({
        
        paste0(instrucTitle,"\n", paste(paste0("- ", instruc(), "\n"), collapse = ""))
        
    })
    
    output$Instructions <- renderUI({ 
        
        
        if( input$FinishTime =="" || input$Weight =="") {
            return(NULL)
        }
        
        
        HTML( markdown::renderMarkdown(text = instruc2()))
        
    })
    
    
    
    dataset <- reactive({
        
        phase = c("Preheat", paste0("Cook at 450",intToUtf8(176),"F") , paste0("Cook at 275",intToUtf8(176),"F"), "Rest")
        
        time = c(10, 10, ((hour() * 60) + min())-10, 15)
        
        
        z <- data.frame(DummyVar = 1, time, phase, order = 4:1) %>%
            mutate(RT_time = cumsum(time)) %>%
            mutate(LenghtOfTime = case_when(
                str_detect(phase, "275") ~ format(parse_date_time(CookTime(), c('HMS', 'HM'))- minutes(10), '%H:%M'),
                TRUE ~ format(parse_date_time(time, c('M', 'HM')), '%H:%M')
            ))
        
        return(z)
        
    })
    
    output$Plot <- renderPlot({
        
        if( input$FinishTime =="" || input$Weight =="") {
            return(NULL)
        }
        
        ggplot(data = dataset(), aes(x = DummyVar, y = time, fill = (factor(order)))) +
            geom_col( show.legend = FALSE,
                      width = .1) +
            scale_y_continuous(breaks = c(0, dataset()$RT_time), labels = rev(Times()), ) +
            geom_text(aes(label = paste(dataset()$phase, "for", dataset()$LenghtOfTime)),
                      position = position_stack(vjust = .5),
                      fontface = "bold",
                      size = 4.5
            ) +
            ylab(NULL) +
            theme(
                # Hide panel borders and remove grid lines
                panel.background = element_rect( colour = "White", fill = "White"),
                panel.border = element_blank(),
                #panel.grid.major = element_line( color = "Grey", arrow = arrow() ),
                panel.grid.minor = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y = element_text(size = 12, face = "bold"),
                axis.ticks.length.y = unit(0.8, "cm")
                
            )
        
    })
    
    output$plotTitle <- renderUI({
        
        if( input$FinishTime =="" || input$Weight =="") {
            return(NULL)
        }
        
        
        HTML( markdown::renderMarkdown(text = paste("<b>","Timeline / Simple Instructions ","</b>")))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
