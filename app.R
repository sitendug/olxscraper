#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(httr)
library(tidyverse)
library(plotly)
olxfind<- function(area,yearstart, yearend, make){

  link <- paste0("https://www.olx.in/",area,"/cars_c84?filter=first_owner_eq_1%2Cmake_eq_",make,"%2Cyear_between_",yearstart,"_to_",yearend)
  page<- link |> session() |> read_html() # It is important to create a session first or else you may get a 403 error
  prices<- page |> html_nodes("._3GOwr") |> html_text()
  prices
  yearmileage<- page |> html_nodes(".KFHpP") |> html_text()
  carname<<- page |> html_nodes("._4aNdc") |> html_text()
  yearmileage
  # pic <- page |> html_attrs("img")
  polo<<- tibble(prices, yearmileage, carname)
  polo1<- polo |> separate(yearmileage, into= c("year", "mileage"), sep = " - ") |>
    mutate(mileage = str_remove_all(mileage,  pattern = "km")) |>
    mutate(mileage = str_remove_all(mileage,  pattern = "\\.+0")) |>
    mutate(mileage = str_remove_all(mileage,  pattern = "[:punct:]")) |>
    mutate(prices = str_remove_all(prices,  pattern = "[:punct:]")) |>
    separate(prices, into = c("symbol", "prices"), sep = " ") |>
    select(year, mileage, prices, carname) |>
    mutate(across(c(1:3), as.numeric))
  return(polo1)
  Sys.sleep(5)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Used car prices on Olx"),
  sidebarLayout(
    sidebarPanel(selectInput("manufacturer",
                             "Car Manufacturer",
                             choices = c("Volkswagen"= "volkswagen", "Maruti" = "maruti-suzuki","Hyundai"= "hyundai", "Honda" = "cars-honda", "Tata"= "tata", "Mahindra" = "mahindra","Toyota" = "toyota", "Ford" = "ford")),
                 selectInput("location",
                             "Area",
                             choices = c("Kolkata"= "kolkata_g4157275", "Delhi" = "delhi_g2001152","Dehradun"=  "dehradun_g4059236", "Chennai" = "chennai_g4059162")),
                 numericInput("fromyear",
                              "Year of start",
                              2014),
                 numericInput("toyear",
                              "Year till manufacture", 2021),
                 sliderInput("mileage",
                              "Miles Run",
                             5000,
                             100000,
                             100000),
                 sliderInput("budget", "Budget",
                             100000,
                             1500000,
                             550000,
                             step = 1000),
                 downloadButton("downloadData", "Download the data")),
    mainPanel(
      plotOutput("Plot")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {


  output$Plot<- renderPlot({
    polo1<- olxfind(area= input$location, yearstart = input$fromyear, yearend = input$toyear,make = input$manufacturer)
    polo1<- as_tibble(polo1)
    polo2<- polo1 |>
      filter(mileage <= input$mileage) |>
      filter(prices <= input$budget) |>
      mutate(year = as.factor(year))

    polo2 |>
      ggplot(aes(x = year, y = prices, group = year)) +
      geom_violin(aes(alpha = 0.001), show.legend = FALSE)+
      geom_boxplot(aes()) +
      geom_jitter(aes(colour = mileage)) +
      stat_summary(fun=mean, geom="point", shape=5, size=4)+
      scale_y_continuous(n.breaks = 10) +
      theme_bw()+
      theme(legend.position = "bottom")

  })
  output$downloadData <- downloadHandler (
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(polo2, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
