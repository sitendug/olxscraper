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
library(lubridate)
olxfind<- function(area,yearstart, yearend, make, fuel){

  link <- paste0("https://www.olx.in/",area,"/cars_c84?filter=first_owner_eq_1%2Cmake_eq_",make,"%2Cyear_between_",yearstart,"_to_",yearend, "%2Cpetrol_eq_", fuel)
  page<- link |> session() |> read_html() # It is important to create a session first or else you may get a 403 error
  prices<- page |> html_nodes("._3GOwr") |> html_text()
  locality <- page |> html_nodes("._1zvfB") |> html_text()
  prices
  yearmileage<- page |> html_nodes(".KFHpP") |> html_text()
  carname<<- page |> html_nodes("._4aNdc") |> html_text()
  yearmileage
  # pic <- page |> html_attrs("img")
  polo<<- tibble(prices, yearmileage, carname, locality)
  polo1<<- polo |> separate(yearmileage, into= c("year", "mileage"), sep = " - ") |>
    mutate(mileage = str_remove_all(mileage,  pattern = "km")) |>
    mutate(mileage = str_remove_all(mileage,  pattern = "\\.+0")) |>
    mutate(mileage = str_remove_all(mileage,  pattern = "[:punct:]")) |>
    mutate(prices = str_remove_all(prices,  pattern = "[:punct:]")) |>
    separate(prices, into = c("symbol", "prices"), sep = " ") |>
    select(year, mileage, prices, carname, locality) |>
    mutate(across(c(1:3), as.numeric)) |>
    mutate(date = as.character(str_extract_all(locality, pattern = "[:digit:]{1,2}+\\s+[:alpha:]{3,}")),
           date = as.character(case_when(str_detect(locality, pattern = "Today") ~ "Today",
                                         str_detect(locality, pattern = "Yesterday") ~ "Yesterday",
                                         TRUE ~ date))) |>
    mutate(locality = str_remove_all(locality, pattern = date),
           locality = str_remove_all(locality, pattern = "\\s+ago")) |>
    mutate(day = str_extract(date, "[:digit:]{1,2}")) |>
    mutate(yeara = as.character(year(Sys.Date()))) |>
    mutate(month = as.character(str_extract_all(date, "\\s+[:alpha:]{3}"))) |>
    mutate(month = case_when(date %in% c("Today", "Yesterday", "days") ~ months.Date(Sys.Date()),
                             str_detect(date, "days") ~ months.Date(Sys.Date()),
                             TRUE ~ month)) |>
    mutate(date = case_when(date == "Today" ~ as.character(Sys.Date()),
                            date == "Yesterday" ~ as.character(Sys.Date()-1),
                            date == "2 days" ~ as.character(Sys.Date() -2),
                            date == "3 days" ~ as.character(Sys.Date() -3),
                            date == "4 days" ~ as.character(Sys.Date() -4),
                            date == "5 days" ~ as.character(Sys.Date() -5),
                            date == "6 days" ~ as.character(Sys.Date() -6),
                            TRUE ~ paste(yeara, month, day, sep = "-"))) |>
    mutate(month = str_extract_all(month, "[:alpha:]")) |>
    mutate(date = as_date(date)) |>
    select(-yeara) |>
    mutate(day = day(date))
  return(polo1)
  Sys.sleep(5)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Used car prices on Olx"),
  sidebarLayout(
    sidebarPanel(selectInput("manufacturer",
                             "Car Manufacturer",
                             choices = c("Volkswagen"= "volkswagen",
                                         "Maruti" = "maruti-suzuki",
                                         "Hyundai"= "hyundai",
                                         "Honda" = "cars-honda",
                                         "Tata"= "tata",
                                         "Mahindra" = "mahindra",
                                         "Toyota" = "toyota",
                                         "Ford" = "ford",
                                         "Renault" = "renault",
                                         "Skoda" ="skoda",
                                         "Nissan" = "nissan",
                                         "Kia" = "kia")),
                 radioButtons("fuel",
                              "Fuel type",
                              choices = c("petrol" = "petrol", "diesel" = "diesel")),
                 selectInput("location",
                             "Area",
                             choices = c("Dehradun"=  "dehradun_g4059236",
                                         "Uttaranchal"= "uttaranchal_g2001175",
                                         "UP" ="uttar-pradesh_g2001176",
                                         "Kolkata"= "kolkata_g4157275",
                                         "Delhi" = "delhi_g2001152",
                                         "Noida" = "noida_g4059326",
                                         "Chennai" = "chennai_g4059162",
                                         "Chandigarh" = "chandigarh_g2001149",
                                         "Bengaluru" = "bengaluru_g4058803",
                                         "Mumbai" = "mumbai_g4058997",
                                         "Ahmedabad" = "ahmedabad_g4058677",
                                         "Jaipur" = "jaipur_g4059123")),
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
      "Plots",
      fluidRow(
        column(12,"Median prices of cars per year", plotlyOutput("Plot1")),
               fluidRow(
                 column(12, "Number of cars from different models", plotOutput("Plot2"))
               )))

      ))





# Define server logic required to draw a histogram
server <- function(input, output, session) {


  output$Plot1<- renderPlotly({
    polo1<- olxfind(area= input$location, yearstart = input$fromyear, yearend = input$toyear,make = input$manufacturer, fuel = input$fuel)
    polo1<- as_tibble(polo1)
    polo2<<- polo1 |>
      filter(mileage <= input$mileage) |>
      filter(prices <= input$budget) |>
      mutate(year = as.factor(year))

    p<- polo2 |>
      ggplot(aes(x = year, y = prices, group = year, label =  carname)) +
      #geom_violin(aes(alpha = 0.001), show.legend = FALSE)+
      geom_boxplot(aes(alpha = 0.0001, fill = "EA5000"), show.legend = FALSE) +
      geom_jitter(aes(size = mileage),show.legend = FALSE) +
      #geom_text(aes_q(label = ~ (paste(carname, locality))))+
      stat_summary(fun=mean, geom="point", shape=5, size=4)+
      scale_y_continuous(n.breaks = 10) +
      theme_bw()+
      #geom_text(aes(size = mileage),check_overlap = TRUE)+
      theme(legend.position = "null")
    p<- ggplotly(p, tooltip = c("mileage", "y", "label"))
    p |> layout(showlegend = FALSE)# Hides the legend in plotly

  })
  output$Plot2<- renderPlot({
    polo1<- olxfind(area= input$location, yearstart = input$fromyear, yearend = input$toyear,make = input$manufacturer, fuel = input$fuel)
    polo1<- as_tibble(polo1)
    polo2<<- polo1 |>
      filter(mileage <= input$mileage) |>
      filter(prices <= input$budget) |>
      mutate(year = as.factor(year))
    polo2 |> mutate(carname = as.factor(carname),
                    year = as.character(year),
                    year = as.numeric(year)) |>
    group_by(carname, year) |> summarise(pricesm = round(mean(prices)), mileage = mean(mileage), count = n()) |>
     ungroup() |>
      ggplot(aes(x = year, y = count, label= pricesm)) +
      geom_col(fill= "#1061C3")+
      # geom_boxplot()+
      theme_bw()+
      geom_label(aes(label= pricesm))+
      #scale_y_discrete() +
      facet_wrap(~carname)
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
