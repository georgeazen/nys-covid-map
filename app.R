
#Deployed Version
#Deployed Version 

library(shiny)
library(RCurl)
library(jsonlite)
library(dplyr)
library(plotly)
library(httr)
library(ggplot2)
library(shinyWidgets)
library(maps)
library(mapproj)
library(dygraphs)
library(xts) 
library(stringr)

setwd("~/")

#string for cleaning the time data
extra_str<-'T00:00:00.000'
#source of data
path<-"https://health.data.ny.gov/resource/xdss-u53e.json"
#get new york county data
ny_counties <- ggplot2::map_data("county","new york") 
#show errors in app
options(shiny.sanitize.errors = F)



# ui.R ----
ui <- fluidPage(
  setBackgroundColor("ghostwhite"),
  h1("Covid-19 Cases In New York State"),
  
  p("This application can be used for comparing the number of positive Covid-19
    cases in New York State by county. All data is pulled directly from data.ny.gov.
    All maps and charts are interactive."),

  
  mainPanel(
    
    h2("Number of New Cases by Day"),
    
    dateInput(inputId = "Date",  # Give the input a name
               label = "Select Date",  # Give the input a label to be displayed in the app
               value=Sys.Date()-2,min='2020-03-02',max=Sys.Date()-2,format = "yyyy-mm-dd"),
    

    plotlyOutput("plot"),
    br(),
    
    h2("Cases by County"),
    
    
    selectInput("county_name", "Select County:",
                   c(unique(ny_counties$subregion))),
            
    
    dygraphOutput('plot3'),
    
    br(),
  
  
    h2("County Change in Cases Over Time"),
    
    p('Select start/end dates and a map will be generated to show the 
      difference in daily cases between the two days. The start date must occur 
      before the end date. A positive number represents an increase in
      cases while a negative represents a decrease.'),
    br(),
    dateInput(inputId = "Far_Date",  # Give the input a name
              label = "Select Start Date",  # Give the input a label to be displayed in the app
              value=Sys.Date()-3,min='2020-03-02',max=Sys.Date()-2,format = "yyyy-mm-dd"),
    

    
    dateInput(inputId = "Near_Date",  # Give the input a name
              label = "Select End Date",  # Give the input a label to be displayed in the app
              value=Sys.Date()-2,min='2020-03-02',max=Sys.Date()-2,format = "yyyy-mm-dd"),
    
    
    
    plotlyOutput('plot2'),
    br(),
    br(),
    br(),
    br()
    
   
    
    
  )
)

# server.R ----
server <- function(input, output) {
  


  
  final_df <<- reactive({
    clean_date<-paste(as.character(input$Date),extra_str,sep='')
    request	<- httr::GET(path,query= list( test_date=clean_date))
    response <- httr::content(request, as = "text", encoding = "UTF-8")
    DF <- fromJSON(response, flatten = TRUE) %>% data.frame()
    FD<-dplyr::rename(ny_counties, county = subregion)
    FD2<-FD[,c(1:3,6)]
    DF2<-DF[,c(2:3)]
    DF2$county<-tolower(DF$county)
    final_df<- dplyr::full_join(FD2,DF2,by='county')
    final_df$new_positives<-as.numeric(final_df$new_positives)
    final_df
  })

  
  final_diff<<- reactive({
    clean_near_range_date<-paste(as.character(input$Near_Date),extra_str,sep='')
    clean_far_range_date<-paste(as.character(input$Far_Date),extra_str,sep='')
    request2	<- httr::GET(path,query= list( test_date=clean_near_range_date))
    response2 <- httr::content(request2, as = "text", encoding = "UTF-8")
    DF_near <- fromJSON(response2, flatten = TRUE) %>% data.frame()
    DF_near2<-DF_near[,c(2:3)]
    
    DF_near2$county<-tolower(DF_near$county)
    
    request3	<- httr::GET(path,query= list( test_date=clean_far_range_date))
    response3 <- httr::content(request3, as = "text", encoding = "UTF-8")
    DF_far <- fromJSON(response3, flatten = TRUE) %>% data.frame()
    DF_far2<-DF_far[,c(2:3)]
    
    DF_far2$county<-tolower(DF_far$county)
    
    FD<-dplyr::rename(ny_counties, county = subregion)
    FD2<-FD[,c(1:3,6)]
    
    case_difference<-as.numeric(DF_near2$new_positives)-as.numeric(DF_far2$new_positives)
    county<-DF_far2$county
    diff_df<- data.frame(county,case_difference,stringsAsFactors = F)
    final_diff<- dplyr::full_join(FD2,diff_df,by='county')
    final_diff
    
  })
  
  data<- reactive({
    county_to_get<-str_to_title(input$county_name)
    request4	<- httr::GET(path,query= list( county= county_to_get))
    response4 <- httr::content(request4, as = "text", encoding = "UTF-8")
    DF_county <- fromJSON(response4, flatten = TRUE) %>% data.frame()
    DF_county$test_date<-gsub(extra_str,'',DF_county$test_date)
    DF_county2<-DF_county[,c(1,3)]
    DF_county2$new_positives<-as.numeric(DF_county2$new_positives)
    DF_county2$test_date<-as.Date(DF_county2$test_date)
    data <- xts(x = DF_county2$new_positives, order.by = DF_county2$test_date)
    data
    
  })
  
 
  
  output$plot <- renderPlotly(
    ggplotly(ggplot(final_df())+ aes(long,lat, group=group) + 
               geom_polygon(aes(fill=new_positives))+
               scale_fill_gradient(low = 'grey', high = 'red', name = 'New Positives')+
               coord_map()+xlab('')+ylab('')+
               theme_minimal()) )
  
  
  output$plot3<-renderDygraph(dygraph(data()) %>%
    dyOptions( stepPlot=TRUE, fillGraph=TRUE))
  
  
  
  output$plot2<-renderPlotly(
    ggplotly(ggplot(final_diff())+ aes(long,lat, group=group) + 
              geom_polygon(aes(fill=case_difference))+
               #scale_fill_gradient(low = 'blue', high = 'red', name = 'case_difference')+
              scale_fill_viridis_b(option = 'magma')+
              coord_map()+xlab('')+ylab('')+
              theme_minimal() ))
            
}


# Run the app ----
shinyApp(ui = ui, server = server)
