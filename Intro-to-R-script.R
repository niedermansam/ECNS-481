library(tidyverse) # For general data manipulation
library(leaflet) # For interactive maps


# Load data from a github repository
resorts <- read.csv("https://raw.githubusercontent.com/niedermansam/SkiResortApp/master/skiResorts_geocoded3.csv") %>% as.tibble() %>% select(-X,-X.1)

resorts
# Into to R 
# Sam Supplee-Niederman


# Leaflet map ##################################

# Easy as can be....
leaflet(resorts) %>% addTiles() %>% addMarkers(popup = ~name)

# Create more informative labels
resorts$label <- sprintf("<div style='text-align:center'><a href='%s' target='_blank'><strong>%s</strong></a><br/> %s %s %s %s</div>",
                         resorts$url,resorts$name, 
                         ifelse(!is.na(resorts$vertical), paste0("<br/>Vertical Rise: <strong>",resorts$vertical," ft.</strong>"),""),
                         ifelse(!is.na(resorts$acres), paste0("<br/>Skiable Acres: <strong>",resorts$acres," acres</strong>"),""),
                         ifelse(!is.na(resorts$lifts), paste0("<br/>Number of Lifts: <strong>",resorts$lifts," lifts</strong>"),""),
                         ifelse(!is.na(resorts$ticket), paste0("<br/><br/><strong>Ticket Price: $",resorts$ticket,"</strong>"),"")) %>% lapply(htmltools::HTML)

# And make a map
leaflet(resorts) %>%
  addTiles() %>% 
  addMarkers(popup = ~label)

# Basic web scraping #######################
library(rvest)

website <- read_html("http://www.skicentral.com/mtcrescent.html") # read the html page
website %>% html_nodes("td") %>% html_text() # extract objects in "td" tags

# Working with APIs #########################
library(tidycensus)

# Load census variables
census_vars <- tidycensus::load_variables(2016,"acs5")

# Search for 'travel time' variables
census_vars %>% filter(concept %>% str_detect("(?i)travel time"))

get_acs('state',table="B08012")

## Summary Statistics with dplyr ###################
library(dplyr)

# Create a new variable
resorts$price_per_lift <- resorts$lifts / resorts$ticket

# Calculate summary statistics grouped by state
resorts %>% group_by(state) %>% summarize(
  count = n(),
  avg_vertical = mean(vertical, na.rm = T),
  avg_acres = mean(acres, na.rm = T),
  avg_lifts = mean(lifts, na.rm = T),
  avg_cost = mean(ticket,na.rm = T),
  avg_cost_per_lift = mean(price_per_lift, na.rm = T),
  avg_trails = mean(trails,na.rm = T)
)
# Note: the 'na.rm=T' part is very important if there are any na values.

## Graphics and ggplot #################################

library(ggplot2)

ggplot(resorts ,aes(y=ticket,x=vertical)) + geom_point() # simple graph

ggplot(resorts,aes(y=log(ticket),x=log(vertical))) + geom_point() # with log transformations

ggplot(resorts,aes(y=log(vertical),x=log(acres),size=ticket)) + geom_point() # colored by ticket price


# Linear Regression ##############################

# Basic Regression
reg1 <- lm(ticket ~ vertical + lifts + trails, data = resorts)
summary(reg1)

# Log transformation
reg2 <- lm(log(ticket) ~ log(vertical) + lifts + trails, data = resorts)
summary(reg2)



# Shiny App #####################################

library(shiny)

# Create a user interface
ui <- fluidPage(
  fluidRow(
    # Select summary variable
    selectInput("variable", "Variable to Summarize:", choices = names(resorts), selected = "ticket"),
    
    # Select grouping variables
    selectInput("group", "Grouping Variable(s):", choices = names(resorts), selected = "state", multiple = T)
  ),
  
  fluidRow(
    # Output data table
    dataTableOutput('table')
  )
  
)

# Create the apps 'backend'
server <- function(input,output,session) {
  
  
  output$table <-  renderDataTable({
    
    # Define user input variables
    group <- input$group
    variable <- input$variable
    
    # Create summary data table
    resorts %>% group_by(.dots = group)  %>% summarize(
      N = n(), 
      min = min(!! sym(variable) %>% as.double(), na.rm = T),
      Q1 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.25),
      mean = mean(!! sym(variable) %>% as.double(), na.rm = T),
      median = median(!! sym(variable) %>% as.double(), na.rm = T),
      Q3 = quantile(!! sym(variable) %>% as.double(), na.rm = T, probs=0.75), 
      max = max(!! sym(variable) %>% as.double(), na.rm = T),
      NAs = sum(is.na(!! sym(variable)))
    )
    
    # Note: the !! sym(variable) format converts the user input into a form that R can recogize.
    
  })
}

shinyApp(ui, server)
