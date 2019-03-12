# Trying to set up BLS api

library(devtools)
library(blsAPI)
library(tidyverse)
library(shiny)
library(data.table)

install_github("mikeasilva/blsAPI")

# getting data
#df2 <- blsAPI(payload = "LNS11000019", api_version = 2, return_data_frame = TRUE) 
payload_total <- list(
  'seriesid'=c('LNS14000000'),
  'startyear'=2009,
  'endyear'=2019)

payload_white <- list(
  'seriesid'=c('LNS14000003'),
  'startyear'=2009,
  'endyear'=2019)

payload_black <- list(
  'seriesid'=c('LNS14000006'),
  'startyear'=2009,
  'endyear'=2019)

payload_asian <- list(
  'seriesid'=c('LNU04032183'),
  'startyear'=2009,
  'endyear'=2019)
payload <- list(
  'seriesid'=c('LNS14000000','LNS14000003', 
               'LNS14000006','LNU04032183'),
  'startyear'=2009,
  'endyear'=2019)

# df_total <- blsAPI(payload = payload_total, api_version = 2, return_data_frame = TRUE)
# df_white <- blsAPI(payload = payload_white, api_version = 2, return_data_frame = TRUE)
# df_black <- blsAPI(payload = payload_black, api_version = 2, return_data_frame = TRUE)
# df_asian <- blsAPI(payload = payload_asian, api_version = 2, return_data_frame = TRUE)

# combining year and month columns and formatting dates for each
df_total <- df_total %>% unite(col = date, periodName, year, sep = " ", remove = FALSE)
df_total$date <- as.Date(paste('01', df_total$date), format = "%d %B %Y")

df_white <- df_white %>% unite(col = date, periodName, year, sep = " ", remove = FALSE)
df_white$date <- as.Date(paste('01', df_white$date), format = "%d %B %Y")

df_black <- df_black %>% unite(col = date, periodName, year, sep = " ", remove = FALSE)
df_black$date <- as.Date(paste('01', df_black$date), format = "%d %B %Y")

df_asian <- df_asian %>% unite(col = date, periodName, year, sep = " ", remove = FALSE)
df_asian$date <- as.Date(paste('01', df_asian$date), format = "%d %B %Y")

# combine tables
df <- df_total %>% 
  left_join(df_white, by = "date") %>% 
  left_join(df_black, by = "date") %>%
  left_join(df_asian, by = "date")

# rename variables
setnames(df, old =c("value.x","value.y", "value.x.x", "value.y.y"),
         new=c("val_total", "val_white", "val_black", "val_asian"))

df$val_total <- as.numeric(df$val_total)
df$val_white <- as.numeric(df$val_white)
df$val_black <- as.numeric(df$val_black)
df$val_asian <- as.numeric(df$val_asian)


ui <- fluidPage(
  titlePanel(title=h4("Some pretty cool graphs going on here", 
                      align="center")),
  selectInput("variable", label = "Pick a group", selected = "val_total",
                     c("Total Unemployment" = "val_total", "White Unemployment" = "val_white", 
                       "Black Unemployment" = "val_black", "Asian Unemployment" = "val_asian")),
  plotOutput("graph", width = "100%")
  
)



server <- function(input, output) 
{
  
   output$graph <- renderPlot(
     {
        ggplot(data = df, aes_string(x = "date", y = input$variable))+ 
        xlab("Date") +
         ylab("Unemployment Rate") + 
         geom_line() +
        geom_point()
  
      }
  )
}
 
shinyApp(ui = ui, server = server)
