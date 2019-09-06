

## Handling API Requests in R
## https://www.youtube.com/watch?v=MAZic778uOw


install.packages('httr')
install.packages('jsonlite')


library(httr)
library(jsonlite)
library(magrittr)

# test on Starwars API:
SW <- GET("http://swapi.co/api/planets/1")

headers(SW)

str(content(SW)) # COntent to look into the objects within the response
http_status(SW)
warn_for_status(SW)

header <- headers(SW)

content(SW, 'text') # output as text
content(SW, 'parsed') # Shortcut


# Excercise:
  
alderaan <- GET("http://swapi.co/api/planets/?search=alderaan")
# but you can use Query = list()
names(alderaan)
alderaan$status_code
alderaan$headers$`content-type` # Usable to check the parsing methodology


# Get the contents of the response
text_content <- content(alderaan, "text", encoding = "UTF-8")

# Parse data to make it more beautiful

# Parse with jsonlite:

json_content <- text_content %>% fromJSON()
names(json_content)
json_content

## Extract the Results from the JSON Content

planetary_data <- json_content$results




##### Project Idea:
## Send Automated Text Messages using the rules:
## Get Data from AccuWeather API
## If there is a predicted rain amount that exceeds ()
## Then send Text messages
## Gathe Historical Data for Monitoring and Machine Learning Purposes later:

## Initital Prototype: Send Daily weather Forecast to my number


#### App Idea: Automated Sending of texts to LGUs as Warning Device.
#### Use some cloud Services to Run Persistent Execution





#