# script name:
# plumber.R
library(RMySQL)
library(tidyverse)
library(caret) 
library(hms) 

source('/code/pivideo/door_pix_recognize/lib/functionsMinimal.R')

#text_event = '20181102084204'

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Run predictions for person prediction with logitboost model
#' @apiDescription This API takes a text_event id, looks up parameters, runs a logitboost model on them
#' indicate if person or not 

# load model
# this path would have to be adapted if you would deploy this
load("/code/pivideo/door_pix_recognize/data/predictionModel.rdata")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' predict person given text_event id
#' @param text_event the text_event
#' @get /predict
#' @html
#' @response 200 Returns the class of text_event (person, bike, blank, car, carperson)
calculate_prediction <- function( text_event ) {  

    my_db_get_query( "select * from security where text_event=?", text_event) %>% 
    as.tibble() %>% 
    mutate( ts = as.hms(strptime(event_time_stamp , "%Y-%m-%d %H:%M:%S"))) %>% 
    group_by( text_event ) %>%
    mutate( ms = round( frame / (max( frame ) + 1) * 10000,0),  
           ts=as.POSIXct( paste0( event_time_stamp, '.', ms))) %>%
    select(-ms) %>%
    mutate( duration = as.numeric(max( ts ) - min(ts)), 
           avg_velocity_X = (max( motion_X ) - min( motion_X )) / duration,
           avg_velocity_Y = (max( motion_Y ) - min( motion_Y )) / duration,
           max_height=max( motion_height ), 
           max_width=max( motion_width ),
           max_pixel = max( numpixel )
           ) %>%
    ungroup() %>%
    select( numpixel:motion_Y, duration:max_pixel)  %>% 
    distinct() %>%
    { . } -> motion_n


  # predict and return result
  predict(predictionModel, motion_n) %>%
    table() %>%
    data.frame() %>%
    rename(category='.') %>%
    filter( Freq==max(Freq )) %>%
    head(1) %>% 
    { . } -> rv

    as.character(rv$category)

}

