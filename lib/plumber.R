# script name:
# plumber.R
library(RMySQL)
library(tidyverse)
library(caret) 
library(hms) 
source('/code/pivideo/door_pix_recognize/lib/functionsMinimal.R')
TEXT_EVENT = ''
BEST_FILE = data.frame() 

# load model
# this path would have to be adapted if you would deploy this
load("/code/pivideo/door_pix_recognize/data/predictionModel.rdata")


# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Run predictions for person prediction with logitboost model
#' @apiDescription This API takes a text_event id, looks up parameters, runs a logitboost model on them
#' indicate if person or not 

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
calculate_prediction <<- function( text_event ) {  

    my_db_get_query( "select * from security where text_event=?", text_event) %>% 
    as.tibble() %>% 
    filter( file_type==1 ) %>%
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
    select( filename, numpixel:motion_Y, duration:max_pixel)  %>% 
    distinct() %>%
    { . } -> df_motion

  df_motion %>%
    select( numpixel:motion_Y, duration:max_pixel)  %>% 
    predict(predictionModel, newdata=., type='prob') %>%
    data.frame() %>%
    bind_cols( filename=df_motion$filename) %>% 
    { . } -> predictions


  predictions %>%
    gather( category, value, -filename ) %>%
    group_by( filename ) %>%
    filter( value == max( value )) %>%
    ungroup() %>%
    count( category, sort=TRUE ) %>%
    head( 1 ) %>% 
    { . } -> rv

  predictions %>%
    gather( category, value, -filename ) %>%
    filter( category == rv$category) %>%
    arrange( desc( value )) %>%
    head(1 ) %>% 
    { . } ->> BEST_FILE

    as.character(rv$category)
}

#' bestfile return the best file for this text_event
#' @param text_event the text_event
#' @get /bestfile
#' @html
#' @response 200 Returns the best file for this text_event
best_file <<- function( text_event ) {  
  if (text_event != TEXT_EVENT ) {
    a=calculate_prediction ( text_event ) 
  }
  BEST_FILE$filename
} 
