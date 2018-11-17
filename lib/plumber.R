# script name:
# plumber.R
library(RMySQL)
library(magrittr)
library(dplyr)
library(tibble)
library(caTools)
library(tidyr)
library(caret) 
library(hms) 
library( stringr )
source('/code/pivideo/door_pix_recognize/lib/functionsMinimal.R')
TEXT_EVENT = ''
BEST_FILE = data.frame() 

text_event=20181009135712
text_event='asdf'
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
  cat(as.character(Sys.time()), "\t", 
      req$REQUEST_METHOD, "\t", 
      req$PATH_INFO, "\t",
      req$HTTP_USER_AGENT, "\t", 
      req$REMOTE_ADDR, "\n")
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
#text_event ='20181117190446'
calculate_prediction <<- function( text_event="blank" ) {  

  if ( str_length ( text_event ) != 14 ) {
    return('ERROR:Invalid_text_event')
  }
  my_db_get_query( "select * from security where text_event=?", text_event) %>% 
    { . } -> rs

  if (dim(rs)[1]==0) {
    return('ERROR:Missing_text_event')
  }
  rs %>%
    as.tibble() %>% 
    filter( file_type==1 ) %>%
    mutate( ts = as.hms(strptime(event_time_stamp , "%Y-%m-%d %H:%M:%S"))) %>% 
    group_by( text_event ) %>%
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
    inner_join( df_motion %>% select( filename, numpixel )) %>%
    gather( category, value, -filename, -numpixel ) %>%
    filter( category == rv$category) %>%
    arrange( desc(  numpixel, value )) %>%
    head(1 ) %$% 
    { filename } ->> BEST_FILE

    as.character(rv$category)
}

#' bestfile return the best file for this text_event
#' @param text_event the text_event
#' @get /bestfile
#' @html
#' @response 200 Returns the best file for this text_event
best_file <<- function( text_event ) {  
#  if (text_event != TEXT_EVENT ) {
#    a=calculate_prediction ( text_event ) 
#  }
  BEST_FILE
} 
