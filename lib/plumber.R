# script name:
# plumber.R
library(RMySQL)
library(tidyverse)
library(caret) 

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Run predictions for person prediction with logitboost model
#' @apiDescription This API takes a text_event id, looks up parameters, runs a logitboost model on them
#' indicate if person or not

# load model
# this path would have to be adapted if you would deploy this
load("/code/pivideo/categorisation/data/savedModel.rdata")

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
#' @param text_event:string the text_event
#' @get /predict
#' @html
#' @response 200 Returns the class of text_event (person, bike, blank, car, carperson)
calculate_prediction <- function( text_event ) {

  input_data <<- as.data.frame(cbind(input_data_num, input_data_int))

  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric or integers"
  }

#  if (any(input_data < 0) || any(input_data > 1)) {
#    res$status <- 400
#    res$body <- "Parameters have to be between 0 and 1"
#  }

  # predict and return result
  pred_rf <<- predict(model_rf, input_data)
  as.character(pred_rf)
}
