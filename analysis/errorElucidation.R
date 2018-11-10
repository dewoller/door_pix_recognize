#error checking

df_text_event %>%
  filter( nframe > 2400 ) %>%


df_motion %>%
  inner_join( df_text_event, by='text_event' ) %>%
  group_by( text_event) %>%
  filter( file_type==4) %>%
  filter( nframe > 2400 ) %>%
  summarise( max( ts)- min( ts  )) %>%
  summarise( max( ts), min( ts  )) %>%
  count( second, sort=TRUE) %>%
  count( ts, sort=TRUE) %>%
    count( event_time_stamp)

  df_motion %>%
    filter( file_type==4) %>%
    #filter(  text_event == '20181009062745' | text_event == '20181009182051') %>%
    #filter(  text_event ==  '20181009182051') %>%
    filter( ts=='2018-10-09 18:20:51' | 
    ts =='2018-10-09 18:28:37' |
    ts =='2018-10-08 21:56:38') %>% 
    select( text_event, ts)  %>%
    unique() %>%
    { . } -> minimal_example

##### 

library( tidyverse )
minimal_example = structure(list(text_event = c("event1", "event2", "event2"), ts = structure(c(1230728400, 1230814800, 1230728400), class = c("POSIXct", "POSIXt"), tzone = "Australia/Melbourne")), .Names = c("text_event", "ts"), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
#
minimal_example
#
# this example does not calculate duration correctly
minimal_example %>%
  group_by( text_event) %>%
  summarise(  duration= difftime( max( ts ) , min( ts )), 
            max(ts), 
            min(ts)
            ) %>% 
  filter(  text_event == 'event2') 
#
# this example calculates duration correctly
minimal_example %>%
  group_by( text_event) %>%
  filter(  text_event == 'event2') %>%
  summarise(  duration= difftime( max( ts ) , min( ts )), 
            max(ts), min(ts) )  



minimal_example %>%
  as.tsibble() %>%
    group_by( text_event) %>%
    filter(  text_event == '20181009182051') %>%
    summarise(  duration= max( ts ) - min( ts ), max(ts), min(ts) ) %>% 
    select( duration )

###### finish
  df_motion %>%
    group_by( text_event) %>%
    filter( file_type==4 ) %>%
    filter(  text_event == '20181009182051') %>%
    summarise(  duration= max( ts ) - min( ts ), max(ts), min(ts) ) %>% 

df_motion %>%
    group_by( text_event) %>%
    filter( file_type==4 ) %>%
    summarise(  duration= max( ts ) - min( ts ) ) %>% 
    filter(  text_event == '20181009062745') %>%

df_motion %>%
distinct( text_event )



