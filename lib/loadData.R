source( 'lib/get_DB_connection.R')


read_marked_categories_set1 = function () {

  read.csv('data/category.csv') %>% 
    as.tibble()  %>%
    mutate( id2 = str_replace_all( id2, '"| ', '')) %>% 
    rename( text_event=id2) %>%
    select( text_event, class ) %>%
    { . } -> df_category_set1


  read.csv('data/category1.csv') %>% 
    as.tibble()  %>%
    mutate( text_event = str_replace_all( text_event, "'| ", '')) %>% 
    select( text_event, class ) %>% 
    bind_rows( df_category_set1 ) %>%
    { . } -> df_category_set1 
  df_category_set1 
}

if (FALSE ) {
  get_DB_connection() %>%
  dbSendQuery( "select * from security") %>% 
    fetch(n=-1) %>%
    as.tibble() %>% 
    filter( text_event != "" ) %>%
    mutate( ts = as.hms(strptime(event_time_stamp , "%Y-%m-%d %H:%M:%S"))) %>% 
    { . } -> df_motion

  df_motion %>% write.csv( 'data/motion.csv')
} else {

  read.csv('data/motion.csv', stringsAsFactors=FALSE  ) %>% 
    as.tibble() %>%
    mutate( text_event = as.character( text_event ) ) %>%
    mutate( ts = as.hms(strptime(event_time_stamp , "%Y-%m-%d %H:%M:%S"))) %>% 
  { . } -> df_motion 

}



read_marked_categories_set2 = function (base_images) {

  read.csv('data/marked.files.txt', stringsAsFactors=FALSE) %>% 
    as.tibble() %>%
    mutate( filename = str_replace( filename, '^\\./','')) %>%
    separate( filename, qc( class, filename ), sep='/') %>%
    filter( !is.na( filename )) %>% 
    distinct( filename, class ) %>%
    right_join( base_images ) %>%
    mutate(  class = ifelse( is.na( class ), 0, class )) %>%
    mutate( class = factor( class, levels=0:5, labels=qc( car, blank, bike, person, carPerson, truck))) %>% 
    { . } -> df_category


  df_category %>% 
    anti_join( 
              df_category %>% 
                count( filename, sort=TRUE ) %>%
                filter( n>1) %>%
                inner_join( df_category ) %>%
                filter( class=='person' | class=='blank') 
              ) %>% 
      distinct(class, filename) %>%
    { . } -> df_category


  read.csv('data/remarked.files.txt', stringsAsFactors=FALSE) %>% 
    as.tibble() %>%
    separate( is_really_bike, qc( class, filename ), sep='/') %>%
    mutate( class = str_replace( class, 'is_really_','')) %>%
    filter( !is.na( filename )) %>% 
    distinct( filename, class ) %>% 
    { . } -> df_remarked

  df_remarked %>%
    inner_join( df_category, by='filename')

  df_category %>%
    anti_join( df_remarked, by='filename') %>%
    bind_rows( df_remarked ) %>% 
    { . } -> df_category

  df_category 
}
