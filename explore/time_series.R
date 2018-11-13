source('lib/loadFunctions.R')
source('lib/loadData.R')

#source('lib/loadBasilica.R')

#library(devtools)
#install_github("vqv/ggbiplot")
#library( ggbiplot )

basedir = '/store/'
embeddir = paste0( basedir, '/embed/' )
imagedir = paste0( basedir, '/resized/' )
reclassdir =  paste0( basedir, '/reclass/' )


load(file='data/cache/embeddings_set2.rdata')  
load(file='data/cache/embeddings_set1.rdata')  

df_category = read_marked_categories_set2( base_images = df_embed_set2 %>% dplyr::select( filename ))

df_embed_set2 %>% 
  inner_join( df_category ) %>% 
  select( class, everything()) %>%
  { . } -> df_embed_set2

df_embed_set1 %>% 
  inner_join( df_category ) %>% 
  select( class, everything()) %>%
  { . } -> df_embed_set1

df_embed_set2 %>%
  bind_rows( df_embed_set1 ) %>% 
  { . } -> df_embed_long

df_embed_set2 %>%
  inner_join( df_embed_set1, by=qc(filename) ) %>% 
  { . } -> df_embed_wide


############################### end data prep

install.packages( "dtw" )
library( dtw )



df_motion %>%
  mutate( filename = str_replace( filename, '^.*/','')) %>%
  inner_join( df_embed_long %>% select( filename )) %>%
  group_by( text_event ) %>%
  mutate( ms = round( frame / (max( frame ) + 1) * 10000,0),  
          ts=as.POSIXct( paste0( event_time_stamp, '.', ms))) %>%
  select(-ms) %>%
  { . } -> df_motion1


#op <- options(digits.secs=6)


#as.POSIXct( '2018-11-02 08:42:04.543')

make_series = function( set ) {
  set%>%
    distinct() %>%
    as.tsibble( index = ts)
}


df_motion1 %>%
  distinct() %>%
  group_by( text_event ) %>%
  nest( numpixel:motion_Y) %>% 
  mutate( ss_ts = sapply( data, as.matrix)) %>%
  { . } -> df_motion_ts

  
dim( df_motion_ts )[1] -> N

df_motion_ts[1, 3][[1]][[1]]  -> a
df_motion_ts[3, 3][[1]][[1]]  -> b

dtw( dist( a,b ), distance.only=T) -> d

a
b
str(d)


result = matrix(nrow=N, ncol=N)
for (i in 1:N) { 
  for (j in 1:N) { 
    df_motion_ts[i, 3][[1]][[1]]  -> a
    df_motion_ts[j, 3][[1]][[1]]  -> b
    result[i,j] <- dtw( dist(a,b), distance.only=T )$normalizedDistance 
  }
}


