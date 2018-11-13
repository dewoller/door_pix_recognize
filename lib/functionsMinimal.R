
library(RMariaDB )
# -------------------------------------------------
my_db_get_query <- function ( query, param ) {

  drv <- dbDriver("MariaDB")
  con <- dbConnect(drv, dbname = "motion",
                   host = "192.168.1.37", port = 5432,
                   user = "motion", password = MOTION_PASSWORD  )
  on.exit(dbDisconnect(con))
  dbGetQuery( con, query, param=param )

}
