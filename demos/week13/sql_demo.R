library(keyring)
library(RMariaDB)

conn <- dbConnect(MariaDB(),
                  user="lande065",
                  password=key_get("latis-mysql","lande065"),
                  host="mysql-prod5.oit.umn.edu",
                  port=3306,
                  ssl.ca = 'mysql_hotel_umn_20220728_interm.cer'
)

databases_df <- dbGetQuery(conn, "SHOW DATABASES;")
dbExecute(conn, "USE cla_tntlab;")