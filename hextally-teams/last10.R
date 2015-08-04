
source("app.R")

reduced.connect <- src_sqlite("../common-data/hextally.sqlite")
sqlstatement <- paste0('SELECT TOP (6) * FROM shotsteam ORDER BY Team')
primer <- tbl(reduced.connect, sql(sqlstatement)) %>% as.data.frame
