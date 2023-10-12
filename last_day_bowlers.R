library(dplyr)
library(DBI)
library(dbplyr)
library(odbc)

odbcListDrivers()

con <- DBI::dbConnect(odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "mcobsql.business.nd.edu",
                      UID = "MSBAstudent",
                      PWD = "SQL%database!Mendoza",
                      Port = 3306, 
                      Database = "BowlingLeagueExample")

dbListFields(con, "Bowler_Scores")

dbListFields(con, "Bowlers")

select_q <- dbSendQuery(
  conn = con, 
  statement = "SELECT * FROM Bowler_Scores"
)

select_res <- dbFetch(select_q)

library(ggplot2)

select_res$HandiCap = select_res$HandiCapScore - select_res$RawScore

ggplot(select_res, aes(x=RawScore, y =HandiCapScore, color = WonGame))+
  geom_point()+
  geom_smooth(method = lm, color = "black")+
  theme_minimal()

ggplot(select_res, aes(x=HandiCap, y =RawScore, color = WonGame))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  theme_minimal()




regr <- lm(RawScore ~ HandiCap, data=select_res)

summary(regr)



cor(select_res$RawScore, select_res$HandiCapScore)

library(gtsummary) 
theme_gtsummary_journal("qjecon")

t1 <- tbl_regression(regr, intercept = TRUE)



dbClearResult(select_q)

