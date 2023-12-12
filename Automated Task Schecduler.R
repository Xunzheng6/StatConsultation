library(taskscheduleR)
library(miniUI)
install.packages("cronR")
library(cronR)

getwd()

##mac
cmd = cron_rscript("/Users/Bailey/Desktop/Fall 2023/StatConsultation/CovidAPI.R")
cron_add(cmd, "daily", at="5PM")

##Window
taskschedulerAddin()
