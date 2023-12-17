library(taskscheduleR)
library(miniUI)
library(cronR)


##mac
cmd = cron_rscript("/Users/Bailey/Desktop/Fall 2023/StatConsultation/CovidAPI.R")
cron_add(cmd, "daily", at="5PM")

cmd1 = cron_rscript("/Users/Bailey/Desktop/Fall 2023/StatConsultation/MovieAPI.R")
cron_add(cmd1, "daily", at="6AM")
cron_clear(ask=TRUE)

cmd2 = cron_rscript("/Users/Bailey/Desktop/Fall 2023/StatConsultation/imbddata.R")
cron_add(cmd1, "daily", at="3PM")

##Window
taskschedulerAddin()
