library(readxl)
library(dplyr)
mnc <- read_excel("Mother & Baby data.xlsx")
View(mnc)

mnc_case <- mnc %>% filter(`Study groups`== "Case")
mnc_control <- mnc %>% filter(`Study groups`== "Control")

range(mnc_case$`Age(yrs)`)

bins=seq(19,38,by=2)

intervals = cut(mnc_case$`Age(yrs)`,bins)

transform(table(intervals))

boxplot(mnc_case$`Age(yrs)`,col="blue")

mean(mnc_case$`Age(yrs)`)

sd(mnc_case$`Age(yrs)`)

table(mnc_case$`TREATMENT HISTORY`,mnc_case$`DIABETIC HISTORY - GDM`)


