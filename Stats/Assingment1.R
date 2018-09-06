library(tidyverse)
library(readxl)

platelet <- read_excel("Dataset 2.xlsx")
platelet<- as.data.frame(platelet)

str(platelet)
subset_g1 <- platelet %>% filter(`Study group` == "Group 1")
View(subset_g1)

col_names <- names(platelet)
for (i in col_names) {
  if(length(unique(platelet[,i])) <= 4)
  {
    platelet[,i] <- as.factor(platelet[,i])
  }
}


levels(platelet$`Study group`)

summary(platelet$`Age (yrs)`)




freq_dist<- function(clm_to_dist)
{
  dataset <- platelet
  m <<- m+1
  freq_db <- data.frame(cat="a",group="b")
  num_col<-ncol(dataset)
  if(is.numeric(clm_to_dist))
  {
    
    range=(max(clm_to_dist,na.rm = T)-min(clm_to_dist,na.rm = T))/5
    min_val<-min(clm_to_dist,na.rm = T)-range
    max_val=max(clm_to_dist,na.rm = T)
    bin=seq(from=min_val,to=max_val,by=range)
    dataset[,num_col+1] <- cut(clm_to_dist,bin)
     
     
     t= dataset %>% group_by(dataset[,(num_col+1)]) %>% summarise(Group_1=sum(Study_Group=="Group 1"),Group_2=sum(Study_Group=="Group 2"))
     
   
    }
  else if(is.factor(clm_to_dist))
   {
     fac_col<-names(dataset)[m-1]
     t = dataset %>% group_by(dataset[,c(fac_col)]) %>% summarise(Group_1 = sum(Study_Group=="Group 1"),Group_2=sum(Study_Group=="Group 2"))
      names(t)[1] <- fac_col
     }
  
  return(t)
}

m=1
a<-lapply(platelet,freq_dist)
a
