#For Example in Notes

m <- c(28,12,50)
f <- c(52,28,30)
df1 <- data.frame(m,f)

chi_df <- chisq.test(df1)


#OR

sex <- rep(c("m","f"),c(90,110))
set <- rep(c("s1","s2","s3","s1","s2","s3"),c(28,12,50,52,28,30))
df <- data.frame(sex,set)
chisq.test(table(df))

#Here df is a dataframe with 2 factors so 1st we'll convert factors into cross table 

# Pearson's Chi-squared test
# 
#             data:  df
# X-squared = 16.768, df = 2, p-value = 0.0002285

table(df[,2:1])


#Automating chi square
dataset <- df
chi_fun <- function(dataset)
{
  func1 <- function(x)
  {
    d<- table(dataset[,c(x[2],x[1])])
    d<-chisq.test(d)
     vec <- c()
     if(d$p.value>0.05)
     {
     vec <- c(x[1],x[2],d$p.value)
     }
     return(vec)
    }
  fac <- names(dataset)[sapply(dataset,is.factor)]
  
  fac_comb <- combn(fac,2)
  
  g <- apply(fac_comb,2,func1)

  return(g[[is.null(g)]])
}

chi_fun(bank)

par$NAME <- as.character(par$NAME)
