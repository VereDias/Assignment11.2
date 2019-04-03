#Assignment11.2

# Importing the bank data 
library(readr)
bank <- read.csv("C:/Users/vere/Downloads/bank-additional-full.csv", sep=";")
View(bank)
dim(bank)      
str(bank)  


#A-	Is there any association between job and default?
with(bank,chisq.test( job, default))
with(bank, table( job, default) )
with(bank, prop.table(table( job,default)))
#Answer - There is no association


#B-	Is there any significant difference in duration of last call between? people having housing loan or not?
#getOption("max.print")

with(bank, chisq.test(duration,housing))
with(bank, table( duration,housing) )


#C-	Is there any association between consumer price index and consumer?
chisq.test(bank$cons.price.idx,bank$cons.conf.idx) 

#Consumer price index and consumer are dependent on each other


#D-	Is the employment variation rate consistent across Job types?

chisq.test(bank$job,bank$emp.var.rate) 


#e.	Is the employment variation rate same across Education?

with(bank, chisq.test( education,emp.var.rate))
with(bank, table( education, emp.var.rate) )


# f.	Which group is more confident?

library(psych)
pairs.panels(bank[,1:6])
pairs.panels(bank[,1:9])
summary(bank) 

library(psych)
pairs.panels(bank[,1:14]
             
#VereDias