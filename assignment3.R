##### assingment 3!!
## read file and seeing  summaries of stuff
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
## do histogram
outcome[, 11] <- as.numeric(outcome[, 11]) 
hist(outcome[, 11])
#### again read file
x = read.csv("outcome-of-care-measures.csv")
x1=head(x)
State=levels(x1$State)



####
## some stuff
xg=xchstate[,3][xchstate[,3]!="Not Available"] ### choose only numbers
xg2=xchstate[xchstate[,3][xchstate[,3]!="Not Available"],]

