### works perfectly
rankall <- function(outcome, num = "best") {
        x1= read.csv("outcome-of-care-measures.csv")
        if (outcome!="heart attack" & outcome!="heart failure" & outcome!="pneumonia") {
                stop("invalid outcome")}
        ## Set kout depends on outcome , 
        if (outcome=="heart attack") {kout=11}
        if (outcome=="heart failure") {kout=17}
        if (outcome=="pneumonia") {kout=23}
        #### create new data frame
        xn_1=x1[2] ## hospitals names
        xn_2=x1[7] ## hospitals states
        xn_3=x1[kout] ## hospital 30 day death rate
        xn=cbind(xn_1,xn_2,xn_3) ### new data frame
        xn_by_state=split(xn,xn_2) ## list of hospitals by states
        lh=length(xn_by_state) ## number of states
        ### change character to numeric
        for (ii in 1:lh) {
        xn_by_state[[ii]][,3]=as.vector(xn_by_state[[ii]][,3])
        xn_by_state[[ii]][,3]=as.numeric(xn_by_state[[ii]][,3])
        }
        ### getting complete cases
        for (iii in 1:lh) {
        xn_by_state[[iii]]=xn_by_state[[iii]][complete.cases(xn_by_state[[iii]]),]     
        }
        ### sort by outcome then alphabetical
        for (i3 in 1:lh) {
                xn_by_state[[i3]]=xn_by_state[[i3]][order(xn_by_state[[i3]][,3],xn_by_state[[i3]][,1]),]
        }
        ### set knum depends on num
        if (num=="best") {num=1}
        if (is.numeric(num)==TRUE) {
                num=num
                x_res=lapply(xn_by_state, function (x) x[num,1]) ## results for best or ranked worse hospitals
        }
        
        if (num=="worst") {
        x_res=lapply(xn_by_state, function (x) tail(x[,1], n=1))} ## results for worst hospitals
        ### testing num
        if (is.numeric(num)==FALSE&&num!="worst") {
                stop("invalid number")}
        
        ### creating a data frame with results
x_data=data.frame(matrix(NA, nrow = 1, ncol = 54))
for (i4 in 1:lh) {
        x_data[i4]=data.frame(as.character(x_res[[i4]]) )
}

x_rank1=t(x_data) ### transpose to make row vector-ish dataframe with names of hospital
x_rank2=levels(x1$State)
x_rank=data.frame(x_rank1,x_rank2)
dimnames(x_rank)[[2]]=c("hospital", "state") ## set names to rows
Statx1=levels(x1$State) ## getting names of states
dimnames(x_rank)[[1]]=Statx1 ## set names to columns
print (x_rank)
}

