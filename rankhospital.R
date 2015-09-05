### works perfectly
rankhospital <- function(state, outcome, num = "best") {
        x1= read.csv("outcome-of-care-measures.csv")
        Statx1=levels(x1$State) ## getting names of states
        ## columns with outcome are : 11, 17, 23
        ## Check that state and outcome are valid
        if  (sum(Statx1==state)==0) {stop("invalid state")} ## checking valid states
        ## Checking valid outcome
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
        xn_state_selected=xn_by_state[[state]] ## list of hospitals by choosen state and choosen outcome
        vec=as.vector(xn_state_selected[,3])
        valg=as.numeric(vec)
        xn_state_num=data.frame(xn_state_selected[,1],xn_state_selected[,2],valg) ### creates vector with numbers
        xn_state_num_comp=xn_state_num[complete.cases(xn_state_num),] ### complete cases
        xn_state_sorted=xn_state_num_comp[with(xn_state_num_comp, order(valg,xn_state_num_comp[,1])), ] ### sort by outcome
        
        test_num=dim(xn_state_sorted)[1]
        ###
        ### set knum depends on num
        if (num=="best") {knum=1}
        if (num=="worst") {knum=test_num}
        if (is.numeric(num)==TRUE) {knum=num}
        super_hosp=xn_state_sorted[knum,] ### choose correct value
        rank=as.character(super_hosp[,1]) ### name with hospital
        ### check if num is bigger than dimensions of list of hospitals
        if (is.numeric(num)==TRUE) {
        if (as.numeric(num)>=test_num) {rank=NA}}  ### end of function
        print (rank)
        

}