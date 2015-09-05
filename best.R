### works 99%
best <- function(state, outcome) {
        x = read.csv("outcome-of-care-measures.csv")
        x1=head(x)
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
        
        ##
        ### column with names of hospital -> 2
        xn_1=x[2] ## hospitals names
        xn_2=x[7] ## hospitals states
        xn_3=x[kout] ## hospital 30 day death rate
        xn=cbind(xn_1,xn_2,xn_3) ### new data frame
        xnst=split(xn,xn_2) ## list of hospitals by states
        xchstate=xnst[[state]] ## list of hospitals by choosen state and choosen outcome
        vec=as.vector(xchstate[,3])
        valg=as.numeric(vec)
        xn2=data.frame(xchstate[,1],xchstate[,2],valg) ### creates vector with numbers
        xn2complete=xn2[complete.cases(xn2),] ### complete cases
         ###  xn2complete[,3]==min(xn2complete[,3]) ### test for minimum
        besthos=xn2complete[xn2complete[,3]==min(xn2complete[,3]),] ### best hospital
        ###  min(xn2[,3], na.rm=1)  ### select best value   
        spital=as.character(besthos[,1]) ### name of hospital finally
        print (spital) ### print output :D
}
        


