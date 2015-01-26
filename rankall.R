##rankall.R


rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        possible_outcomes<-c("heart attack","heart failure","pneumonia")
        ## Check that state and outcome are valid
        if(!(any(outcome==possible_outcomes)))
                stop("invalid outcome")
        
        col_num<-if(outcome==possible_outcomes[1]) 11
        else if(outcome==possible_outcomes[2])17
        else 23
        ## For each state, find the hospital of the given rank
        if (num=="best") {num <-1}
        s<-split(data,data[,7])
        
        for (i in length(s))
        {
                
        }
        df<-sapply(s, function(x) each_state(split=x,col=col_num,num=num))
        ##print(df)
        dm<-matrix(df,ncol(df),2)
        dm
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ##df<-data.frame(hospital=df[1:nrow(df),1],state=df[1:nrow(df),2])
        ##colnames(df)<-c("hospital","state")
        ##df
}
each_state <-function(split,col,num)
{
    ord<-order(split[,col],split[,2])
    if (num=="worst") {num <-length(ord)}
    
    else if (as.numeric(num)>length(ord)) {
            k1<-NA
            k2<-split[1,7]
            m<-c(k1,k2)
            return(m)
            ##invisible(m)    
            
    }
    
    k1<-split[ord[num],2]
    k2<-split[ord[num],7]
    m<-c(k1,k2)
    return(m)
    ##invisible(m)
}