##rankhospital.R
rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        possible_outcomes<-c("heart attack","heart failure","pneumonia")
        
        ## Check that state and outcome are valid
        if(!(any(data[,7]==state)))
                stop("invalid state")
        if(!(any(outcome==possible_outcomes)))
                stop("invalid outcome")
        
        col_num<-if(outcome==possible_outcomes[1]) 11
        else if(outcome==possible_outcomes[2])17
        else 23
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        subdata<-data[which(data[,7]==state),c(2,7,col_num)]
        subdata[,3]<-as.numeric(subdata[,3])
        
        subdata<-subdata[complete.cases(subdata),]
        
        rank<-order(subdata[,3],subdata[,1])
                
        
        if (num=="best") {num <-1
                               }
        else if (num=="worst") {num <-length(rank)}
                                
        else if (as.numeric(num)>length(rank)) { return(NA)}
        
        as.character(subdata[rank[num],1])
}