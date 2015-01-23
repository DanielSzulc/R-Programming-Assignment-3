##best function

best <- function(state, outcome) {
        ## Read outcome data
        data<-read.csv("outcome-of-care-measures.csv")
        possible_outcomes<-c("heart attack","heart failure","pneumonia")
        
        ## Check that state and outcome are valid
        if(!(any(data[,7]==state)))
                stop("invalid state")
        if(!(any(outcome==possible_outcomes)))
                stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        col_num<-if(outcome==possible_outcomes[1]) 11
                else if(outcome==possible_outcomes[2])17
                else 23
        
        ##Return name of hospital
        subdata<-data[which(data[,7]==state),c(2,7,col_num)]
        subdata[,3]<-as.numeric(subdata[,3])
        
        subdata<-subdata[complete.cases(subdata),]
        
        rank<-order(subdata[,3],subdata[,1])
        
        as.character(subdata[rank[1],1])
}