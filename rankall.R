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
        subdata<-data[,c(2,7,col_num)]
        subdata[,3]<-as.numeric(subdata[,3])
        subdata<-subdata[complete.cases(subdata),]
        
        s<-split(subdata,subdata[,2])
        df<-data.frame()
        for (i in 1:length(s))
        {
                ss<-s[[i]]
                rank<-order(ss[,3],ss[,1])
                if(num=="worst") num<-length(rank) else num<-num
                df<-rbind(df,data.frame(ss[rank[num],1],ss[2,2]))
                
        }
        colnames(df)<-c("hospital","state")
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        #
        df[-1,]
}
