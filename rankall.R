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
        
        df<-data.frame(hospital=" ",state=" ")
        for (i in 1:length(s))
        {
                sp<-s[[i]]
                sp<-sp[,c(2,7,col_num)]
                sp<-sp[complete.cases(sp),]
                
                ord<-order(sp[,3],sp[,1])
                ##print(sp[ord,c(2,col_num)])
                if (num=="worst") {num <-length(ord)} else num<-num
                
                hosp=sp[ord[num],1]
                
                if (as.numeric(num)>length(ord)) 
                {       hosp=NA
                        
                }
                
                
                state=sp[1,2]
                d<-data.frame(hosp,state)
                names(d)<-c("hospital","state")
                df<-rbind(df,d)
                
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        #
        df[-1,]
}
