rf <-function()
{
        files_full<-list.files("diet_data",full.names=TRUE)
        summary(files_full)
        tmp<-vector(mode="list",length=length(files_full))
        tmp<-lapply(files_full,read.csv)  ##ta linia jest rownowazna ponizszej petli
        ##for (i in seq_along(files_full))
        ##{
        ##        tmp[[i]]<-read.csv(files_full[i])
        ##}
        output<-do.call(rbind,tmp)
        output
}