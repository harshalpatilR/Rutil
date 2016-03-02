source("201511_Harshal_R_Functions.R")
options(scipen=999)

## Venture funding in 2015 and 2014
y <- rcsv("20151124_AngelList_CBI_Company_List_homogenized.csv")


x <- rcsv("combined_angel_cb_V6.csv")
x$raw.funding.dates.modf <- gsub("[/-]","_",x$raw.funding.dates)

x_y <- merge(y,x,by.x="Company",by.y="Company",all.x=T)
wcsv(x_y,"Readded_columns.csv")

fundlist <- strsplit(x$raw.funding.amount,";")
datelist <- strsplit(x$raw.funding.dates,";")
roundlist <- strsplit(x$raw.funding.stage,";")

# yearly breakdown saved
yearmatrix <- matrix(data=0.00,nrow(x),25,dimnames = list(row.names(x),c(2015:(2015-24))))
colnames(yearmatrix)
rownames(yearmatrix)

names(x)

# factor for source to get formatting
src.ft <- factor((x$src),levels=c("AngelList","CBInsight","Both AngelList & CBInsight"))
#nlevels(src.ft)
#"CBInsight"
#as.Date(datelist[[2200]],"%b %d %Y")


### do this for each index in datasource 
tempfx <- function(i){
    ## numeric vector
    #i=10
    temp<-gsub("\\$","",fundlist[[i]])
    temp<-gsub("Unknown","0",temp)
    temp<-(as.numeric(trim(temp)))
    
    ## rounds of funding
    

    ##character vector - format based on src
    yearloc <- c(3,3,1)
    ## AngelList, CBInsights single, CBI multiple
    delim <- c("%b %d %Y","%d/%m/%Y","%Y-%m-%d")
    
    ##
    ##i=586
    multi <- 10^6
    whatsrc <- x[i,"src"]
    if(whatsrc=="AngelList"){
        dateform <- delim[1]
        multi <- 1
    } else if (length(temp)>1){
        dateform = delim[3]
    } else
    {
        dateform = delim[2]
    }
    datevec <- as.character(as.Date(trim(datelist[[i]]),
                                    format=rep(dateform,length(datelist[[i]]))))
    ## once a date - it alway has the same delim
    yearvec <- sapply(strsplit(datevec,"-"),function(x){x[1]})
    
    fdf <- data.frame()
    
    ## assign values to matrix
    for(p in c(1:length(yearvec))){
        ## add in case multiple rounds in same year
        #yearmatrix[i,yearvec[p]] <- yearmatrix[i,yearvec[p]] + temp[p]
        opdf <- data.frame(row = i, company=x[i,"Company"],
                           year =yearvec[p],date=as.character(datevec[p]),
                           usd = (temp[p] * multi),
                           round = roundlist[[i]][p],
                           src =whatsrc)
        if(p==1){
            fdf <- opdf
        } else {
            fdf <- rbind(fdf,opdf)
        }
    }

    
    ##as.numeric(yearmatrix[i,])
    return(fdf)
}

coldf <- lapply(c(1:nrow(x)),tempfx)
coldf <- do.call(rbind,coldf)

##
coldf[coldf$row==586,]
##
coldf$date <- gsub("-","_",coldf$date)
wcsv(coldf,"Funding_Seperated.csv")
## add back columns
