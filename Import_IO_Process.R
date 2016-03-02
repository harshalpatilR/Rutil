source("201511_Harshal_R_Functions.R")

## read
x<-universe_merge_files("./importiosrc",".*csv")
## make sure you replace it with semi-colon in csv if not done already

#------------------------------------------------------------
## most recent funding stage
fundlist <- strsplit(x$funding.stage,";")
# check what unique values exit
#unique(unlist(fundstage))
flevel <- c("-","No Stage","Seed","Series A","Series B","Series C",
            "Series D","Series E","Series F")
#
#x<-c(fundlist[[4]],NA,"unxhhdgd")
tempfx <- function(x){
    x <- trim(x)
    x[is.na(x)] <- "-"
    x[!(x %in% flevel)] <- "-"
    test <- factor(x, ordered=T,levels=flevel)
    as.character(max(test))
}

fundstage<-sapply(fundlist,tempfx)

#------------------------------------------------------------
## total funding
fundval <- strsplit(x$funding.amount,";")

tempfx <- function(x){
    test<-gsub("[//$]","",x)
    test<-gsub("unknown","0",tolower(test))
    test<-as.numeric(test)
    sum(test)
}

fundtotal<- sapply(fundval,tempfx)

#-----------------------------------------------------------
## latest funding date
dateval <- strsplit(x$funding.dates,";")

tempfx <- function(x){
    date<- as.Date(trim(x),"%b %d %Y")
    as.character(format(max(date),"%b %d %Y"))
}

funddate<-sapply(dateval,tempfx)


#-----------------------------------------------------------
x$fundstage <- fundstage
x$fundtotal <- fundtotal
x$funddate <- funddate

#-----------------------------------------------------------
## www.link
x$company.link <- paste("www",trim(x$company.link),sep=".")

#-----------------------------------------------------------
## HQ country (implied)
mapctry <- data.frame(unique(x$ctry))
wcsv(mapctry,"mapctry.csv")
# edit in excel
mapctry<-rcsv("mapctry_edited_v1.csv")

x<-merge(x,mapctry,by.x="ctry",by.y="src",all.x=T)


#-----------------------------------------------------------
## clean-up the locations from details
pattern <- paste(mapctry$src, collapse="|")

detaillist <- strsplit(x$details,";")
tempfx <- function(x){
    test <- gsub(pattern,"",x)
    test <- paste(trim(test),collapse="; ")
    test <- gsub("^(; )+","",test)
}
detailfixed <- sapply(detaillist, tempfx)

x$detailfixed <- detailfixed

#----------------------------------------------------------
### write file with converted fields
wcsv(x,"converted.csv")


#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# CB Insights Prep
#------------------------------------------------------------------------------------
#options(datatable.optimize=1)

inp2 <- universe_merge_files("./cbinsight",".*csv")
inp2 <- as.data.table(inp2)
str(inp2)

# counts are correct
inp2[,.N,sfname]

# build summary view - funding, series, latest round
inp2[,.N,company][order(N),] #check counts - mutiple rows

wcsv(inp2,"cbiconsolidated1.csv")

# FIX amount
# NAs set to zeros
inp2[is.na(amount),amount:=0]

# FIX date
inp2[,date:=as.Date(paste("01",trim(date),sep="-"),format="%d-%b-%y")]
# dummy date - remove if appears in final
inp2[is.na(date),date:=as.Date("1999-01-01")]

# Table for summary
# useful fields first
compcol <- colnames(inp2)[c(1:11,12,14,16,18)]
subcol <- compcol[c(1:11,15)]
inp3 <- inp2[,compcol,with=F]

compcol
subcol

# fix funding round order
unique(inp3$round)
inp3[,round:=factor(round,levels=c("Other","Other Venture Capital","Grant","Debt",
                                   "Growth Equity","Private Equity","Seed / Angel",
                                   "Series A" ,"Series B","Series C","Series D","Series E+"
                                   ),ordered=T)]
str(inp3)

#Summary views
inp3 <- inp3[order(-date),list(totalfund=sum(amount),
       maxround=max(round),
       maxdate=max(date),
       raw.funding.amount=paste(amount,collapse=";"),
       raw.funding.stage=paste(round,collapse=";"),
       raw.funding.dates=paste(date,collapse=";")),by=eval(subcol)]    	

inp3[,.N,by=company][order(N)]

# raw funding stages, amounts and dates

#raw funding details
#inp2 <- inp2[-order(date),]
#inp2[,raw.funddate:=paste(date,collapse="; "),by=company]

test1 <- inp2[company=="Lazada",][order(-date),]
test2 <- inp3[company=="Lazada",]
    
wcsv(inp3,"cbisummary.csv")

############################################
# Intersection of angellist and cbinsight

# IMP - works only with dataframes -
x<- data.frame(x)
inp3 <- data.frame(inp3)

match1 <- name_match_wtokens(x,inp3,"name","company")
namechk <- match1[,c("name","match1","raw_match1")]
chk_match_pct(match1,"",1)

wcsv(namechk,"matched1.csv")

##########################################
# - final format
#
angelf1 <- rcsv("20151118_AP_AngelList_Funded_Companies_s.csv")
names(angelf1)

matched <- rcsv("matched_modified.csv")
angelf2 <- merge(angelf1,matched,by.x="Company",by.y="angellist",all.x=T)

# Exclude CBInsight duplicates
angelf3 <- angelf2[is.na(angelf2$cbinsight),]

####
nameset1 <- names(inp3)[c(1,8,6,14,13,15,3,2,9,10,11,16,17,18)]
cbf1 <- inp3[,nameset1]

names(cbf1) <- c("Company","HQ.Country...implied.","Location...Additional.Details.",
                 "Highest.Funding.Stage","Total.Funding.USD","Latest.Funding.Date",
                 "Company.Desc1","Company.URL","sector","industry","sub.industry",
                 "raw.funding.amount","raw.funding.stage","raw.funding.dates")

cbf2 <- merge(cbf1,matched,by.x="Company",by.y="cbinsight",all.x=T)
cbf2 <- merge(cbf2, angelf2, by.x="angellist","Company",all.x=T,suffixes = c("",".y"))

# pull useful text from angellist
cbf1$Company.Desc2 <- cbf2[match(cbf1$Company,cbf2$Company),"Company.Desc2"]
cbf1$Company.Focus.Areas <- cbf2[match(cbf1$Company,cbf2$Company),"Company.Focus.Areas"]
cbf1$matched_angel <- cbf2[match(cbf1$Company,cbf2$Company),"angellist"]
cbf1$AngelList.Profile.URL <- cbf2[match(cbf1$Company,cbf2$Company),"AngelList.Profile.URL"]

# convert to char
cbf1$Highest.Funding.Stage <- as.character(cbf1$Highest.Funding.Stage)
cbf1$Latest.Funding.Date <- as.character(format(cbf1$Latest.Funding.Date,
                                                 format="%b %d %Y"))
cbf1$Total.Funding.USD <- format(cbf1$Total.Funding.USD * 10^6,format="d", big.mark=',')

# ADD source
angelf3$src <- "AngelList"
cbf1[,"src"] <- "CBInsight"
cbf1[!is.na(cbf1$matched_angel),"src"] <- "Both AngelList & CBInsight"

# for matching add Desc2 and 


### Merge unique portions - unmatched into a single file
str(angelf3)
str(cbf1)

mfnl1 <- rbind.all.columns(angelf3,cbf1)

### final nitpick
mapctry<-data.frame(unique(mfnl1$HQ.Country...implied.))
wcsv(mapctry,"mapctry.csv")
mapctry <- rcsv("mapctry_modified.csv")

mfnl2 <- df.get.lkup.val(mfnl1,mapctry,"HQ.Country...implied.","src","HQ.Country...implied.",
                "dest")

wcsv(mfnl2,"combined_angel_cb.csv")

