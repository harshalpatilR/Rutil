source("20150404_Harshal_R_Functions.R")

### Gartner Market-Share
g.df <- rcsv("Software_Marketshare_All_2015Q1.CSV")

### Mapping
map.df <- rcsv("Gartner_Offerings_Mapping.csv")

### Validate the below is correct
names(g.df)

### Year
yrv <- unique(g.df[,1])
### Vendor
vv <- unique(g.df[,2])
### Region
regv <- unique(g.df[,3])
### Market
mktv <- unique(g.df[,6])
### Segment
segv <- unique(g.df[,7])


#### Select YEAR, REGION
g1.df <- subset(g.df, 
                g.df$Region %in% c("Emerging Asia/Pacific","Mature Asia/Pacific") & 
                g.df$Year %in% c("2014 YR")    )

#### Build country views
unique(g1.df$Country)
ctryv <- c("Australia","Singapore","India","South Korea")
maprow <- 1:nrow(map.df)

####


#### GET VENDOR SHARE
#### FOR EACH CTRY
for(x in ctryv)
{
    opdf <- map.df
    #### FOR EACH SEGMENT
    for(y in maprow){
        ####x.df <- g1.df
        ####y <- 1
        
        #### CTRY
        x.df <- subset(g1.df, g1.df$Country == x)
        
        #### SEGMENT or MARKET
        field <- map.df[y,"Matched_Seg"]
            
        #### FILTER ON SEGMENT
        #### You COULd FILTER ON MULTIPLE - change reqd for multiple
        x.df <- subset(x.df, trim(x.df[,field]) %in% trim(as.character(map.df[y,6])))
        
        tab.df <- cast(x.df,Vendor~Year,margins=c("grand_row"),value="Revenue..M....USD",fun=sum)
        test <- data.frame(lapply(tab.df,function(x){x/x[length(x)]}))
        test <- test[,2:ncol(test),drop=F]
        ###wcsv(cbind(tab.df,test),px(x,"_total.csv"))
        share.df <- cbind(tab.df,test)
        names(share.df) <- c("Vendor","dollar","pct")
        share.df$Segment <- map.df[y,6]
        share.df$Offering <- map.df[y,"Offerings"]
        
        ####
        if(nrow(share.df[share.df$Vendor=="IBM",])>0){
            opdf[y,"pct"] <- share.df[share.df$Vendor=="IBM","pct"]
            opdf[y,"dollar"] <- share.df[share.df$Vendor=="IBM","dollar"]
        }
    }

wcsv(opdf,px(x,"_IBM_Share.csv"))
}

