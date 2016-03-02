source("201511_Harshal_R_Functions.R")

### clustered outputs
matchfiles <- c("20151124_cluster_output_refined2_V1_b1.csv",
                "AP_AngelList_CBInsight_clustered_V1.csv")
path <- "./matchsrc/"

flist <- lapply(matchfiles,function(x,y=path){f<-rcsv(px(y,x))})

#matched1 <- name_match_wtokens(flist[[1]],flist[[2]],scol="company.name","Company",tokens=2)
#wcsv(matched,"one_token.csv")
#wcsv(matched1,"two_token.csv")

###Base is CapIQ data.
###matchedflag=1 and matchedname=name from angellist file 
matchedf <- rcsv("one_token_fixed_v1.csv")
names(matchedf)

capiqmatched <- df.get.lkup.val(matchedf,flist[[2]],"matchedname","Company",
                          "src","Data.Source",nomatch="CapIQ only")
table(capiqmatched$src)
wcsv(capiqmatched,"CapIQ_Matched.csv")

###### Now bring matching CapIQ into Angellist files
angelmatched <- df.get.lkup.val(flist[[2]],capiqmatched,"Company","matchedname",
                                "capiq_matched","src",nomatch="Not in CapIQ")

angelmatched <- df.get.lkup.val(angelmatched,capiqmatched,"Company","matchedname",
                                "capiq_name","company.name",nomatch="Not in CapIQ")



table(angelmatched$Data.Source,angelmatched$capiq_matched)
wcsv(angelmatched,"Angel_CBI_Matched.csv")


######-------------------------------------------------------------------
## Do a clean cut job

# intersection















