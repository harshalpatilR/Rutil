source("201603_Harshal_R_Functions.R")

#### 1. take coverage fields from consolidated coverage file incl sector
n1 <- combine.csvs.dt("1h16 ent coverage consolidated",".*csv",add_name=F,allchar=T)
n1$cmr <- paste("00",n1$cmr,sep="")
unique(n1$sec)
names(n1)

#### 2. just add MF installed acct
mf1 <- combine.csvs.dt("1h16 mf systems",".*csv",add_name=F,allchar=T)
mf1$mfinstall <- 1
names(mf1)
setnames(mf1,"ctry","ctry.code")
setnames(mf1,"custno","cmr")
mf1$cmr <- sapply(mf1$cmr,function(x){paste(paste(rep("0",8-nchar(trim(x))),collapse=""),trim(x),sep="")})

mf1install <- mf1[,.(mfinstallmax=max(mfinstall)),by=.(ctry.code,cmr)]

#### 3. Add MF installed tag into coverage
n2 <- merge(n1,mf1install,all.x=T,by=c("ctry.code","cmr"))
n2[mfinstallmax==1,]

wcsv(n2,"mfintegrated.csv")

#### 4. Add CW for systems server
cw1 <- combine.csvs.dt("1h16 systems cw",".*csv",add_name=F,allchar=T)
names(cw1)
options(datatable.optimize=1)
cw2 <- cw1[,.(sysopfy16=max(op.systems.servers.fy),
              sysrevfy15=max(rv.systems.servers.cy),CWN=.N)
              ,by=.(iso.country.code,company.name,duns.no)]

cw2

#### 5. Add DUNS to coverage
duns1 <- combine.csvs.dt("1h16 covg duns",".*csv",add_name=F,allchar=T)
duns2 <- duns1[,.(dunsn=.N),by=.(src.key1,src.key2,duns.no,ctry.code)]
setnames(duns2,"src.key1","iso.country.code")
setnames(duns2,"src.key2","cmr")
duns2$iso.country.code <- trim(duns2$iso.country.code)

## merge with CW
cw3 <- merge(duns2,cw2,all.x=T,by=c("iso.country.code","duns.no"))
cw2$duns.no
cw2$iso.country.code
duns2$duns.no
duns2$iso.country.code

# has duns op and cmr
cw3[CWN==1,]

#### 6. Link back to n2
cw3$ctry.code
cw3$cmr <- trim(cw3$cmr)
n3 <- merge(n2,cw3,all.x=T,by=c("ctry.code","cmr"))
wcsv(n3,"mfcwintegrated.csv")


#### 7. 
l1 <- combine.csvs.dt("1h16 hgd linux",".*csv",add_name=F,allchar=T)
setnames(l1,"dunsnumber","duns.no")
setnames(l1,"country","iso.country.code")

fields <- names(n3)
fields <- c(fields[2:6],fields[10:13])
n4 <- n3[,.(mfinstallmax1=max(mfinstallmax)),by=fields]
n4[mfinstallmax1==1,]

wcsv(n4,"n4.csv")

l2 <- merge(l1,n4,all.x=T,by=c("iso.country.code","duns.no"),allow.cartesian=T)
wcsv(l2,"linux w details.csv")



##############################
s1 <- combine.csvs.dt("1h16 covg duns",".*csv",add_name=F,allchar=T)
s2 <- combine.csvs.dt("1h16 hgd linux",".*csv",add_name=F,allchar=T)
smfs <- combine.csvs.dt("1h16 mf systems",".*csv",add_name=F,allchar=T)

# 114 unique CMRs
nrow(smfs[,.N,by=.(ctry,custno)])
# 105 unique Client IDs
nrow(smfs[,.N,by=.(ctry,dom.client.id)])

mfinstalled <- smfs[,.N,by=.(ctry,custno)]
mfinstalled[,mfinstall:=1]

names(s1)
names(smfs)
names(s2)




#################
#s1mf <- merge(s1,smfc,all.x=T,by=c("duns.no","country"),allow.cartesian=TRUE)
mfinstalled$custno <- paste("00",trim(mfinstalled$custno),sep="")
setnames(mfinstalled,"ctry","ctry.code")
setnames(mfinstalled,"custno","cmr.num")

chk <- merge(s1,mfinstalled,all.x=T,by=c("ctry.code","cmr.num"))
chk[,N:=NULL]
names(chk)
s1 <- chk

### S1 unique duns only - subtract sets
nset <- names(s1)
exset <- c("cmr","cmr.num","client.name","src.key1","src.key2","fname","dcid")
uset <- setdiff(nset,exset)

# rules with data.table
# see if a vector of strings works as is
# see if list works
# see if with=F works
s1dedup <- s1[,.N,by=uset]
names(s1dedup)
wcsv(s1dedup,"s1dedup.csv")
# 
chk <- s1dedup[,.N,by=.(ctry.code,duns.no)]

### S2 unique duns+name+product
nset <- names(s2)
uset <- c(nset[1:12],nset[18])

s2dedup <- s2[,.N,by=uset]

## merge linux + coverage
setnames(s2,"dunsnumber","duns.no")
setnames(s1dedup,"iso.cd","country")

s3 <- merge(s1dedup,s2,all.x=T,by=c("duns.no","country"),allow.cartesian=TRUE)

wcsv(s3,"1h16 ent covg with linux.csv")


## merge linux + coverage + cw
s4 <- combine.csvs.dt("1h16 systems cw",".*csv",add_name=F,allchar=T)
names(s4)
setnames(s4,"gmv.country.cd","country")

s5 <- merge(s3,s4,all.x=T,by=c("duns.no","country"))
wcsv(s5,"1h16 ent covg with linux w cw.csv")

## mainframe clients



############################## Enterprise Coverage 

c1 <- combine.csvs.dt("ent coverage src",".*csv")
names(c1)

sec1 <- unique(c1$fname)
sec2 <- regmatches(c1$fname,regexpr("CMR[ ].*csv$",c1$fname,ignore.case=T))
sec3 <- gsub("CMR |.csv","",sec2)

c1$sec <- sec3

wcsv(c1,"1H16_ent_covg_comb.csv")

############################## 6K and above linux models
# gartner mkt share pivot by OS
s1 <- combine.csvs.dt("src",".*csv")
names(s1)
s1[,.N,os.family]
wcsv(s1[os.family=="Linux",.N,price.band.3],"price_bands.csv")

# build this from above output
m1 <- rcsv("price_bands_select.csv")
m1 <- as.data.table(m1)
gt6k <- m1[select==1,price.band.3]

# get Greater than 6K models from all
s1[price.band.3 %in% gt6k,.N,
   list(os.family,os,vendor,brand,sub.brand,model,price.band.3)]

linuxmodel <- s1[(price.band.3 %in% gt6k) & os.family == "Linux",.N,
   list(os.family,os,vendor,brand,sub.brand,price.band.3)]

wcsv(s1[price.band.3 %in% gt6k,.N,
   list(os.family,os,vendor,brand,sub.brand,price.band.3)],"GT6K_server_models.csv")

# match the linux GT6K with what is available from HGdata->models
s2 <- combine.csvs.dt("hg",".*csv")
matchmodel <- linuxmodel[vendor!="IBM",]

match <- name_match_wtokens(s2,matchmodel,"product","sub.brand",tokens=3)
match <- name_match_wtokens(s2,matchmodel,delim="|","product","brand",tokens=1)
