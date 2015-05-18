source("20150404_Harshal_R_Functions.R")

########################################################################
path_all <- list(
    c("../20 CAMS ASEAN","../21 MSP ASEAN"),
    c("../../../AP GTS Acquisition/Voyager_150206"),
    c("ID","MY","SG","TH","VN","PH"),
    "./"
    )
preamble_all <- list("","",
                     c("../../0VOYAGER_UNIVERSE_VIEWS/sources/"),
                     "")

pattern_all <- list("Company.*csv",
                    "MSP.*Company.*csv",
                    "Company.*csv",
                    "SawPheng.*csv"
                    )

##### Index needs to be along the LONGEST list so each element gets unique number
index <- 1:length(path_all)

### do.call function
build_voyager_df <- function(preamble, path, pattern, pidx)
{
    # elementwise application of function on list elements
    pathx <- mapply(paste,preamble,path,sep="")
    univc.df <- universe_merge_files(pathx,pattern)
    univc.df$index <- pidx
    return(univc.df)
}

### apply on list of parallel arguments
x <- mapply(build_voyager_df,preamble_all,path_all,pattern_all,index)
#str(x)

### how many duplicates within each consolidation
### count1 <- lapply(x,function(x) {sum(table(x$code)>1)})

### Total and how many duplicates in TOTAL
x <- do.call(rbind.all.columns,x)
##test <- unique(x[,c("sfname","index")])
count2 <- table(x$code)
hist(count2)

### tag the ones duplicate
x<-cbind(x,cdup=count2[as.character(x$code)])
names(x)
x[is.na(x)] <- ""
x <- unique(cbind(x[,1:51], x[,c("index","cdup")]))

### information is varying so pick 
### 1. From recent work OR
### 2. First line
### BY function
get_unique_record <- function(x)
{
    ### just pick first row
    default <- x[1,]
    ### if first priority exists then pick the first row in that
    if(1 %in% unique(x$index)){
        default <- x[x$index==1,]
        default <- default[1,]
    }
return(default)
}

x <- do.call(rbind,by(x,x$code,get_unique_record))
### Operating and EXCLUDE HR
x <- x[x$status=="Operating",]

### excl industries
ind_count <- data.frame(table(x$ind_name))
excl_ind <- c("Human Resources",
              "Staffing & Recruiting")
x <- x[!(x$ind_name %in% excl_ind), ]

### ASEAN ALL
wcsv(x,"201505_UNIV_Voyager_ASEAN.csv")

### ASEAN HQ
incl_ctry <- c("sg","my","th","id","ph","vn")
x <- x[(x$hq_ctry %in% incl_ctry), ]
wcsv(x,"201505_UNIV_Voyager_ASEAN_HQonly.csv")

ctry_cnt <- data.frame(table(x$hq_ctry))

#########################################################################################
### Consolidated File Ready
#########################################################################################

### Tag Keywords and Count
path <- c("./keywords")
keyw <- universe_merge_files(path,".*csv")
srcvec <- do.call(paste,x[,c(27:51,11)])
keym <- match_keywords(srcvec,keyw[,1],"all")
x <- cbind(x,keym)

table(x$hq_ctry,x$all_Key_Count>0)

### Exclude existing work
path <- c("../../0VOYAGER_UNIVERSE_VIEWS/ASEAN Universe/prev")
sgw <- universe_merge_files(path,".*csv")
ex <- x[!(x$code %in% unique(sgw$Linkedin.Company.Code)),]

table(ex$hq_ctry,ex$all_Key_Count>0)
wcsv(ex,"Excl_PrevWork_ASEAN.csv")

#################################################
incl_ind <- c("Information Technology & Services")
fex <- ex[(ex$ind_name %in% incl_ind), ]
names(fex)
### Cluster and Count
srcvec <- do.call(paste,fex[,c(27:51)])

require(tm)

clusno <- cluster_docs(srcvec)
excl <- cbind(cluster=clusno[[1]][[1]],srcvec=clusno[[2]],fex)
keym <- match_keywords(srcvec,keyw[,1],"all")
excl <- cbind(keym,excl)
wcsv(excl,"Industry_Clustered_ASEAN.csv")

###############################################
require(tm)
pxdelim <- function(...){
    return(paste(...,sep=" "))
}
### Cluster and Count
srcvec <- do.call(pxdelim,fex[,c(27:51,11)])

chk <- verify_keyword_match("cloud.{1,20}solutio.", srcvec)
chk <- verify_keyword_match("data.cent.{1,8}in", srcvec)
chk <- verify_keyword_match(".{10}hosting", srcvec)
chk <- verify_keyword_match(".{20}managed.*?service", srcvec)
chk <- chk[[2]]
chk_table <- data.frame(table(chk[,1]))
chk_table <- chk_table[order(-chk_table$Freq),]


#################### TOPIC MODELING
skills <- Corpus(VectorSource(srcvec))
skills <- tm_map(skills, tolower)
skills <- tm_map(skills,removeWords,stopwords("english"))
skills <- tm_map(skills,removeWords,removeNumbers)
skills <- tm_map(skills,removePunctuation, preserve_intra_word_dashes = TRUE)

require(topicmodels)
dtm <- DocumentTermMatrix(skills)
## Atleast 2 docs
##
freqterms <- findFreqTerms(dtm,0.2*length(srcvec))
## REMOVE freq terms
skills <- tm_map(skills,removeWords,freqterms)
skills <- Corpus(VectorSource(srcvec))
dtm <- DocumentTermMatrix(skills)
dtm <- removeSparseTerms(dtm, 1-(2/length(srcvec)))

total_tf <- apply(dtm,2,function(x){sum(x)})
total_tf <- sort(total_tf,decreasing=T)

#dtm.filtered = DocumentTermMatrix(dtm, dictionary = dtm.dict)

ingredient.counts <- apply(dtm, 1, function (x) sum(x))
recipes.dtm.filtered <- dtm[ingredient.counts > 0,]

recipes.lda = LDA(recipes.dtm.filtered, 50)
t = terms(recipes.lda,20)

findFreqTerms(dtm,500)
chk <- data.frame(findAssocs(dtm,"solution",0.00001))

