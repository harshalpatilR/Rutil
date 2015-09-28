require(data.table)
require(tm)
require(stringdist)
require(reshape)

options(stringsAsFactors=FALSE)
options(scipen=200000)


############################################################################################################

### -------------------------------
# dataframe
# row variable name
# col variable name
# data vairable name
# data function to be applied
# row variable - vector of values to be applied as filter
# col variable - vector of values as filter

hCrossTab <- function(df, r_var, c_var, d_var, r_vector, c_vector, duplicates = F, data_fun = sum,
                      data_table_inp=F)
{
  # TEST VALUES
  #df = sub_rev
  #r_var ="DASHBOARD_ACCT_NAME"
  #c_var = "Acquistion.in.Dashboard"
  #d_var ="REV_PLN"
  #r_vector = acc_list[,"DASHBOARD_ACCT_NAME"]
  #c_vector = acq_grp[,"Acquistion.in.Dashboard"]
  #duplicates = F
  #data_fun = sum
  
  
  ### Duplicates allowed or unique only
  if (duplicates)
  {
    #rows and columns in output crosstab
    r_xtab = length(r_vector)
    c_xtab = length(c_vector)
  }else
  {
    #rows and columns in output crosstab
    r_xtab = length(unique(r_vector))
    c_xtab = length(unique(c_vector))
    r_vector = unique(r_vector)
    c_vector = unique(c_vector)
  }
  
  #init row_vector x column_vector matrix of zeros
  output_m <- rep(0.0000, r_xtab * c_xtab)
  dim(output_m) <- c(r_xtab, c_xtab)
  colnames(output_m) <- c_vector
  
  # apply row filter
  # loop
  ri = 1
  ci = 1
  
  # repeat for all accts in rows
  while (ri <= r_xtab){
    
    # repeat for all accts in columns
    while (ci <= c_xtab) {
          
      # subset for row & col specific data
      tempdf <- subset(df, (df[,r_var] == r_vector[ri]) & (df[,c_var] == c_vector[ci]) )
      
      if (nrow(tempdf)>0) {
          # apply function - typically sum  
          output_m[ri,ci] <- data_fun(as.numeric(tempdf[,d_var]))
      } else {
          output_m[ri,ci] <- "ZeroMatched"
      }
      
      ci = ci + 1
    }  
    
    ## initi ci again for next columns
    ci = 1
    ri = ri + 1
  }
  
  
  final_df <- data.frame(r_vector)
  colnames(final_df) <- r_var
  final_df <- cbind(final_df, data.frame(output_m))
  
  return(final_df)
}

###############################################################################################
### Hassle Free Combining of TWO dataframes with different set of columns
###############################################################################################

rbind.match.columns <- function(input1, input2) {
  n.input1 <- ncol(input1)
  n.input2 <- ncol(input2)
  
  if (n.input2 < n.input1) {
    TF.names <- which(names(input2) %in% names(input1))
    column.names <- names(input2[, TF.names])
  } else {
    TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])
  }
  
  return(rbind(input1[, column.names], input2[, column.names]))
}

rbind.all.columns <- function(...) {
    z <- data.frame()
    if(nargs()>=2)
    {
        largs <- list(...)
        for(i in 1:(nargs()-1)){
            if(i==1){
                x <- largs[[i]]
                y <- largs[[i+1]]
            }
            else
            {
                x <- z
                y <- largs[[i+1]]
            }
            #debug()
            #### PRESENT in first & NOT in second
            x.diff <- setdiff(colnames(x), colnames(y))
            y.diff <- setdiff(colnames(y), colnames(x))
            
            x[, c(as.character(y.diff))] <- NA
            y[, c(as.character(x.diff))] <- NA
            
            z <- rbind(x, y)
        }
    } 
    
return(z)
}


###########################################################################################
## Tokenize and write - for name matching
############################################################################################
tokenize_h <- function(ipfile,opfile="Tokenized.CSV",ipcol=1,all_tokens=F,token_count_file="Token_counts.csv",
                       df=F)
{
###### UNIT TEST DATA
#ipfile = "BDA_BPs_ISA.csv"
#opfile = "BDA_BPs_Tokens.CSV"
#ipcol=1
#all_tokens=T

### Input is a dataframe    
if(!df){    
    ##### READ DATA
    src.df <- read.csv(ipfile,stringsAsFactors=F,strip.white=T)
} else {
    src.df <- ipfile
} 

##### Tokenize & count of tokens in each vector in list
token_list <- strsplit(src.df[,ipcol], " ")
num_char_token <- lapply(token_list,length)

##### what is max columns needed?
max_col <- max(unlist(num_char_token))

##### rows and columns of character matrix
mrow <- length(num_char_token)
mcol <- max_col



if(!(all_tokens))
{
  ###### matrix with maximum columns
  output_matrix <- matrix("",nrow=0,ncol=3)
  
  #### assign
  i <- 0
  while (i < mrow){
    element <- token_list[[i+1]]
    f_element <- c(element,rep("",mcol-length(element)))
    f_element1 <- c(f_element[1],f_element[2],do.call(paste,as.list(f_element[3:mcol])))
    output_matrix<-rbind(output_matrix, matrix(f_element1,nrow=1,ncol=3))
  
    i <- i+1
  }
}
else
{
  ###### matrix with maximum columns
  output_matrix <- matrix("",nrow=0,ncol=mcol)
  
  #### assign
  i <- 0
  while (i < mrow){
    element <- token_list[[i+1]]
    f_element <- c(element,rep("",mcol-length(element)))
    output_matrix<-rbind(output_matrix, matrix(f_element,nrow=1,ncol=mcol))
    
    i <- i+1
  }    
}

if(all_tokens)
{
####### build token counts if all_tokens=T
lookup_vector <- c("xt$7dummy")
count_vector <- c(0)

  i <- 0
  while (i<mrow){
    element <- token_list[[i+1]]
    
    j = 0
    while(j<length(element)){
      check <- element[j+1]      
      pos <- match(check,lookup_vector)
      
      if(is.na(pos)){
        lookup_vector <- c(check,lookup_vector)
        count_vector <- c(1,count_vector)
      }
      else{
        count_vector[pos] <- count_vector[pos] + 1
      }
    
      j <- j+1      
    }
  
    i <- i+1
  }
token_count.df <- data.frame(lookup_vector,count_vector)
write.csv(token_count.df,token_count_file,row.names=F, col.names=T)
}

#### write output files
write.csv(data.frame(output_matrix), opfile,  row.names=F, col.names=T)

}

###########################################################################################
# REMOVE TOKENS 
###########################################################################################
cleanup_tokens_h <- function(ipfile,opfile="Cleanup_Tokenized.CSV", opcleanupfile= "Cleanup_Orig.CSV",
                             ipcol=1,cleanupfile="Token_counts.csv",
                             cleanuptoken=1,cleanupflag=3)
{
  
  ### TEST
  #ipfile = "BDA_BPs_ISA.csv"
  #ipcol = 1
  #opfile="Cleanup_Tokenized.CSV"
  #cleanupfile="Token_counts.csv"
  #cleanuptoken=1
  #cleanupflag=3
  
  ##### READ DATA
  src.df <- read.csv(ipfile,stringsAsFactors=F,strip.white=T)
  cleanup.df<-read.csv(cleanupfile,stringsAsFactors=F,strip.white=T)
  
  #####
  cleanuptoken_v <- cleanup.df[,cleanuptoken]
  cleanupflag_v <- cleanup.df[,cleanupflag]
  
  cleanupflag_v[is.na(cleanupflag_v)]<-0
  
  #### more than zero means cleanup
  cleanuptoken_v <- cleanuptoken_v[cleanupflag_v>0]
  
  ##### Tokenize & count of tokens in each vector in list
  token_list <- strsplit(src.df[,ipcol], " ")
  num_char_token <- lapply(token_list,length)
  
  ##### what is max columns needed?
  max_col <- max(unlist(num_char_token))
  
  ##### rows and columns of character matrix
  mrow <- length(num_char_token)
  mcol <- max_col
  
  ###### matrix with maximum columns
  output_matrix <- matrix("",nrow=0,ncol=3)
  orig_output_matrix <- matrix("",nrow=0,ncol=1)
  
  #### Match & Delete
  i <- 0
  while (i < mrow){
    element <- token_list[[i+1]]
    matching_pos <- match(cleanuptoken_v,element)
    element[matching_pos] <- NA    

    modf_element_x <- element[!is.na(element)]
    modf_element <- c(modf_element_x,rep("",mcol-length(modf_element_x)))
    
    f_element1 <- c(modf_element[1],modf_element[2],do.call(paste,as.list(modf_element[3:mcol])))
    output_matrix<-rbind(output_matrix, matrix(f_element1,nrow=1,ncol=3))
    
    orig_output_matrix<-rbind(orig_output_matrix, paste(modf_element_x,collapse=" "))
    
    i <- i+1
  }
  
  #### write output files
  write.csv(data.frame(output_matrix), opfile,  row.names=F, col.names=T)
  
  #### write/read/write with strip.white=T
  write.csv(data.frame(orig_output_matrix), opcleanupfile,  row.names=F, col.names=T)
}

############## function:
merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]		
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}
###############################################################################
px <- function(...,collapse=NULL)
{
    paste0(...,collapse)
}

##############################################################################
build_xtab_views <- function(src.df, view_files, acq_files, vp, acqp,
                             view_field_name, acq_field_name, x,y,z, op_prefix)
{
    
    #vp=layoutp
    #acqp=layoutp
    #view_field_name=3
    #acq_field_name=1
    #x="F_NAME"
    #y="ACCT_YR"
    #z="S_PLAN"
    #op_prefix="IMT_Annual_"
    
    i <- 1
    views <- length(view_files[view_files != ""])
    
    while (i <= views)
    {
        # req output - vectors
        # acct names
        acc_list <- read.csv(px(vp,view_files[i]), strip.white = TRUE, stringsAsFactors = F)
        
        # acquisitions sequence
        dash_acq <- read.csv(px(acqp,acq_seq[i]),strip.white=T, stringsAsFactors = F )
        
        output_final <- hCrossTab(src.df,x,y,z, acc_list[,view_field_name], 
                                  dash_acq[,acq_field_name])
        
        ##### write excel
        nfile = px(op_prefix,"op",view_files[i])
        write.csv(output_final,file = nfile, row.names = F )
        
        #### next output
        i <- i + 1
        
    }
    return(T)
}

########################################################################################
combine_csvs <- function(file_list, add_file_name=T)
{
    i <- 1
    max <- length(file_list)
    
    while (i<=max){
        df <- read.csv(file_list[i],stringsAsFactors=F, strip.white=T)
        df$sfname <- file_list[i]
        
        if(i==1)
        {
            cons <- df
        } else
        {
            cons <- rbind.all.columns(cons,df)
        }
        
        i <- i+1    
    }
    
    return(cons)
}

###########################################################################################
### TRIM Functions
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# trim excessive space in between, newline and carriage returns
trim.excess <- function(x) trim(gsub("\\s+|\\n|\\r"," ",x))

# trim special characters
trim.special <- function(x) trim(gsub("[\\(|\\)|\\,|\\&|\\-]","",x))

##################################################################################
break_keywords <- function(ivec,delim)
{
    #ivec = '("enterprise 2.0" OR "Social Biz" OR "social business" OR "#SocBiz" OR "#SocialBiz" OR "#socialbusiness" OR SocBiz OR SocialBiz OR socialbusiness OR "enterprise social business" OR "enterprise social network" OR "enterprise social grid" OR "social CRM" OR "social BPM") AND ("activity stream" OR "activity streams" OR "application development" OR "best practices" OR "business model" OR "business models" OR "content management" OR "crowd sourcing" OR "customer service" OR "disruptive technologies" OR "disruptive technology" OR "human resources" OR "information technology" OR "next generation social" OR "open social" OR "organizational culture" OR "predictive analytics" OR "product lifecycle management" OR "social analytics" OR "social mail" OR "social portal" OR "social portals" OR "social commerce" OR "customer experience management" OR "innovation management" OR "social app" OR "social application" OR "social applications" OR "social apps" OR "social business app" OR "social business application" OR "social business applications" OR "social business apps" OR "social collaboration" OR "social computing" OR "social learning" OR "social media" OR "social network" OR "social networks" OR "social platform" OR "social platforms" OR "social software" OR "social softwares" OR "social technologies" OR "social technology" OR "software" OR "tool kit" OR "unified telephony" OR "analytics" OR "app" OR "application" OR "applications" OR "apps" OR "benefit" OR "benefits" OR "blog" OR "blogs" OR "bookmark" OR "bookmarks" OR "BPM" OR "collaboration" OR "collaborative" OR "commerce" OR "communication" OR "communities" OR "community" OR "compliance" OR "innovation" OR "connection" OR "connections" OR "CRM" OR "crowdsourcing" OR "engagement" OR "ERM" OR "information" OR "intelligence" OR "intelligent" OR "interaction" OR "interactive" OR "internet" OR "interoperability" OR "learning" OR "mail" OR "marketing" OR "mobile" OR "network" OR "nimble" OR "optimization" OR "optimized" OR "organization" OR "organizational" OR "organizations" OR "platform" OR "platforms" OR "portal" OR "portals" OR "productive" OR "productivity" OR "risk" OR "risks" OR "sales" OR "software" OR "softwares" OR "technologies" OR "technology" OR "tool" OR "toolkit" OR "tools" OR "transparency" OR "transparent" OR "value" OR "voIP" OR "wiki" OR "collective intelligence" OR "customer self service" OR "social grid" OR "content" OR "social content" OR "insights" OR "social selling" OR "social communications" OR "collaborative communications" OR "human capital management" OR "business process management" OR "enterprise resource management" OR "enterprise resource planning")'
    #delim = "OR"
    
    tokens <- unlist(strsplit(ivec,split=delim))
    ### remove double quotes
    ### remove  ()
    ### remove excess space, add . instead of space as grep pattern
    ### keep literal dot
    tokens <- tolower(tokens)
    tokens <- gsub("\"","",tokens)
    tokens <- gsub("[(|)]","",tokens)
    tokens <- gsub("and","|",tokens)
    tokens <- gsub("[.]","[.]",tokens)
    tokens <- trim(tokens)
    tokens <- gsub(" ",".",tokens)
    
    return(tokens)
}
#################################################################################
#### Purpose : start at directory in listdir, go thru entire structure 
#### incl subdirectories and identify all files matching pattern.
#### ignore hidden files, ignore directory names
#### READ ALL files and combine in one dataframe
#### sfname holds source file name

# listdir - relative to current working directory
universe_merge_files <- function (listdir,patternx,onlyfilelist=F,nofilenames=F)
{
    allfilelist <- c()
    i <- 1
    final <- data.frame()
    
    #### each country has one vector of file names by industry - combine all in one shot
    while(i<=length(listdir)){
        fl1 <- list.files(path=listdir[i],pattern=patternx,full.names=T,recursive=T,ignore.case=T,include.dirs=F,no..=T)        
        allfilelist <- c(allfilelist,fl1)

        #### not matching files then do not proceed
        if (!onlyfilelist && length(fl1)>0){
            #### read data in DF
            test <- combine_csvs(fl1)
            
            ### add country to final
            if(nrow(final)==0)
            {
                final <- test
            }else{
                
                final <- rbind.all.columns(final,test)            
            }
            
        }
        i <- i+1
    }
    
    #### FIX Names
    namesv <- names(final)
    ### Valid Names in lowercase - remove duplicate "."s
    ###setnames(fdt,namesv,gsub("\\.+",".",tolower(make.names(namesv,unique=T,allow=F))))
    names(final)<-gsub("\\.+",".",tolower(make.names(namesv,unique=T,allow=F)))

    if (!onlyfilelist){
        flist <- final
    }else{
        flist <- allfilelist
    }
        
    
    return(flist)
}

################################################################################
match_keywords <- function(srcvec, keyvec, prefix, case.ignored=T)
{
    #### TEST
    #srcvec <- comp.df$desc
    #keyvec <- as.vector(read.csv("./keywords/Use_Cleanup_Social_Business_Keywords.csv",stringsAsFactors=F,strip.white=T))
    #keyvec <- keyvec[,1]
    ####### Loop over ALL records
    
    i=0
    i_len = length(keyvec)
    key_match <- rep("",length(srcvec))
    key_count <- rep(0,length(srcvec))
    
    while (i<i_len){
        
        mv <- grepl(keyvec[i+1],srcvec,ignore.case=case.ignored)
        key_match[mv] <- paste(trim(key_match[mv]),keyvec[i+1],sep=", ")          
        key_count <- key_count + as.numeric(mv)
        
        i = i+1  
    }
    
    ### not reqd
    key_match <- gsub("^,", "", key_match)
    key_match <- trim(key_match)
    
    rv <- data.frame(key_match, key_count)
    colnames(rv) <- c(px(prefix,"_Key_Match"),px(prefix,"_Key_Count"))

return(rv)
}
################################################################################################
#################################################################################
# Move to function files
# input : pattern and srcvec
# Output : returns list with 1. logical match vector, 2. matched results in df
# USEFUL : to check what is being matched by the pattern

verify_keyword_match <- function(pattern,srcvec)
{
    #### strong
    m_v <- regexpr(pattern, srcvec, ignore.case =T)
    chk_v.df <- data.frame(regmatches(srcvec, m_v))
    b_v <- grepl(pattern, srcvec, ignore.case =T)
    
    return(list(b_v,chk_v.df))
}

###################################################################################
# Move to function file
# input : srcvec containing "documents" i.e. text 
#       : no of clusters
#       : stem = F
# output : vector of cluster numbers

cluster_docs <- function(srcvec, nocluster=30, stem=F, toplaintext=F, 
                         sparsethr=0.995)
{
    
    skills <- Corpus(VectorSource(srcvec))
    
    if(toplaintext){
        ## Convert to Plain Text Documents
        skills <- tm_map(skills, as.PlainTextDocument)
        ##skills[[1]]
    }
    
    ## Convert to Lower Case
    skills <- tm_map(skills, tolower)
    ##skills[[1]]
    
    ## Remove Stopwords
    ### NOT ON SKILLS
    skills <- tm_map(skills, removeWords, stopwords("english"))
    ##skills[[1]]
    
    ## Remove Punctuations
    ### NOT ON SKILLS
    skills <- tm_map(skills, removePunctuation)
    ##skills[[1]]
    
    if(stem) {
        ## Stemming
        skills <- tm_map(skills, stemDocument)
        #skills[[1]]
    }
    
    ## Remove Numbers
    skills <- tm_map(skills, removeNumbers)
    ##skills[[1]]
    
    ## Eliminating Extra White Spaces
    skills <- tm_map(skills, stripWhitespace)
    ##skills[[1]]
    
    ## create a term document matrix
    dtm <- DocumentTermMatrix(skills)
    ##inspect(dtm)
    ##dtm
    
    if(sparsethr>0){
        ##### REDUCE terms counts - atleast 2 documents 
        dtm <- removeSparseTerms(dtm,1-(2/length(srcvec)))
        ###dtm
    }
    #### REMOE TOP 10 most frequent words - noise
    total_tf <- apply(dtm,2,function(x){sum(x)})
    total_tf <- sort(total_tf,decreasing=T)
    skills <- tm_map(skills,removeWords,total_tf[1:10])
    dtm <- DocumentTermMatrix(skills)
    
    ####### INSPECT
    ##findFreqTerms(dtm, 5)
    ##assoc <- data.frame(unlist(findAssocs(dtm, c("software","solution","based"), .05)))
    
    
    ## do tfxidf
    dtm_tfxidf <- weightTfIdf(dtm)
    ##inspect(dtm_tfxidf)
    ##inspect(dtm_tfxidf[1:10, 1:10])
    
    ## do document clustering
    
    ### k-means (this uses euclidean distance)
    m <- as.matrix(dtm_tfxidf)
    
    ### don't forget to normalize the vectors so Euclidean makes sense
    norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
    m_norm <- norm_eucl(m)
    ##m_norm[1:10, 1:10]
    
    ###### CLEAR DIV ZERO ERRORS
    m_norm_nan <- is.nan(m_norm)
    m_norm[m_norm_nan] <- 0
    
    cl <- kmeans(m_norm, nocluster)
    
    process_docs <- rep("",length(skills))
    for (i in 1:length(skills))
    {
        # Corpus -> list -> each element -> (text + metadata)
        process_docs[i] <- skills[[i]][[1]]
    }
    
    return(list(cl,process_docs))
}

##########################################################################
# REMOVE tokens
#########################################################################
## This function is used to remove words from a text column
## for cleaning up company names by excluding worthless words.
remove_tokens <- function(x.df,token_x,xcol=1,tokencol=1)
{
    ### EXCLUDE tokens
    for(i in 1:nrow(token_x))
    {
        x.df[,xcol] <- gsub(token_x[i,tokencol],"",x.df[,xcol],
                         ignore.case=T)
    }    
    
    x.df[,xcol] <- trim.special( x.df[,xcol])
    x.df[,xcol] <- trim.excess( x.df[,xcol])
    
    return(x.df)
    
}




#############################################################################
#Match Names using Stringdist
#############################################################################
### token_unlist is lapply function
token_unlist <- function(x,y)
{
    end <- min(y,length(x))
    r <- ""
    for(i in 1:end){
        r <- paste(r,x[i],sep=" ") 
    }
    return(trim(r))
}
####################################
### actual name matching function
name_match_wtokens <- function(s.df,lk.df, scol=1,lcol=1, tokens=1, 
                               delim=" ",seq=1,pwhat="",
                               nmethod="qgram",...)
{
    #### use either FULL name, a substring from start, specific # of tokens
    #### token version
    tokens <- max(1,tokens)
    
    m1 <- strsplit(as.character(s.df[,scol]),delim)
    m1 <- tolower(unlist(lapply(m1,token_unlist,tokens)))
    
    m2 <- strsplit(as.character(lk.df[,lcol]),delim)
    m2 <- tolower(unlist(lapply(m2,token_unlist,tokens)))

    ##### DECENT MATCH
    for(i in 1:nrow(s.df))
    {   
        ### element of vector m1 searched in m2
        mn <- amatch(m1[i],m2, method=nmethod,...)
        s.df[i,px("match",pwhat,seq)] <- lk.df[mn,lcol]
        s.df[i,px("raw_match",pwhat,seq)] <- m2[mn]
    }  
    
    return(s.df)
}

###############################################
## This function counts the NAs in columns representing specific subsets of data
## the key values are the various row values such as by argument in SQL
df_count_key_na <- function(idf, keyname, keyv)
{
    ### assign to relevant vars    
    b.df <- idf
    fnames_v <- keyv
    
    #counter
    i <- 1
    
    # empty Output Dataframe
    opdf <- data.frame()
    
    for(i in 1:length(fnames_v))
    {
        sdf <- subset(b.df,(b.df[,keyname] == fnames_v[i]))
        trow <- nrow(sdf)
        v<-unlist(lapply(sdf,function(x,y){(sum(is.na(x))/y)*100},trow))
        v <- as.matrix(t(v))
        rownames(v) <- fnames_v[i]
        opdf <- rbind(opdf,v)    
    }
    
    return(opdf)
}

###################################################
## This function allows us to sort columns in df by a total row
## Typically used after a cast function on a grand total row "(all)"

####################################################
## shortcut to read and write csv
wcsv <- function(df,file,unq=FALSE,...){
    if(!unq){
        write.csv(df,file,row.names=F,...)
    }
    else
    {
        write.csv(unique(df),file,row.names=F,...)
    }
    
    
}

rcsv <- function(file){
    df <- read.csv(file,strip.white=T,stringsAsFactors=F)
}

####################################################
# Wrapper for Matching and returning matched data
#####################################################
get_matched_subset <- function(df,...){
    
}

#### PCT match rate
chk_match_pct <- function(m.df,what,nseq){
    
    pct <- sum(!is.na(m.df[,px("match",what,nseq)]))/nrow(m.df)
    return(pct*100)
}



#### GENERIC function - DOES NOT WORK w DUPLICATES - USE MERGE
#### sdf source 
#### lkdf lookup source
#### scol - format (source) - scaler
#### lkcol - format (source, destination) - vector
#### sfmapname - name of mapped field in source df
map_field <- function(sdf,lkdf,scol,lkcol,sfmapname)
{
    lktable <- unique(lkdf[,lkcol])
    mv <- match(sdf[,scol],lkdf[,lkcol[1]]) 
    sdf[,sfmapname] <- lktable[mv,lkcol[2]]
    
    return(sdf)
}


##### READ CSVs in DataTables
##### unmatched = T returns a list (dt, umatched names)
combine.csvs.dt <- function(path,pattern,add_name=T,allchar=F,unmatched=F,rmformat=T,...){
    
    ### For each path - append files matching all patterns
    tflist <- c()
    for(cpath in path){
        for(cpattern in pattern){
            flist <- list.files(cpath,cpattern,ignore.case=T,recursive=T,include.dirs=F,no..=T,all.files=T,full.names=T)
            tflist<- c(tflist,flist)
        }
    }
    
    ### List of files from ALL paths matching ALL patterns
    ### read and append together
    fdt <- data.table()
    unmdt <- data.table()
    for(cfile in tflist){
        
        ###cfile <- tflist[1]
        test <- fread(cfile,...)
        if(allchar){
            colclass <- rep("character",ncol(test))
            test<-fread(cfile,colClasses=colclass,...)
        }
        
        if(add_name){
            test[,"fname"] = cfile
        }
        
        a<-names(test)
        b<-names(fdt)
        add_b <- setdiff(a,b)
        add_a <- setdiff(b,a)
        
        # cfile + names in test(a)
        # rowbind to unmdt
        # RETURN UNMATCHED NAMES ALSO
        if(unmatched & nrow(fdt)>0 & length(add_b)>0){
            unmtest <- data.table(cfile,add_b)
            unmdt <- rbind(unmdt,unmtest)
        }
        
        ### add to B
#         if(nrow(fdt)>0 & length(add_b)>0){
#             fdt[,add_b] = NA
#         }
#         
#         ### add to A
#         if(nrow(test)>0 & length(add_a)>0){
#             test[,add_a] = NA
#         }
        
        fdt <- rbind(test,fdt,use.names=T,fill=T)
    }
    
    ### MAKE VALID NAMES
    namesv <- names(fdt)
    ### Valid Names in lowercase - remove duplicate "."s
    ###setnames(fdt,namesv,gsub("\\.+",".",tolower(make.names(namesv,unique=T,allow=F))))
    names(fdt)<-gsub("\\.+",".",tolower(make.names(namesv,unique=T,allow=F)))
    
    #### REMOVE ,s added by excel formatting - so text to numeric can be done
    if(rmformat==T){
        srcv <- names(fdt)
        fdt[,(srcv):=lapply(.SD,function(v){gsub(",+","",v)})]
    }

    if(unmatched){
        list(fdt,unmdt)
    }
    else{
    return(fdt)
    }
}



