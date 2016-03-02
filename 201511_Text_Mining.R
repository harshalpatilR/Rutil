source("201511_Harshal_R_Functions.R")

x<-rcsv("combined_angel_cb_V6.csv")
x[is.na(x)] <- ""
names(x)

srcvec<-mapply(paste,x$Company.Desc1,x$Company.Desc2,x$Company.Focus.Areas,x$sub.industry)
m1<-cluster_docs(as.character(srcvec))

x$srcvec <- srcvec
x$cluster <- m1[[1]]$cluster
wcsv(x,"clustered.csv")

#------------------------------------
# recluster clusters 13 and 16
x1 <- x[x$cluster %in% c(13),]
m2 <- cluster_docs(as.character(x1$srcvec))

x1$cluster1 <- m2[[1]]$cluster

#-----------------------------------
# recluster clusters 13 and 16
x2 <- x[x$cluster %in% c(16),]
m3 <- cluster_docs(as.character(x2$srcvec))

x2$cluster1 <- m3[[1]]$cluster

wcsv(x1,"iterate_cluster1.csv")
wcsv(x2,"iterate_cluster2.csv")

###-------------------------------------------------
# KNN
###-------------------------------------------------
require(class)
c1 <- rcsv("for_knn_recluster_clustered_V1.csv")

skills <- Corpus(VectorSource(c1$srcvec))

## Convert to Lower Case
skills <- tm_map(skills, content_transformer(tolower))
##skills[[1]]

## Remove Stopwords
### NOT ON SKILLS
skills <- tm_map(skills, removeWords, stopwords("english"))
##skills[[1]]

## Remove Punctuations
### NOT ON SKILLS
skills <- tm_map(skills, content_transformer(removePunctuation))
##skills[[1]]

## Remove Numbers
skills <- tm_map(skills, content_transformer(removeNumbers))
##skills[[1]]

## Eliminating Extra White Spaces
skills <- tm_map(skills, content_transformer(stripWhitespace))
##skills[[1]]

## create a term document matrix
skills <- Corpus(VectorSource(skills))
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

c1$id <- as.numeric(rownames(c1))

test.id <- c1[trim(c1$final_cluster_3) == "recluster","id"]
train.id <- c1[!trim(c1$final_cluster_3) == "recluster","id"]

result <- knn(as.matrix(dtm)[train.id,],
            as.matrix(dtm)[test.id,],
                      c1[train.id,"final_cluster_3"],
                      k=49)
c1[test.id,"knn"] <- result
wcsv(c1,"knn_op.csv")

