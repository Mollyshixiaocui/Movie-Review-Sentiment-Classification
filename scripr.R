library(tm)
library(SnowballC)
library(e1071)
library(caret)
library(ggplot2)
library(ROCR)
library(qdap)
library(doParallel)
library(stringr)
library(beepr)

accuracy = function(cm){  # input confusin matrix
  return(sum(diag(cm))/sum(cm))  # accuracy
}

#------------------------------------prepare----------------------------------------
##read txt file into different data frame
pos<-read.table("Tweets_pos.txt",header=FALSE,stringsAsFactors = FALSE,sep = "\n",comment.char = "")
neg<-read.table("Tweets_neg.txt",header=FALSE,stringsAsFactors = FALSE,sep = "\n",comment.char = "")
eval<-read.table("Tweets_eval.txt",header=FALSE,stringsAsFactors = FALSE,sep = "\n",comment.char = "")

neg$label = "neg"
pos$label = "pos" 

# trainwithoutlabel = rbind(neg,pos)
# train$label = as.factor(train$label)
# rm(neg)
# rm(pos)
# rm(eval)
# save(train, file = "train.Rdata")
load("train.Rdata")
traincorpus = VCorpus(DataframeSource(trainwithoutlabel))
traincorpus = tm_map(traincorpus, content_transformer(stripWhitespace))
traincorpus = tm_map(traincorpus, stemDocument)
traincorpus = tm_map(traincorpus, removeWords, 
                     c("holm","avatar","shutter","one","amp","movi","sherlock",
                     "island","time","watch","hangov","see","saw","just",
                     "year","district","perci","jackson","john","dear","land",
                     "lost","propos","mom","think","realli","seen","final",
                     "look","night","will","last","ever","joe","new","now",
                     "today","joelauzon","jooo","kevinceron","morgen","otm",
                     "blind","legion","film","rome","thing","destin","eli","said",
                     "thief","lightn","enemi","wolfman","went","thought","law",
                     "side","public","day","two","wild","abid","olympian","finish",
                     "citizen","way","get","made","tonight","transform","frog","start",
                     "princess","got","world","quotth","keeper","sister","rock","theater",
                     "bandslam","fallen","dark","knight","end","mairimba","guy"))

#-----bigram tdm------------
detach("package:qdap", unload = TRUE)

BigramTokenizer =  function(x) 
  unlist(lapply(ngrams(content(x), 2)[[7]][[2]], paste, sep =" ", collapse = " "), use.names = FALSE)


tdm_bigram_freq = TermDocumentMatrix(traincorpus, control = list(tokenize = BigramTokenizer,
                                                                tolower = TRUE,
                                                                removePunctuation = TRUE,
                                                                removeNumbers = TRUE,
                                                                stopwords = TRUE))
Presence = WeightFunction(function(m) m>0, "presence", "pres")
tdm_bigram_pres = TermDocumentMatrix(traincorpus, control = list(tokenize = BigramTokenizer,
                                                                 tolower = TRUE,
                                                                 removePunctuation = TRUE,
                                                                 removeNumbers = TRUE,
                                                                 stopwords = TRUE,
                                                                 weighting = Presence))


#-----unigram tdm---------
#first remove numbers, stop words
traincorpus = tm_map(traincorpus, content_transformer(removeNumbers))
traincorpus = tm_map(traincorpus, removeWords,stopwords("english"))

#-----negation tag------------
#negation tag function
tag = function(x){ 
  temp = unlist(strsplit(x," "))
  temp1 = temp[1]
  temp = temp[0:-1]
  paste(temp1,paste(sapply(temp, function(x) paste0("not_",x)),collapse = " "),collapse = "")
}

library(qdap)
#define content transform function of negation tag applied on TDM
negationtag = content_transformer(function(x){
  #find the part need to add tag
  negation_part = unlist(str_extract_all(x,"n't.+?[:punct:]|not.+?[:punct:]|nt.+?[:punct:]"))
  #add tag
  negation_part = sapply(negation_part, tag)
  #relace the origimal text
  mgsub(names(negation_part), negation_part, x)
})

corpus_negation_tag = tm_map(traincorpus, negationtag)
# content(corpus_negation_tag[[1]])

#unigram TDM
tdm_unigram_tag = TermDocumentMatrix(corpus_negation_tag, control = list(tolower = TRUE,
                                                                         removePunctuation = TRUE))

tdm_unigram = TermDocumentMatrix(traincorpus, control = list(weighting = weightTfIdf,
                                                             tolower = TRUE,
                                                             removePunctuation = TRUE))
tdm_unigram_tag_pres = TermDocumentMatrix(corpus_negation_tag, control = list(tolower = TRUE,
                                                                              removePunctuation = TRUE,
                                                                              weighting = Presence))
#-----Model Building-----
#termfreq = function(tdm,freq) colSums(as.matrix(tdm))
registerDoParallel(cores=2) 
trc = trainControl(method = "repeatedcv", 
                   number = 10, 
                   repeats = 5, 
                   classProbs = TRUE,
                   allowParallel = T,
                   search = "random")


#-----SVM-----
#tdmList = list(tdm_unigram, tdm_unigram_tag, tdm_bigram_freq, tdm_bigram_pres)
tdmList = list(tdm_unigram_tag_pres)
#tdmNames = c("tdm_unigram", "tdm_unigram_tag", "tdm_bigram_freq", "tdm_bigram_pres")
tdmNames = c("tdm_unigram_tag_freq")
ntdm = length(tdmList)
sparsity.setting = c(0.998,0.995,0.99,0.98)
nss = length(sparsity.setting)
num.terms = matrix(nrow = ntdm, ncol = nss)
sparsity = matrix(nrow = ntdm, ncol = nss)
svm.models = list()
model.names = paste(rep(tdmNames,each = nss),as.character(sparsity.setting),sep = " ")
for (i in 1:ntdm){
  for (j in 1:nss){
    tdm.ss = removeSparseTerms(tdmList[[i]], sparsity.setting[j])
    num.terms[i,j] = tdm.ss$nrow  # number of terms
    sparsity[i,j] = 1 - length(tdm.ss$i)/(tdm.ss$ncol * tdm.ss$nrow) # sparsity
    # make the datasets, term as column and each review as row
    tdm.df = t(as.data.frame(as.matrix(tdm.ss)))
    # tune the model
    set.seed(1)
    svm.models[[j+2*(i-1)]] = train(y = train$label, x = tdm.df, 
                                   method = "svmRadial", 
                                   metric = 'kappa',
                                   tuneLength = 3,
                                   preProcess = "scale",
                                   trControl = trc)
  }
}
beep()
svm.results = resamples(svm.models, modelNames = model.names)
write.csv(svm.results$values,file = "results_svm.csv")
#table comparison
summary(svm.results)
#density plot to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(svm.results, scales=scales, pch = "|", auto.key = TRUE)
#Box and Whisker Plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(svm.results, scales=scales)

#-----random forest-----
trc.rf = trainControl(method = "repeatedcv", 
                   number = 5, 
                   repeats = 3, 
                   classProbs = TRUE,
                   allowParallel = T,
                   search = "random")

tdmList.rf = list(tdm_unigram_tag_pres, tdm_bigram_freq, tdm_bigram_pres)
tdmNames.rf.2 = c("tdm_unigram_tag_pres", "tdm_bigram_freq", "tdm_bigram_pres")
tdmNames.rf.1 = c("tdm_unigram","tdm_unigram_tag", "tdm_bigram_freq", "tdm_bigram_pres")
ntdm.rf = length(tdmList.rf)
sparsity.setting.rf = c(0.995,0.99)
nss.rf = length(sparsity.setting.rf)
num.terms.rf = matrix(nrow = ntdm.rf, ncol = nss.rf)
sparsity.rf = matrix(nrow = ntdm.rf, ncol = nss.rf)
#models.rf = list()
models.rf.2 = list()
model.names.rf = paste(rep(tdmNames.rf,each = nss.rf),as.character(sparsity.setting.rf),sep = " ")
for (i in 1:ntdm.rf){
  for (j in 1:nss.rf){
    tdm.ss.rf = removeSparseTerms(tdmList.rf[[i]], sparsity.setting.rf[j])
    num.terms.rf[i,j] = tdm.ss.rf$nrow  # number of terms
    sparsity.rf[i,j] = 1 - length(tdm.ss.rf$i)/(tdm.ss.rf$ncol * tdm.ss.rf$nrow) # sparsity
    # make the datasets, term as column and each review as row
    tdm.df.rf = t(as.data.frame(as.matrix(tdm.ss.rf)))
    # tune the model
    set.seed(1)
    models.rf.2[[j+2*(i-1)]] = train(y = train$label, x = tdm.df.rf, 
                                    method = "rf", 
                                    metric = 'kappa',
                                    tuneLength = 3,
                                    preProcess = "scale",
                                    trControl = trc.rf)
  }
}
beep()
results.rf.1 = resamples(models.rf, modelNames = paste(rep(tdmNames.rf.1,each = nss.rf),as.character(sparsity.setting.rf),sep = " "))
results.rf.2 = resamples(models.rf.2, modelNames = model.names.rf)
write.csv(results.rf.2$values,file = "results_rf_2.csv")
write.csv(results.rf.1$values,file = "results_rf_1.csv")

#table comparison
summary(results.rf)
#density plot to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results.rf, scales=scales, pch = "|", auto.key = TRUE)
#Box and Whisker Plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results.rf, scales=scales)