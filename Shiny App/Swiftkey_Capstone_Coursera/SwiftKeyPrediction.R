
prediction<-function(prefix_raw,prof_data,en_US_Sample_Unigram_count,en_US_Sample_Bigram_count,en_US_Sample_Trigram_count)
{
#Get the prefix as input
#prefix_raw<-input
#num<-as.numeric(readline(prompt = "Enter number of prefix words(1-4): "))
#num<-2
num_pred<-5
alpha_min<-1-num_pred/(num_pred+1)
prefix_raw<-gsub("[0-9[:punct:]]","",prefix_raw)#Remove numbers and punctiaons
prefix_raw<-gsub(paste(prof_data$V1,collapse="|"),"",prefix_raw)#remove profanity words
prefix_raw<-gsub("[^a-zA-Z0-9[:punct:][:space:]]","",prefix_raw)#Remove foreign words
prefix_raw<-gsub("[[:space:]]{2,}"," ",prefix_raw)#Remove extra whitespaces
prefix_raw<-tolower(prefix_raw)

temp_prefix<-strsplit(prefix_raw," ")
alpha<-1
l<-length(temp_prefix[[1]])
final_words<-data.frame(lastwords=character(),prob=numeric())

#Trigram
prefix1<-paste(temp_prefix[[1]][(l-1):l],collapse = " ")
temp1<-en_US_Sample_Trigram_count[en_US_Sample_Trigram_count$firstwords==prefix1,]
temp1$prob<-alpha*temp1$disc_n/sum(temp1$n)
alpha<-alpha-sum(temp1$prob)
#return(head(en_US_Sample_Trigram_count))
final_words<-rbind(final_words,temp1[1:num_pred,c("lastwords","prob")])

#Bigram
prefix2<-temp_prefix[[1]][l]
temp2<-en_US_Sample_Bigram_count[en_US_Sample_Bigram_count$firstwords==prefix2,]
temp2<-temp2[!(temp2$lastwords %in% temp1$lastwords),]
temp2$prob<-alpha*temp2$disc_n/sum(temp2$n)
alpha<-alpha-sum(temp2$prob)
final_words<-rbind(final_words,temp2[1:num_pred,c("lastwords","prob")])

#Unigram
temp3<-en_US_Sample_Unigram_count
temp3<-temp3[!(temp3$lastwords %in% temp2$lastwords)&!(temp3$lastwords %in% temp1$lastwords),]
temp3$prob<-alpha*temp3$disc_n/sum(temp3$n)
final_words<-rbind(final_words,temp3[1:num_pred,c("lastwords","prob")])
#final_words<-temp3

final_words<-final_words[order(-final_words$prob),][1:5,]
return(final_words$lastwords)

# Old 1

# for (i in num:1){
#         if((alpha>=alpha_min) | (nrow(final_words)<5)){
#                 prefix1<-paste(temp_prefix[[1]][(l-i+1):l],collapse = " ")
#                 prefix2<-paste(temp_prefix[[1]][(l-i):l],collapse = " ")
#                 temp1<-en_US_Sample_count[en_US_Sample_count$ngram==(i+1) & en_US_Sample_count$firstwords==prefix1,]
#                 temp2<-en_US_Sample_count[en_US_Sample_count$ngram==(i+2) & en_US_Sample_count$firstwords==prefix2,]
#                 temp1<-temp1[!(temp1$lastwords %in% temp2$lastwords),]
#                 temp1$prob<-alpha*temp1$disc_n/sum(temp1$n)
#                 alpha<-alpha-sum(temp1$prob)
#                 final_words<-rbind(final_words,temp1[1:num_pred,c("lastwords","prob")])
#                 final_words<-final_words[complete.cases(final_words),]
#         }
# }


#final_words<-final_words[order(-final_words$prob),][1:5,]
#print(final_words[complete.cases(final_words),])

# Old 2
# for (i in 1:num){
#         if(i==1){
#                 assign(paste0("prefix_",i),temp[[1]][l-i+1])
#         }
#         else{
#                 assign(paste0("prefix_",i),paste(temp[[1]][l-i+1],eval(parse(text=(paste0("prefix_",i-1))))))
#         }
# }
# rm(prefix_raw)
# rm(temp)
# rm(l)
#
# #Calculate the left over prob
# for (i in 1:num){
#         temp<-eval(parse(text=paste0("en_US_Sample_",ngram_identifier[i+1],"_count[en_US_Sample_",ngram_identifier[i+1],"_count$firstwords==prefix_",i,",]")))
#         assign(paste0("alpha_",i+1),1- sum(temp$disc_n)/sum(temp$n))
# }
#
# #Calculate the required numbers
# obs_word_five<-en_US_Sample_Fivegram_count[en_US_Sample_Fivegram_count$firstwords==prefix_five,c("lastwords","n","disc_n")]
# unobs_word_five<-en_US_Sample_Fivegram_count[!(en_US_Sample_Fivegram_count$firstwords==prefix_five),c("lastwords","n","disc_n")]
# obs_word_four<-en_US_Sample_Fourgram_count[en_US_Sample_Fourgram_count$firstwords==prefix_four,c("lastwords","n","disc_n")]
# obs_word_four_unobs_five<-unobs_word_five[unobs_word_five$lastwords %in% obs_word_four$lastwords,c("lastwords","n","disc_n")]
# unobs_word_four<-en_US_Sample_Fourgram_count[!(en_US_Sample_Fourgram_count$firstwords==prefix_four),c("lastwords","n","disc_n")]
# obs_word_tri<-en_US_Sample_Trigram_count[en_US_Sample_Trigram_count$firstwords==prefix_tri,c("lastwords","n","disc_n")]
# obs_word_tri_unobs_four<-unobs_word_four[unobs_word_four$lastwords %in% obs_word_tri$lastwords,c("lastwords","n","disc_n")]
# unobs_word_tri<-en_US_Sample_Trigram_count[!(en_US_Sample_Trigram_count$firstwords==prefix_tri),c("lastwords","n","disc_n")]
# obs_word_bi<-en_US_Sample_Bigram_count[en_US_Sample_Bigram_count$firstwords==prefix_bi,c("lastwords","n","disc_n")]
# obs_word_bi_unobs_tri<-unobs_word_tri[unobs_word_tri$lastwords %in% obs_word_bi$lastwords,c("lastwords","n","disc_n")]
# unobs_word_bi<-en_US_Sample_Bigram_count[!(en_US_Sample_Bigram_count$firstwords==prefix_bi),c("lastwords","n","disc_n")]
#
# obs_word_five_sum<-sum(obs_word_five$n)
# obs_word_four_sum<-sum(obs_word_four$n)
# obs_word_tri_sum<-sum(obs_word_tri$n)
# obs_word_bi_sum<-sum(obs_word_bi$n)
# obs_word_four_unobs_five_sum<-sum(obs_word_four_unobs_five$n)
# obs_word_tri_unobs_four_sum<-sum(obs_word_tri_unobs_four$n)
# obs_word_bi_unobs_tri_sum<-sum(obs_word_bi_unobs_tri$n)
# unobs_word_bi_sum<-sum(unobs_word_bi$n)
#
#
# #Observed Five
# prob_five<-sum(obs_word_five$disc_n)/obs_word_five_sum
#
# #Unobserved Five
# #Observed Four
# prob_four<-(alpha_five/denom_four)*(sum(obs_word_four_unobs_five$disc_n)/obs_word_four_unobs_five_sum)
#
# #Unobserved Four
# #Observed Tri
# prob_tri<-(alpha_five/denom_four)*(alpha_four/denom_tri)*(sum(obs_word_tri_unobs_four$disc_n)/obs_word_tri_unobs_four_sum)
#
# #Unobserved Tri
# #Observed_bi
# prob_bi<-(alpha_five/denom_four)*(alpha_four/denom_tri)*(alpha_tri/denom_bi)*(sum(obs_word_bi_unobs_tri$disc_n)/obs_word_bi_unobs_tri_sum)
#
# #Unobserved bi
# prob_uni<-(alpha_five/denom_four)*(alpha_four/denom_tri)*(alpha_tri/denom_bi)*(alpha_bi/denom_uni)*(sum(unobs_word_bi$disc_n)/unobs_word_bi_sum)
#
#
#
#
#
#
#
#
#
#
#
# # #Calculate the probabilities
# # #Observed Trigrams
# # prob_obs_tri<-obs_word_tri[order(-obs_word_tri$disc_n),][1:5,]
# # prob_obs_tri$prob<-prob_obs_tri$disc_n/obs_word_tri_sum
# # prob_obs_tri$class<-"obs_tri"
# #
# # #Unobserved Trigram
# # #Observed Bigram
# # prob_obs_bi_unobs_tri<-obs_word_bi_unobs_word_tri[order(-obs_word_bi_unobs_word_tri$disc_n),][1:5,]
# # prob_obs_bi_unobs_tri$prob<-(alpha_tri/bi_denom)*(prob_obs_bi_unobs_tri$disc_n/obs_word_bi_sum)
# # prob_obs_bi_unobs_tri$class<-"obs_bi_unobs_tri"
# #
# # #Unobserved Bigram
# # prob_unobs_bi_unobs_tri<-unobs_word_bi_unobs_word_tri[order(-unobs_word_bi_unobs_word_tri$n),][1:5,]
# # prob_unobs_bi_unobs_tri$prob<-(alpha_tri/bi_denom)*(alpha_bi*prob_unobs_bi_unobs_tri$n/unobs_word_bi_unobs_word_tri_sum)
# # prob_unobs_bi_unobs_tri$class<-"unobs_bi_unobs_tri"
# #
# # #Final Probability
# # final_data<-rbind(prob_obs_tri,prob_obs_bi_unobs_tri,prob_unobs_bi_unobs_tri)
# # final_words<-final_data[order(-final_data$prob),]$lastwords[1:5]
# # final_words
}