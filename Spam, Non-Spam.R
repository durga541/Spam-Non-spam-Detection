getwd()
##spam.df = read.csv("spambase.data.csv", header = FALSE)
spam.df = read.csv(url('https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data'), header=FALSE)

norm.df = data.frame(sapply(spam.df[,-58], scale))
spam_norm.df = cbind(norm.df, spam.df[,58])

names(spam_norm.df) = c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d",
                   "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet",
                   "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will",
                   "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free",
                   "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit",
                   "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money",
                   "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", 
                   "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857",
                   "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology",
                   "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct",
                   "word_freq_cs", "word_freq_meeting", "word_freq_original","word_freq_project",
                   "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference",
                   "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$",
                   "char_freq_#", "capital_run_length_average", "capital_run_length_longest",
                   "capital_run_length_total", "status")


head(spam_norm.df)

table(spam.df$status)

##d.df = data.frame(a=mean(spam.df$word_freq_make[spam.df$status==0]), b=mean(spam.df$word_freq_make[spam.df$status==1]))


compare.df = data.frame(abs(colMeans(spam_norm.df[which(spam_norm.df$status==0),])), 
                  abs(colMeans(spam_norm.df[which(spam_norm.df$status==1),])))
names(compare.df) = c("Non-spam", "spam")
options(scipen = 999)
View(compare.df)

diff = sort(abs(colMeans(spam_norm.df[which(spam_norm.df$status==0),])-
       colMeans(spam_norm.df[which(spam_norm.df$status==1),])), decreasing = TRUE)

View(diff)
predictors = head(diff,11)

############################
library(caret)
library(MASS)
spam_10.df = spam_norm.df[c(5,7,16,17,19,21,23,25,53,57,58)]
names(spam_10.df)

set.seed(123)
train.index = createDataPartition(spam_10.df$status, p=0.8, list = FALSE)
train.df = spam_10.df[train.index,]
valid.df = spam_10.df[-train.index,]

lda = lda(status~., data= train.df)

lda

pred = predict(lda, valid.df)


table(pred$class, valid.df$status)
mean(pred$class == valid.df$status)

confusionMatrix(factor(pred$class),factor(valid.df$status))

library(gains)

gain = gains(spam_10.df[valid.df,]$status[!is.na(pred)], pred[!is.na(pred)])

lift = lift(relevel(as.factor(valid.df$status), ref = "0")~pred, data = pred)

pb = as.data.frame(pred$posterior)
pred.lda = data.frame(valid.df$status, pb$`1`)
colnames(pred.lda) = c("target", "prob")

factor(lift_lda) = lift(target~prob, data=pred.lda, cuts=10,class="1")

