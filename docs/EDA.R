## EDA file

# read in spam data
spam = read.csv("spambase_csv.csv") ; originalData = spam
spam = unique(spam)

n = nrow(spam)
p = ncol(spam)

# subset X and Y from the dataframe
Y = spam[,p]
XQual = spam[,1:(p-4)]
XQuan = spam[,(p-3):(p-1)]

for(i in 1:(p-4)){
  for(j in 1:n){
    if(XQual[j,i] > 0){
      XQual[j,i] = 1
    }
  }
}

temp = data.frame(Type = c("Spam", "Not Spam"), Count = c(sum(Y == 1),sum(Y == 0)))

ggplot(temp, aes(y = Count, x = Type, fill = factor(Type))) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual("legend", values=c("Spam" = 'maroon', "Not Spam" = "dodgerblue3")) + 
  theme(legend.position = "none") + 
  geom_text(aes(label = Count),size = 15, stat = "identity", vjust = 1.5, colour = "white") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15))

rm(temp)


#aaa = XQual%>% sapply(sum,2) %>% .[order(.,decreasing = T)]
#bbb = colnames(XQual[order(XQual%>% sapply(sum,2),decreasing = T)])
#bbb = gsub("word_freq", "WF", bbb)
#bbb = gsub("char_freq", "CF", bbb)
#temp = data.frame(variable = bbb, value = aaa, row.names = NULL)

#ggplot(data = temp, aes(y = reorder(variable, value), x = value)) + geom_bar(stat = 'identity')



## Separate for spam
spammails = filter(cbind(XQual,spam = Y), spam == 1)
aaa = spammails[-ncol(spammails)]%>% sapply(sum,2) %>% .[order(.,decreasing = T)]
bbb = colnames(spammails[order(spammails[-ncol(spammails)]%>% sapply(sum,2),decreasing = T)])
bbb = gsub("word_freq_", "", bbb)
bbb = gsub("char_freq_", "", bbb)
temp = data.frame(variable = bbb, value = aaa, row.names = NULL)

# Plot 1
ggplot(data = temp, aes(y = reorder(variable, value), x = value)) + 
  geom_bar(stat = 'identity', fill = "maroon") + 
  labs(x = "Frequency", y = 'Word') + 
  ggtitle("Frequent words in Spam Emails") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15))



## Separate for non-spam
nonspammails = filter(cbind(XQual,spam = Y), spam == 0)
aaa = nonspammails[-ncol(nonspammails)]%>% sapply(sum,2) %>% .[order(.,decreasing = T)]
bbb = colnames(nonspammails[order(nonspammails[-ncol(spammails)]%>% sapply(sum,2),decreasing = T)])
bbb = gsub("word_freq_", "", bbb)
bbb = gsub("char_freq_", "", bbb)
temp = data.frame(variable = bbb, value = aaa, row.names = NULL)


# Plot 2
ggplot(data = temp, aes(y = reorder(variable, value), x = value)) + 
  geom_bar(stat = 'identity', fill = "dodgerblue3") + 
  labs(x = "Frequency", y = 'Word') +
  ggtitle("Frequent words in Non-Spam Emails") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15))


