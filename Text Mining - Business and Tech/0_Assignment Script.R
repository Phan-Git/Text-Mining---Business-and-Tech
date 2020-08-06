# packages
library(textreadr)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(tidyr)


######################################################
########Step1: building a  text object################
######################################################
setwd("~/Desktop/Data Learning/Text Mining with R/Assignment")
nm <- list.files(path="~/Desktop/Data Learning/Text Mining with R/Assignment")
nm

#using read document to import the data:
talk_df_1 <- read_document(file=nm[2]) 
talk_df_2 <- read_document(file=nm[3]) 
talk_df_3 <- read_document(file=nm[4]) 
talk_df_4 <- read_document(file=nm[5]) 

talk_data_together <- paste(c(talk_df_1, talk_df_2, talk_df_3, talk_df_4), collapse = " ")

my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

######################################################
###STEP2: Putting the vector in a data frame##########
######################################################

my_talk_df <- data_frame(line=1, text=talk_data_together)
print(my_talk_df)

######################################################
######## Step3: tokenizing the mydf dataframe#########
######################################################

token_list <- my_talk_df %>%
  unnest_tokens(word, text)
#no punctutation, no upper case letters
print(token_list)

#######################################################
##########STEP4: token frequencies####################
#######################################################

frequencies_tokens <- my_talk_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(frequencies_tokens)

#######################################################
#########STEP5:### stop words #########################
#######################################################


data(stop_words)
frequencies_tokens_nostop <- my_talk_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)


print(frequencies_tokens_nostop)

#######################################################
#####STEP6:  token frequency histograms################
#######################################################

freq_hist <- my_talk_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 25) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#######################################################
#####STEP7:  ANALYZE THE JOBS IN SENTIMENTS ###########
#######################################################

my_junk = data_frame(
   word = c('job'),
   lexicon = rep('junk', each = 1))

talk_df_5 <- read_document(file=nm[6]) 
talk_df_5 = data_frame(line=1, text=talk_df_5)

freq_hist_2 <- talk_df_5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=TRUE) %>%
  filter(n > 3) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(freq_hist_2)

talk_df_5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

talk_df_5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)
  
#######################################################
####STEP8: PUT IT ALL IN BUSINESS STRATEGY & TECH #####
#######################################################
talk_df_6 <- read_document(file=nm[7]) 
talk_df_6 = data_frame(line=1, text=talk_df_6)


talk_df_7 <- read_document(file=nm[8]) 
talk_df_7 = data_frame(line=1, text=talk_df_7)



#################################################
### Combining all 3 tidy data frames ############
#################################################

#this is where you tokenize all 3 twitter datasets
tidy_DI <- my_talk_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk)

tidy_Business <- talk_df_6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk)

tidy_Tech <- talk_df_7 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk)


frequency <- bind_rows(mutate(tidy_DI, author="DISRUPTIVE INNOVATION"),
                       mutate(tidy_Business, author= "BUSINESS STRATEGY"),
                       mutate(tidy_Tech, author="FUTURE TECHNOLOGY")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `BUSINESS STRATEGY`, `FUTURE TECHNOLOGY`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`DISRUPTIVE INNOVATION`, 
                      color = abs(`DISRUPTIVE INNOVATION`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "DISRUPTIVE INNOVATION", x=NULL)


cor.test(data=frequency[frequency$author == "BUSINESS STRATEGY",],
         ~proportion + `DISRUPTIVE INNOVATION`)

cor.test(data=frequency[frequency$author == "FUTURE TECHNOLOGY",],
         ~proportion + `DISRUPTIVE INNOVATION`)

