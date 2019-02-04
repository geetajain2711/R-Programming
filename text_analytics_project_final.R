#### TEAM 1 #####

# Set working directory --> Remember to set the working directory
# make sure it is the directory / folder where you keep the data
setwd("/Users/juanl/OneDrive - Education First/Text Analytics/data/")

#Call the textreadr library to read the txt / doc file containing the data
library(textreadr)

# Read the data file and transfer it to an object
data <- read_document(file="data_final.txt")

# Define the structure of the data frame --> How many columns or variable and how many observations
observations <- 47 #how many observations to you have
variables<- 9 #how many variables do you have

# Create and empty data frame with the defined structure
my_df <- as.data.frame(matrix(nrow=observations, ncol=variables))

# Run a loop to write observations into the data frame
for(z in 1:variables){
  for(i in 1:observations){
    my_df[i,z]<- data[i*variables+z-variables]
  }#closing z loop
}#closing i loop

# Set the names of the variables
variable_names <- c("Age",
                    "Gender",
                    "Years of experience",
                    "Skills prior program",
                    "Skills acquired during program",
                    "Internship",
                    "Internship responsibilities",
                    "Job",
                    "Job responsibilities")
colnames(my_df) <- variable_names

#Call the tidytext library to tokenize text from the variable "skills prior program"
library(tidytext)
library(dplyr)

#Call stop words
data("stop_words")

# subset variable "skills prior program"
skills_prior_program <- my_df[c("Gender","Skills prior program")]

# need to rename the column to text 
colnames(skills_prior_program) <- c("Gender", "text")

summary_skills_prior_program <- skills_prior_program %>% unnest_tokens(output = word, input = text, token = "words") %>% 
                                anti_join(stop_words) %>% 
                                filter(word != "skills") %>% #filter word skills as there is no insigths
                                count(word, sort = TRUE)

# call the library ggplot
library(ggplot2)

# plot frequency of words for skilss prior program
summary_skills_prior_program %>%
              filter(n > 1) %>%
              mutate(word = reorder(word,n)) %>%
              ggplot(aes(word,n)) +
              geom_col(colour = "blue", fill = "blue") +
              xlab(NULL) + 
              coord_flip()

# subset variable "skills acquired during the program"
skills_during_program <- my_df["Skills acquired during program"]

# need to rename the column to text 
colnames(skills_during_program) <- c("text")

summary_skills_during_program <- skills_during_program %>% unnest_tokens(output = word, input = text, token = "words") %>% 
  anti_join(stop_words) %>% 
  filter(word != "skills") %>% #filter word skills as there is no insigths
  count(word, sort = TRUE)

# plot frequency of words for skils acquired during program
summary_skills_during_program %>%
  filter(n > 1) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col(colour = "red", fill = "red") +
  xlab(NULL) + 
  coord_flip()

# subset variable "Internship responsibilities"
intership_res <- my_df["Internship responsibilities"]

# need to rename the column to text 
colnames(intership_res) <- c("text")

summary_intership_res<- intership_res %>% unnest_tokens(output = word, input = text, token = "words") %>% 
  anti_join(stop_words) %>% 
  filter(word != "skills") %>% #filter word skills as there is no insigths
  count(word, sort = TRUE)

# plot frequency of words for internship responsibilities
summary_intership_res %>%
  filter(n > 1) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col(colour = "red", fill = "red") +
  xlab(NULL) + 
  coord_flip()

# subset variable "job responsibilities"
job_res <- my_df["Job responsibilities"]

# need to rename the column to text 
colnames(job_res) <- c("text")

summary_job_res<- job_res %>% unnest_tokens(output = word, input = text, token = "words") %>% 
  anti_join(stop_words) %>% 
  filter(word != "skills") %>% #filter word skills as there is no insigths
  count(word, sort = TRUE)

# plot frequency of words for job responsibilities
summary_job_res %>%
  filter(n > 1) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n)) +
  geom_col(colour = "blue", fill = "blue") +
  xlab(NULL) + 
  coord_flip()

library(tm)
library(dplyr)
library(tidytext)


library(wordcloud)
library(reshape2)
#we need to use the NRC sentiments
summary_skills_prior_program %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="nn", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)


#we need to use the NRC sentiments
summary_skills_during_program %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="nn", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)

# Subsetting data using job yes
library(dplyr)
Job_yes <- filter(my_df, Job == 'yes')
write.table(Job_yes, 'jobyes.txt', sep='\t')
# Wordcloud with People with job yes
library(tm)
library(wordcloud)
library(RColorBrewer)
data_jobyes <- read_document(file='jobyes.txt')
data_jobno <- read_document(file='jobno.txt')
data1<-Corpus(VectorSource(data_jobyes))
data_jobyes<-tm_map(data1,stripWhitespace)
data_jobyes<-tm_map(data_jobyes,tolower)
data_jobyes<-tm_map(data_jobyes,removeNumbers)
data_jobyes<-tm_map(data_jobyes,removePunctuation)
data_jobyes<-tm_map(data_jobyes,removeWords, stopwords('english'))
data_jobyes<-tm_map(data_jobyes, removeWords, c('and','the','our','that','for','are','also','more','has','must','have','should','this','with'))
tdm_yes<-TermDocumentMatrix(data_jobyes) #Creates a TDM
TDMP<-as.matrix(tdm_yes) #Convert this into a matrix format
v = sort(rowSums(TDMP), decreasing = TRUE) #Gives you the frequencies for every word
jobyes <- wordcloud(data_jobyes, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

# file with no job
Job_no <- filter(my_df, Job == 'no')
write.table(Job_no, 'jobno.txt', sep='\t')
# Wordcloud with People with job no
library(tm)
library(wordcloud)
library(RColorBrewer)

data_jobno <- read_document(file='jobno.txt')
data2<-Corpus(VectorSource(data_jobno))
data_jobno<-tm_map(data2,stripWhitespace)
data_jobno<-tm_map(data_jobno,tolower)
data_jobno<-tm_map(data_jobno,removeNumbers)
data_jobno<-tm_map(data_jobno,removePunctuation)
data_jobno<-tm_map(data_jobno,removeWords, stopwords('english'))
data_jobno<-tm_map(data_jobno, removeWords, c('and','the','our','that','for','are','also','more','has','must','have','should','this','with'))
tdm_no<-TermDocumentMatrix(data_jobno) #Creates a TDM
TDMN<-as.matrix(tdm_no) #Convert this into a matrix format
v = sort(rowSums(TDMN), decreasing = TRUE) #Gives you the frequencies for every word
jobno <- wordcloud(data_jobno, scale=c(5,0.5), random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))


