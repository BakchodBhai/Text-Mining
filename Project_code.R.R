library("tm")
library("tidytext")
library("dplyr")
library("wordcloud")

# 0 Importing the deata set
Reviews <- read.csv("C:/Users/sbtrader/Desktop/data mining-DG/amazon-fine-foods/Reviews.csv")

# 1 Exploratory data Analysis

# 1.1 # Unique Product IDs
length(unique(Reviews$ProductId))
# 4224 products

# 1.1.1 Distribution of products
Product_summary<- group_by(Reviews, ProductId)%>% summarise(length(ProductId))
plot(density(Product_summary$`length(ProductId)`))
mean(Product_summary$`length(ProductId)`)
# 8.326941
max(Product_summary$`length(ProductId)`)
# 632

# Ploting a histogram to see distribution of products
hist(Product_summary$`length(ProductId)`[Product_summary$`length(ProductId)`<60])
# Chart 1
# Insight - Most of the product IDs occur just few times, so we cannot process the ...
#... data case by case and hence we will have to take an alternate approach

# 1.2 Distribution by cust id
length(unique(Reviews$UserId))
#29430

# Helpfullness
#Helpfullness_Summary<- group_by(Reviews, HelpfulnessNumerator)%>% summarise(length(HelpfulnessNumerator))

# 1.3 Score Dist
Score_Summary<- group_by(Reviews, Score)%>% summarise(length(Score))
# most of the scores are 5

# Staring off with text data
# 1.4 Summary column

# 1.4.1 Text processing - Basic Cleaning

# I dont want to loose not as they completely change the meaning
f1<- function(x)
{
  x<- gsub("not good", "notgood", x, ignore.case = T)
  return(x)
}

Reviews<- mutate(Reviews, Summary= f1(Summary))

f2<- function(x)
{
  x<- gsub("not great", "notgreat", x, ignore.case = T)
  return(x)
}
Reviews<- mutate(Reviews, Summary= f2(Summary))

f3<- function(x)
{
  x<- gsub("dogs", "dog", x, ignore.case = T)
  return(x)
}
Reviews<- mutate(Reviews, Summary= f3(Summary))

# Adding a row identifier
Reviews$row_id<- row.names(Reviews)
reader<- readTabular(mapping = list(content = "Summary", id = "row_id"))
Summary_corp<- Corpus(DataframeSource(Reviews), readerControl = list(reader= reader))
# cleaning the damm text
Summary_corp<- tm_map(Summary_corp, content_transformer(tolower))
Summary_corp<- tm_map(Summary_corp, content_transformer(stripWhitespace))
Summary_corp<- tm_map(Summary_corp, removeNumbers)
Summary_corp<- tm_map(Summary_corp, removePunctuation)
Summary_corp<- tm_map(Summary_corp, removeWords, stopwords("en")[-167]) # not removing "not"
Summary_corp<- tm_map(Summary_corp, stemDocument, language = "english")
Summary_dtm<- DocumentTermMatrix(Summary_corp, control = list(wordLengths = c(3,20),
                                                              bounds = list(global = c(50,35173))))


# Looking at most frequent words and Word Cloud
# Crart 2
wordcloud(Summary_corp, min.freq = 100, max.words = 100, random.order = FALSE)

# Creating Document Term Matrix
Summary_dtm<- DocumentTermMatrix(Summary_corp, control = list(wordLengths = c(3,20),
                                                              bounds = list(global = c(50,35173))))
findFreqTerms(Summary_dtm, lowfreq = 300)

# Tagging based on Summary each product to a topic
row_tota<- apply(desc_dtm, 1, sum)
col_tota<- as.data.frame(apply(Summary_dtm, 2, sum))

col_tota$product<- rownames(col_tota)
col_tota<- arrange(col_tota, desc(`apply(Summary_dtm, 2, sum)`))
barplot(col_tota[1:15,1], names.arg=c(col_tota$product[1:15]))

# 1.4.2 Matching foods to product ids

# Below are the food items identified in the data
# dog, coffe, tea, chip, cat, snack, popcorn, cracker, soda, peanut, soda, 
# cereal, chai, cocoa, rice, bean , oatmeal, juic,pancak, pork, coconut,
# milk, cicken
sum(col_tota$`apply(Summary_dtm, 2, sum)`[col_tota$product %in% c("dog", "coffe", "tea", "chip", "cat", "snack", "popcorn", "cracker", "soda", "peanut", "soda", "cereal", "chai", "cocoa", "rice", "bean", "oatmeal", "juic", "pancak" ,"pork", "coconut", "milk", "chicken")])
#> 7794/35173*100
#[1] 22.15904

# Convert the dtm to data frame and select only food columns
Summary_df<- as.data.frame(as.matrix(Summary_dtm))
Summary_df<- select(Summary_df,dog, coffe, tea, chip, cat, snack, popcorn, cracker, soda, peanut, soda, cereal, chai, cocoa, rice, bean , oatmeal, juic, pancak, pork, coconut, milk, chicken  )
x<- apply(Summary_df, 1, sum)
hist(x)

# Merge this with the main data
Summary_df$id<- seq(1:35173)

# Calculate cosine similarity
library("lsa")
Summary_cos_dist<- cosine(as.matrix(Summary_df[,-23]))
Summary_cos_dist[Summary_cos_dist==1]<-0

# Obs
# if bean and coffee then cofee, chicken and cat then cat, peanut and dog then dog, cerial rice then rice

Reviews<- merge(Reviews, Summary_df, by.x = "row_id", by.y = "id")

# Pull product Ids and food columns separately
Products<- select(Reviews, ProductId, dog, coffe, tea, chip, cat, snack, popcorn, cracker, soda, peanut, soda, cereal, chai, cocoa, rice, bean , oatmeal, juic, pancak, pork, coconut, milk, chicken )
Products$food_type<- NA
# Function to pull colnames from
for (i in 1: 35173)
{
  x<-  colnames(Products[which(Products[i,]==1)]) 
  x<- x[1]
  Products$food_type[i]<-x
}

# Product Id and Food mapping

Products<- select(Products,ProductId, food_type)
Products<- Products[which(is.na(Products$food_type)==F),]
Products<- unique(Products)
length(unique(Products$ProductId))
Products$dup_flag<- duplicated(Products$ProductId)
Products<- arrange(Products, ProductId)
Products<- Products[Products$dup_flag == FALSE,-c(3)]
Products$flag<- "Summary"


Reviews<- select(Reviews, -dog, -coffe, -tea, -chip, -cat, -snack, -popcorn, -cracker, -soda, -peanut, -soda, -cereal, -chai, -cocoa, -rice, -bean , -oatmeal, -juic, -pancak, -pork, -coconut, -milk, -chicken)

############ Insert Text column code ####################

# Adding a row identifier
Reviews$row_id<- row.names(Reviews)
reader<- readTabular(mapping = list(content = "Text", id = "row_id"))
Text_corp<- Corpus(DataframeSource(Reviews), readerControl = list(reader= reader))
writeLines(as.character(Text_corp[1:2]))


# cleaning the damm text
Text_corp<- tm_map(Text_corp, content_transformer(tolower))
Text_corp<- tm_map(Text_corp, content_transformer(stripWhitespace))
Text_corp<- tm_map(Text_corp, removeNumbers)
Text_corp<- tm_map(Text_corp, removePunctuation)
Text_corp<- tm_map(Text_corp, removeWords, stopwords("en")[-167]) # not removing "not"

writeLines(as.character(Text_corp[1:2]))

Text_corp<- tm_map(Text_corp, stemDocument, language = "english")
# Looking at most frequent words and Word Cloud
# Chart 2
wordcloud(Text_corp, min.freq = 100, max.words = 100, random.order = FALSE)

# Creating Document Term Matrix
Text_dtm<- DocumentTermMatrix(Text_corp, control = list(wordLengths = c(3,20),
                                                        bounds = list(global = c(50,35173))))
findFreqTerms(Text_dtm, lowfreq = 300)

# Tagging based on Text each product to a topic
row_tota<- apply(desc_dtm, 1, sum)
col_tota<- as.data.frame(apply(Text_dtm, 2, sum))

col_tota$product<- rownames(col_tota)
col_tota<- arrange(col_tota, desc(`apply(Text_dtm, 2, sum)`))
barplot(col_tota[1:15,1], names.arg=c(col_tota$product[1:15]))

# 1.4.2 Matching foods to product ids

# Below are the food items identified in the data
# dog, coffe, tea, chip, cat, snack, popcorn, cracker, soda, peanut, soda, cereal, rice, bean , oatmeal, juic, pork, coconut, milk, chicken, chocol, popchip, bread, corn, nut
sum(col_tota$`apply(Text_dtm, 2, sum)`[col_tota$product %in% c("dog", "coffe", "tea", "chip", "cat", "snack", "popcorn", "cracker", "soda", "peanut", "soda", "cereal", "rice", "bean" , "oatmeal", "juic", "pork", "coconut", "milk", "chicken", "chocol", "popchip", "bread", "corn", "nut")])
#> 7794/35173*100
#[1] 22.15904

# Convert the dtm to data frame and select only food columns
Text_df<- as.data.frame(as.matrix(Text_dtm))
Text_df<- select(Text_df,dog, coffe, tea, chip, cat, snack, popcorn, cracker, soda, peanut, soda, cereal, rice, bean , oatmeal, juic, pork, coconut, milk, chicken  )
x<- apply(Text_df, 1, sum)
hist(x)

# Merge this with the main data
Text_df$id<- seq(1:35173)

# Calculate cosine similarity
library("lsa")
Text_cos_dist<- cosine(as.matrix(Text_df[,-20]))
Text_cos_dist[Text_cos_dist==1]<-0
which(Text_cos_dist == max(Text_cos_dist))

# Obs
# Chicken and dog then dog, chip and snack, cracker can be combined, cereal and oatmeal, 
# Soda and Juic, remove rice, remove chicken

#Text_Products<- Text_df
p<- select(Reviews, Id,ProductId )
Text_Products<- merge(Text_df, p, by.x = "id", by.y = "Id")

Text_Products$food_type<- NA
# Function to pull colnames from
for (i in 1: 35173)
{
  x<-  colnames(Text_Products[which(Text_Products[i,]==1)]) 
  x<- x[1]
  Text_Products$food_type[i]<-x
}

# Product Id and Food mapping

Text_Products<- select(Text_Products,ProductId, food_type)
Text_Products<- Text_Products[which(is.na(Text_Products$food_type)==F),]
Text_Products<- unique(Text_Products)
length(unique(Text_Products$ProductId))
Text_Products$dup_flag<- duplicated(Text_Products$ProductId)
Text_Products<- arrange(Text_Products, ProductId)
Text_Products<- Text_Products[Text_Products$dup_flag == FALSE,-c(3)]
Text_Products$flag<- "Text"


# Create final mapping
Final_Products<- rbind(Products, Text_Products)
Final_Products<- arrange(Final_Products, ProductId, flag)
Final_Products$dup<- duplicated(Final_Products$ProductId)
Final_Products<- Final_Products[Final_Products$dup == FALSE,]
Final_Products<- Final_Products[,-c(3,4)]

Reviews<- merge(Reviews, Final_Products, by.x = "ProductId", by.y = "ProductId", all.x = T)
length(which(is.na(Reviews$food_type)==T)) # only 4918
# Count of topics
Topic_contri<- group_by(Reviews, food_type)%>% summarise(count= length(ProductId))%>% arrange(desc(count))
barplot(Topic_contri$count[-c(3,21,22,23)], names.arg=c(Topic_contri$food_type[-c(3,21,22,23)])) 

############ Topic Modelling for COFFEE################

# Seperate rows tagged as Coffee
Coffee<- as.data.frame(Reviews$Text[which(Reviews$food_type== "coffe")])
Coffee$id<- seq(1:5301)
names(Coffee)[1]<- "Text"
reader<- readTabular(mapping = list(content = "Text", id = "id"))
Coffee_corp<- Corpus(DataframeSource(Coffee), readerControl = list(reader= reader))


# cleaning the text
Coffee_corp<- tm_map(Coffee_corp, content_transformer(tolower))
Coffee_corp<- tm_map(Coffee_corp, content_transformer(stripWhitespace))
Coffee_corp<- tm_map(Coffee_corp, removeNumbers)
Coffee_corp<- tm_map(Coffee_corp, removePunctuation)
Coffee_corp<- tm_map(Coffee_corp, removeWords, stopwords("en")[-167]) # not removing "not"
Coffee_corp<- tm_map(Coffee_corp, removeWords, c("coffe"))

writeLines(as.character(Coffee_corp[1:2]))

Coffee_corp<- tm_map(Coffee_corp, stemDocument, language = "english")
# Looking at most frequent words and Word Cloud
# Crart 2
wordcloud(Coffee_corp, min.freq = 100, max.words = 100, random.order = FALSE)

# Creating Document Term Matrix
Coffee_dtm<- DocumentTermMatrix(Coffee_corp, control = list(wordLengths = c(3,20),
                                                            bounds = list(global = c(30,5301))))
findFreqTerms(Summary_dtm, lowfreq = 300)


# Topic Modelling
row_tota<- apply(Coffee_dtm, 1, sum)
col_tota<- data.frame(apply(Coffee_dtm, 2, sum))

Coffee_dtm2<- Coffee_dtm[row_tota>0,]
# Try LDA to get some topics
#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 30, by = 5), function(d){LDA(Coffee_dtm2, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
#calculating LDA
k = 5;#number of topics
# SEED = 786; # number of tweets used

# running LDA with 27 topics
Coffee_lda<- LDA(Coffee_dtm2, k = k)
Topics<- topics(Coffee_lda)
Terms<- as.matrix(terms(Coffee_lda,10))
Topic_probab<- as.data.frame(desc_lda@gamma)

####################
Coffee<- as.data.frame(Reviews$Text[which(Reviews$food_type== "dog")])
Coffee$id<- seq(1:5301)
names(Coffee)[1]<- "Text"
reader<- readTabular(mapping = list(content = "Text", id = "id"))
Coffee_corp<- Corpus(DataframeSource(Coffee), readerControl = list(reader= reader))


# cleaning the damm text
Coffee_corp<- tm_map(Coffee_corp, content_transformer(tolower))
Coffee_corp<- tm_map(Coffee_corp, content_transformer(stripWhitespace))
Coffee_corp<- tm_map(Coffee_corp, removeNumbers)
Coffee_corp<- tm_map(Coffee_corp, removePunctuation)
Coffee_corp<- tm_map(Coffee_corp, removeWords, stopwords("en")[-167]) # not removing "not"
Coffee_corp<- tm_map(Coffee_corp, removeWords, c("coffe","will", "cup", "get", "just", "use", "much"))

writeLines(as.character(Coffee_corp[1:2]))

Coffee_corp<- tm_map(Coffee_corp, stemDocument, language = "english")
# Looking at most frequent words and Word Cloud
# Crart 2
wordcloud(Coffee_corp, min.freq = 100, max.words = 100, random.order = FALSE)

# Creating Document Term Matrix
Coffee_dtm<- DocumentTermMatrix(Coffee_corp, control = list(wordLengths = c(3,20),
                                                            bounds = list(global = c(30,5301))))
findFreqTerms(Summary_dtm, lowfreq = 300)


# Topic Modelling
row_tota<- apply(Coffee_dtm, 1, sum)
col_tota<- data.frame(apply(Coffee_dtm, 2, sum))

Coffee_dtm2<- Coffee_dtm[row_tota>0,]
# Try LDA to get some topics
#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 30, by = 1), function(d){LDA(Coffee_dtm2, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
#calculating LDA
k = 3;#number of topics
# SEED = 786; # number of tweets used

# running LDA with 27 topics
Coffee_lda<- LDA(Coffee_dtm2, k = k)
Coffee_Topics<- topics(Coffee_lda)
Coffee_Terms<- as.matrix(terms(Coffee_lda,20))
Topic_probab<- as.data.frame(Coffee_lda@gamma)

Coffee$Topics<- Coffee_Topics

# Tidy Text
# Remove stop words from the Text
new_stopwords<- stopwords("en")[-167]
new_stopwords<- paste0(new_stopwords, " ", collapse = "|")
f5<- function(x)
{
  x<- gsub(new_stopwords, "", x, ignore.case = T)
  return(x)
}
Coffee<- mutate(Coffee, Text2= f5(Text))

Topics1<- Coffee[Coffee$Topics==1,]
Topics2<- Coffee[Coffee$Topics==2,]
Topics3<- Coffee[Coffee$Topics==3,]

#Topic 1
Topic1_biagram<- unnest_tokens(Topics1, bigram, Text2, token = "ngrams", n = 2 )
# Sorting the biagram
Topic1_biagram<- Topic1_biagram %>%count(bigram, sort = TRUE)

# Tpoic2
Topic2_biagram<- unnest_tokens(Topics2, bigram, Text2, token = "ngrams", n = 2 )
# Sorting the biagram
Topic2_biagram<- Topic2_biagram %>%count(bigram, sort = TRUE)

# Tpoic3
Topic3_biagram<- unnest_tokens(Topics3, bigram, Text2, token = "ngrams", n = 2 )
# Sorting the biagram
Topic3_biagram<- Topic3_biagram %>%count(bigram, sort = TRUE)


# # ###################Seoarated Biagrams
Topic1_separated<- Topic1_biagram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

View(Topic1_separated%>% filter(word2 == "roast"))


Topic2_separated<- Topic2_biagram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

View(Topic1_separated%>% filter(word1 == "not"))


Topic3_separated<- Topic3_biagram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

View(Topic1_separated%>% filter(word2 == "shipment"))

