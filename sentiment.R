#Sentiment Analysis

library(twitteR)
library(ROAuth)
require(RCurl)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(doBy)
library(streamR)
library(RJSONIO)
library(plotly)

key="toZclzv39Sdzo4EfIlXqjRd1y"
secret="yvBzqfORpR5RJLCgHLaCaodstiCvGVGHoeiOzVu7xSHcffjWxl"
setwd("/Users/ArvSharma/Desktop/Social-Media-Analytics-in-Universties/")

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="/Users/ArvSharma/Desktop/Social-Media-Analytics-in-Universties/cacert.pem",
              method="auto")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret)
save(authenticate, file="twitter authentication.Rdata")


unimelb.tweets = searchTwitter('@unimelb', n= 1500)
save(unimelb.tweets,file="/Users/ArvSharma/Desktop/Research/unimelb.tweets.Rdata",ascii=T)
unimelb.text = laply(unimelb.tweets, function(t) t$getText() )

syduni.tweets = searchTwitter('@Sydney_Uni' , n= 1500)
save(syduni.tweets,file="/Users/ArvSharma/Desktop/Research/syduni.tweets.Rdata",ascii=T)
syduni.text = laply(syduni.tweets, function(t) t$getText() )

monash.tweets = searchTwitter('@MonashUni' , n= 1500)
save(monash.tweets,file="/Users/ArvSharma/Desktop/Research/monash.tweets.Rdata",ascii=T)
monash.text = laply(monash.tweets, function(t) t$getText() )

anu.tweets = searchTwitter('@ANUmedia' , n= 1500)
save(anu.tweets,file="/Users/ArvSharma/Desktop/Research/anu.tweets.Rdata",ascii=T)
anu.text = laply(anu.tweets, function(t) t$getText() )

unsw.tweets = searchTwitter('@UNSW' , n= 1500)
save(unsw.tweets,file="/Users/ArvSharma/Desktop/Research/unsw.tweets.Rdata",ascii=T)
unsw.text = laply(unsw.tweets, function(t) t$getText() )

uq.tweets = searchTwitter('@UQ_News', n= 1500)
save(unimelb.tweets,file="/Users/ArvSharma/Desktop/Research/uq.tweets.Rdata",ascii=T)
uq.text = laply(uq.tweets, function(t) t$getText() )

uwa.tweets = searchTwitter('@uwanews', n= 1500)
save(unimelb.tweets,file="/Users/ArvSharma/Desktop/Research/uwa.tweets.Rdata",ascii=T)
uwa.text = laply(uwa.tweets, function(t) t$getText() )

uade.tweets = searchTwitter('@UniofAdelaide', n= 1500)
save(unimelb.tweets,file="/Users/ArvSharma/Desktop/Research/uade.tweets.Rdata",ascii=T)
uade.text = laply(uade.tweets, function(t) t$getText() )

length(unimelb.tweets)
length(syduni.tweets)
length(monash.tweets)
length(anu.tweets)


hu.liu.pos = scan('positive-words.txt',
                  what='character', comment.char=';')
hu.liu.neg = scan('negative-words.txt',
                  what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'scholarship' , 'award', 'research', 'openday')
neg.words = c(hu.liu.neg, 'wtf', 'exams','shit',
              'stupid', 'fail','assignments','quiz')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    #sentence = gsub('[[:alnum:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub("rt", "", sentence)
    
    # Replace @UserName
    sentence <- gsub("@\\w+", "", sentence)
    
    # Remove links
    sentence <- gsub("http\\w+", "", sentence)
    
    # Remove tabs
    sentence <- gsub("[ |\t]{2,}", "", sentence)
    
    # Remove blank spaces at the beginning
    sentence <- gsub("^ ", "", sentence)
    
    # Remove blank spaces at the end
    sentence <- gsub(" $", "", sentence)
    
    sentence = iconv(sentence, 'UTF-8', 'ASCII')
    
    # and convert to lower case:
    sentence = tolower(sentence)
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
unimelb.scores = score.sentiment(unimelb.text, pos.words,
                                 neg.words, .progress='text')
unimelb.scores$uni = 'University of Melbourne'
class(unimelb.scores)
unimelb.scores$score
#unimelb.scores$uni = 'Melbourne Uni'
unimelb.scores$code = 'Unimelb'
unimelb.scores
unimelb.hist <- unimelb.scores$score
#fix(unimelb.hist)
qplot(unimelb.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

syduni.scores = score.sentiment(syduni.text, pos.words,
                                neg.words, .progress='text')
syduni.scores$uni = 'University of Sydney'
class(moansh.scores)
syduni.scores$score
syduni.scores$code = 'Sydu'
syduni.scores
syduni.hist <- monash.scores$score
qplot(syduni.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

monash.scores = score.sentiment(monash.text, pos.words,
                                neg.words, .progress='text')
monash.scores$uni = 'Monash University'
class(moansh.scores)
monash.scores$score
monash.scores$code = 'Monash'
monash.scores
monash.hist <- monash.scores$score
qplot(monash.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

anu.scores = score.sentiment(anu.text, pos.words,
                             neg.words, .progress='text')
anu.scores$uni = 'Australian National University'
class(anu.scores)
anu.scores$score
anu.scores$code = 'Anu'
anu.scores
anu.hist <- anu.scores$score
qplot(anu.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

unsw.scores = score.sentiment(unsw.text, pos.words,
                              neg.words, .progress='text')
unsw.scores$uni = 'University of New South Wales'
class(unsw.scores)
unsw.scores$score
unsw.scores$code = 'UNSW'
unsw.scores
unsw.hist <- unsw.scores$score
qplot(unsw.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

uq.scores = score.sentiment(uq.text, pos.words,
                            neg.words, .progress='text')
uq.scores$uni = 'University of Queensland'
uq.scores$score
uq.scores$code = 'UQ'
uq.scores
uq.hist <- uq.scores$score
#fix(unimelb.hist)
qplot(uq.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

uwa.scores = score.sentiment(uwa.text, pos.words,
                             neg.words, .progress='text')
uwa.scores$uni = 'University of Western Australia'
uwa.scores$score
uwa.scores$code = 'UWA'
uwa.scores
uwa.hist <- uwa.scores$score
qplot(uwa.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

uade.scores = score.sentiment(uade.text, pos.words,
                              neg.words, .progress='text')
uade.scores$uni = 'University of Adelaide'
uade.scores$score
uade.scores$code = 'UA'
uade.scores
uade.hist <- uade.scores$score
qplot(uade.hist, xlim = c(-4,4), ylim = c(0,500), xlab = "Sentiment score", ylab = "Tweet Counts", main = "Sentiment analysis")

# Combine all scores and plot the graphs
all.scores = rbind( unimelb.scores, monash.scores, syduni.scores,unsw.scores,anu.scores,uq.scores,uwa.scores,uade.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=uni), binwidth=1) +
  facet_grid(uni~.) + # making a separate plot for each uni
  theme_bw() + scale_fill_brewer(palette="Set1") # plain display, nicer colors

#Sentiment scoring 

all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )

twitter.df = ddply(all.scores, c('uni'), summarise,
                   pos.count = sum( very.pos ), neg.count = sum( very.neg ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count /
                            twitter.df$all.count )
orderBy(~-score, twitter.df)


unisc.df <- read.csv("/Users/ArvSharma/Desktop/marco_bon/uni_au_scr.csv", header=TRUE)
# only keep column #1 (name) and #18 (2010 score)
unisc.df =  unisc.df[,c(2,6)]
#head(unisc.df,10)
colnames(unisc.df) = c('uni', 'score')
unisc.df$code = c('Unimelb', 'UA','UWA','Anu','UQ','Monash','UNSW','Sydu')
unisc.df$score = as.numeric(unisc.df$score)
#head(unisc.df$score,10)

compare.df = merge(twitter.df, unisc.df,'uni',
                   suffixes=c('.twitter', '.unisc'))

g.scatter = ggplot( compare.df, aes(x=score.twitter, y=score.unisc) ) + 
  geom_point( aes(color=uni), size=5) + 
  theme_bw()
(gg <- ggplotly(g.scatter))

# have ggplot2 fit and plot a linear model with R's lm() function
g.fit = g.scatter + geom_smooth(aes(group=1), se=F, method="lm")

print(g.scatter)
print(g.fit)

(gf <- ggplotly(g.fit))

#t.test(twitter.df$score,unisc.df$score, paired = FALSE, conf.level = 0.95, var.equal = FALSE)
#chisq.test(twitter.df$score,unisc.df$score,p = rep(1/length(x), length(x)), rescale.p = FALSE,
# simulate.p.value = FALSE, B = 2000)

#Pearson Product moment correlation calculation
library(Hmisc)
cor.test(twitter.df$score,unisc.df$score,type="pearson")

# Spearman Rank coefficient
cor.test(twitter.df$score, unisc.df$score,
         alternative = "two.sided",
         method =  "spearman",
         exact = FALSE, conf.level = 0.95, continuity = FALSE)
