
data <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/features.csv", header=TRUE, sep = "|", stringsAsFactors = FALSE)
head(data)
dim(data)
str(data)
labels <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/labels.csv", header=TRUE, sep = "|", stringsAsFactors = FALSE)
head(labels)
str(labels)
dim(labels)
labels$genre=as.factor(labels$genre)

#from trifacta
labels1 <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/labels1.csv", header=TRUE)
str(labels1)
dim(labels1)

total1 <- merge(data,labels1,by="etag")
str(total1)
head(total1)
dim(total1)
write.csv(total1, file = "total1.csv")

#from trifacta all dummy variables created for training
total3 <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/total3.csv", header=TRUE)
str(total3)
dim(total3)

#4------- loading eval file
featureseval <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/featureseval.csv", header=TRUE, sep = "|", stringsAsFactors = FALSE)
str(featureseval)


library(randomForest)

#1-
total3$news=as.factor(total3$news)
rfnews=randomForest(news~date+hour+month+channel_name+series_id2, data = total3)
rfnews
attributes(rfnews)
rfnews$confusion
prfnews=predict(rfnews, total3)
head(prfnews)

#2
evalnews=predict(rfnews, totaleval)
head(evalnews)
plot(evalnews)

#3
featureseval$news=evalnews

#1
total3$talk=as.factor(total3$talk)
rftalk=randomForest(talk~date+hour+month+channel_name+series_id2, data = total3)
rftalk
prftalk=predict(rftalk, total3)
head(prftalk)

#2
evaltalk=predict(rftalk, totaleval)
head(evaltalk)
plot(evaltalk)

#3
featureseval$talk=evaltalk


#1
total3$sports=as.factor(total3$sports)
rfsports=randomForest(sports~date+hour+month+channel_name+series_id2, data = total3)
rfsports
prfsports=predict(rfsports, total3)
head(prfsports)

#2
evalsports=predict(rfsports, totaleval)
head(evalsports)
plot(evalsports)

#3
featureseval$sports=evalsports


#1
total3$event=as.factor(total3$event)
rfevent=randomForest(event~date+hour+month+channel_name+series_id2, data = total3)
rfevent
prfevent=predict(rfevent, total3)
head(prfevent)

#2
evalevent=predict(rfevent, totaleval)
head(evalevent)
plot(evalevent)

#3
featureseval$event=evalevent


#1
total3$non=as.factor(total3$non)
rfnon=randomForest(non~date+hour+month+channel_name+series_id2, data = total3)
rfnon
prfnon=predict(rfnon, total3)
head(prfnon)


#2
evalnon=predict(rfnon, totaleval)
head(evalnon)
plot(evalnon)

#3
featureseval$non=evalnon


#1
total3$newsmagazine=as.factor(total3$newsmagazine)
rfnewsmagazine=randomForest(newsmagazine~date+hour+month+channel_name+series_id2, data = total3)
rfnewsmagazine
prfnewsmagazine=predict(rfnewsmagazine, total3)
head(prfnewsmagazine)

#2
evalnewsmagazine=predict(rfnewsmagazine, totaleval)
head(evalnewsmagazine)
plot(evalnewsmagazine)

#3
featureseval$newsmagazine=evalnewsmagazine


#1
total3$politics=as.factor(total3$politics)
rfpolitics=randomForest(politics~date+hour+month+channel_name+series_id2, data = total3)
rfpolitics
prfpolitics=predict(rfpolitics, total3)
head(prfpolitics)

#2
evalpolitics=predict(rfpolitics, totaleval)
head(evalpolitics)
plot(evalpolitics)

#3
featureseval$politics=evalpolitics

#1
total3$interview=as.factor(total3$interview)
rfinterview=randomForest(interview~date+hour+month+channel_name+series_id2, data = total3)
rfinterview
prfinterview=predict(rfinterview, total3)
head(prfinterview)

#2
evalinterview=predict(rfinterview, totaleval)
head(evalinterview)
plot(evalinterview)

#3
featureseval$interview=evalinterview

#1
total3$entertainment=as.factor(total3$entertainment)
rfentertainment=randomForest(entertainment~date+hour+month+channel_name+series_id2, data = total3)
rfentertainment
prfentertainment=predict(rfentertainment, total3)
head(prfentertainment)

#2
evalentertainment=predict(rfentertainment, totaleval)
head(evalentertainment)
plot(evalentertainment)

#3
featureseval$entertainment=evalentertainment

#1
total3$public=as.factor(total3$public)
rfpublic=randomForest(public~date+hour+month+channel_name+series_id2, data = total3)
rfpublic
prfpublic=predict(rfpublic, total3)
head(prfpublic)

#2
evalpublic=predict(rfpublic, totaleval)
head(evalpublic)
plot(evalpublic)

#3
featureseval$public=evalpublic


#1
total3$affairs=as.factor(total3$affairs)
rfaffairs=randomForest(affairs~date+hour+month+channel_name+series_id2, data = total3)
rfaffairs
prfaffairs=predict(rfaffairs, total3)
head(prfaffairs)

#2
evalaffairs=predict(rfaffairs, totaleval)
head(evalaffairs)
plot(evalaffairs)

#3
featureseval$affairs=evalaffairs

#1
total3$football=as.factor(total3$football)
rffootball=randomForest(football~date+hour+month+channel_name+series_id2, data = total3)
rffootball
prffootball=predict(rffootball, total3)

#2
evalfootball=predict(rffootball, totaleval)
head(evalfootball)
plot(evalfootball)

#3
featureseval$football=evalfootball


#1
total3$basketball=as.factor(total3$basketball)
rfbasketball=randomForest(basketball~date+hour+month+channel_name+series_id2, data = total3)
rfbasketball
prfbasketball=predict(rfbasketball, total3)

#2
evalbasketball=predict(rfbasketball, totaleval)
head(evalbasketball)
plot(evalbasketball)

#3
featureseval$basketball=evalbasketball

#1
total3$comedy=as.factor(total3$comedy)
rfcomedy=randomForest(comedy~date+hour+month+channel_name+series_id2, data = total3)
rfcomedy
prfcomedy=predict(rfcomedy, total3)
head(prfcomedy)

#2
evalcomedy=predict(rfcomedy, totaleval)
head(evalcomedy)
plot(evalcomedy)

#3
featureseval$comedy=evalcomedy

#1
total3$bus=as.factor(total3$bus)
rfbus=randomForest(bus~date+hour+month+channel_name+series_id2, data = total3)
rfbus
prfbus=predict(rfbus, total3)
head(prfbus)

#2
evalbus=predict(rfbus, totaleval)
head(evalbus)
plot(evalbus)

#3
featureseval$bus=evalbus

#1
total3$financial=as.factor(total3$financial)
rffinancial=randomForest(financial~date+hour+month+channel_name+series_id2, data = total3)
rffinancial
prffinancial=predict(rffinancial, total3)
head(prffinancial)

#2
evalfinancial=predict(rffinancial, totaleval)
head(evalfinancial)
plot(evalfinancial)

#3
featureseval$financial=evalfinancial

#1
total3$special=as.factor(total3$special)
rfspecial=randomForest(special~date+hour+month+channel_name+series_id2, data = total3)
rfspecial
prfspecial=predict(rfspecial, total3)
head(prfspecial)

#2
evalspecial=predict(rfspecial, totaleval)
head(evalspecial)
plot(evalspecial)

#3
featureseval$special=evalspecial

#1
total3$documentary=as.factor(total3$documentary)
rfdocumentary=randomForest(documentary~date+hour+month+channel_name+series_id2, data = total3)
rfdocumentary
prfdocumentary=predict(rfdocumentary, total3)
head(prfdocumentary)

#2
evaldocumentary=predict(rfdocumentary, totaleval)
head(evaldocumentary)
plot(evaldocumentary)

#3
featureseval$documentary=evaldocumentary

#1
total3$anthology=as.factor(total3$anthology)
rfanthology=randomForest(anthology~date+hour+month+channel_name+series_id2, data = total3)
rfanthology
prfanthology=predict(rfanthology, total3)
head(prfanthology)

#2
evalanthology=predict(rfanthology, totaleval)
head(evalanthology)
plot(evalanthology)

#3
featureseval$anthology=evalanthology

#1
total3$cooking=as.factor(total3$cooking)
rfcooking=randomForest(cooking~date+hour+month+channel_name+series_id2, data = total3)
rfcooking
prfcooking=predict(rfcooking, total3)
head(prfcooking)

#2
evalcooking=predict(rfcooking, totaleval)
head(evalcooking)
plot(evalcooking)

#3
featureseval$cooking=evalcooking

#1
total3$playoff=as.factor(total3$playoff)
rfplayoff=randomForest(playoff~date+hour+month+channel_name+series_id2, data = total3)
rfplayoff
prfplayoff=predict(rfplayoff, total3)
head(prfplayoff)

#2
evalplayoff=predict(rfplayoff, totaleval)
head(evalplayoff)
plot(evalplayoff)

#3
featureseval$playoff=evalplayoff

#1
total3$outdoors=as.factor(total3$outdoors)
rfoutdoors=randomForest(outdoors~date+hour+month+channel_name+series_id2, data = total3)
rfoutdoors
prfoutdoors=predict(rfoutdoors, total3)
head(prfoutdoors)

#2
evaloutdoors=predict(rfoutdoors, totaleval)
head(evaloutdoors)
plot(evaloutdoors)

#3
featureseval$outdoors=evaloutdoors

#1
total3$fishing=as.factor(total3$fishing)
rffishing=randomForest(fishing~date+hour+month+channel_name+series_id2, data = total3)
rffishing
prffishing=predict(rffishing, total3)
head(prffishing)

#2
evalfishing=predict(rffishing, totaleval)
head(evalfishing)
plot(evalfishing)

#3
featureseval$fishing=evalfishing

#1
total3$olympics=as.factor(total3$olympics)
rfolympics=randomForest(olympics~date+hour+month+channel_name+series_id2, data = total3)
rfolympics
prfolympics=predict(rfolympics, total3)
head(prfolympics)

#2
evalolympics=predict(rfolympics, totaleval)
head(evalolympics)
plot(evalolympics)

#3
featureseval$olympics=evalolympics

#1
total3$skiing=as.factor(total3$skiing)
rfskiing=randomForest(skiing~date+hour+month+channel_name+series_id2, data = total3)
rfskiing
prfskiing=predict(rfskiing, total3)
head(prfskiing)

#2
evalskiing=predict(rfskiing, totaleval)
head(evalskiing)
plot(evalskiing)

#3
featureseval$skiing=evalskiing

#5
head(featureseval)
write.csv(featureseval, file = "featuresevalwithoutscore.csv", sep="|")
write.table(featureseval, file = "featuresevalwithoutscorepipe4.csv", sep="|")
?write.table

#reading and writing to csv
evalscore <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/featuresevalwithscore2.csv", header=TRUE, stringsAsFactors = FALSE)
write.table(evalscore, file = "featuresevalwithscorepipe4.csv", sep="|")

#4
eval <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/featureseval.csv", header=TRUE, sep = "|", stringsAsFactors = FALSE)
head(eval)
dim(eval)
str(eval)

#1.1 ---- File from Trifacta
totaleval <- read.csv(file="c:/Users/mq0458/Desktop/CELL-R/data sets/hackathon/totaleval.csv", header=TRUE, stringsAsFactors = FALSE)
head(totaleval)
dim(totaleval)
str(totaleval)
totaleval$news=as.factor(totaleval$news)
totaleval$talk=as.factor(totaleval$talk)
totaleval$sports=as.factor(totaleval$sports)
totaleval$event=as.factor(totaleval$event)
totaleval$non=as.factor(totaleval$non)
totaleval$newsmagazine=as.factor(totaleval$newsmagazine)
totaleval$politics=as.factor(totaleval$politics)
totaleval$interview=as.factor(totaleval$interview)
totaleval$entertainment=as.factor(totaleval$entertainment)
totaleval$public=as.factor(totaleval$public)
totaleval$affairs=as.factor(totaleval$affairs)
totaleval$football=as.factor(totaleval$football)
totaleval$comedy=as.factor(totaleval$comedy)
totaleval$bus=as.factor(totaleval$bus)
totaleval$basketball=as.factor(totaleval$basketball)
totaleval$financial=as.factor(totaleval$financial)
totaleval$special=as.factor(totaleval$special)
totaleval$documentary=as.factor(totaleval$documentary)
totaleval$anthology=as.factor(totaleval$anthology)
totaleval$cooking=as.factor(totaleval$cooking)
totaleval$playoff=as.factor(totaleval$playoff)
totaleval$outdoors=as.factor(totaleval$outdoors)
totaleval$fishing=as.factor(totaleval$fishing)
totaleval$olympics=as.factor(totaleval$olympics)
totaleval$skiing=as.factor(totaleval$skiing)


str(total3)
totaleval$channel_name=as.factor(totaleval$channel_name)
totaleval$series_id2=as.factor(totaleval$series_id2)
totaleval$channel_name=as.factor(totaleval$channel_name)
str(totaleval)


