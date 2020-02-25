#Importazione dati
class <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/class.csv")

zoo <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/zoo.csv")

################
# Import librerie
library(ggplot2) # visualizzazione dati
library(readr) # lettura file .csv
library(Hmisc) # Statistiche descrittive
library(gridExtra) # Griglia per visualizzazione
library(plyr) # data manipulation
library(corrplot) # grafico correlazioni

################
#Merge
class = rename(class , c("Class_Number"="class_type" , "Class_Type" = "Types"))
zoo = merge (zoo , class , by = c("class_type")  )

################
# Grafici per Statistiche Descrittive
dev.new()
par(mfrow = c(1,2))
grid.arrange()

#hair
p <- ggplot(zoo, aes(x=hair , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$hair)

#feathers
p <- ggplot(zoo, aes(x=feathers , fill = Types)) # creazione grafico
p <- p + geom_histogram(binwidth = 0.5) #aggiunta istogramma
p <- p + scale_fill_brewer(palette="Set3") #definizione colori
p <- p + facet_wrap( ~ Types, ncol=7) #differenzazione per tipologia
p +   scale_x_continuous(breaks = zoo$feathers) #stampa grafico con dati adeguati

#eggs
p <- ggplot(zoo, aes(x=eggs , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$eggs)

#milk
p <- ggplot(zoo, aes(x=milk , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$milk)


#airborne
p <- ggplot(zoo, aes(x=airborne , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$airborne)

#aquatic
p <- ggplot(zoo, aes(x=aquatic , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$aquatic)

#predator
p <- ggplot(zoo, aes(x=predator , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$predator)

#toothed
p <- ggplot(zoo, aes(x=toothed , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$toothed)


#backbone
p <- ggplot(zoo, aes(x=backbone , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$backbone)


#breathes
p <- ggplot(zoo, aes(x=breathes , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$breathes)



#venomous
p <- ggplot(zoo, aes(x=venomous , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$venomous)




#fins
p <- ggplot(zoo, aes(x=fins , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$fins)




#legs
p <- ggplot(zoo, aes(x=legs , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$legs)



#tail
p <- ggplot(zoo, aes(x=tail , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$tail)




#domestic
p <- ggplot(zoo, aes(x=domestic , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$domestic)


#catsize
p <- ggplot(zoo, aes(x=catsize , fill = Types)) 
p <- p + geom_histogram(binwidth = 0.5)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Types, ncol=7)
p +   scale_x_continuous(breaks = zoo$catsize)

#Grafici Correlazione
corrplot(cor(zoo[,c("hair" , "feathers" , "eggs" , "milk" , "airborne" , "aquatic" , "predator" , "toothed" , "backbone" , "breathes" , "venomous" , "fins" ,  "tail" , "domestic" , "catsize" )]), type = "upper", method="ellipse",order = "hclust",
         col = c("lightblue", "lightgreen"), bg = "lightyellow")


corrplot(cor(zoo[,c("hair" , "feathers" , "eggs" , "milk" , "airborne" , "aquatic" , "predator" , "toothed" , "backbone" , "breathes" , "venomous" , "fins" ,  "tail" , "domestic" , "catsize" )]), type = "upper", method="number",order = "hclust",
         col = c("darkblue", "darkgreen"), bg = "lightyellow")

################
# Creazione dei Dataset di Train e test

class <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/class.csv")

zoo <- read.csv("C:/Users/chiar/Desktop/Progetto personale zoo/zoo.csv")


test=test[-1]
ytest=test[17]
test=test[-17]
y=as.factor(train$class_type)
train=train[-1]
train=train[-17]

nums <- sapply(train, is.integer)
corrplot(cor(train[,nums]), type = "upper", method="number",order = "hclust",
         col = c("darkblue", "darkgreen"), bg = "lightyellow")

corrplot(cor(train[,nums]), type = "upper", method="ellipse",order = "hclust",
         col = c("lightblue", "lightgreen"), bg = "lightyellow")

