library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)

veg1 <- read_xlsx("VegData.xlsx")

cnames1 <- colnames(veg1)

#Try
n_distinct(veg1[,1])
n_distinct(veg1[,2])
unique(veg1[,2])

#Now get the count for each column
c <- apply(veg1, 2, n_distinct)
d <- names(c[c==1])
e <- names(c[c>1])

veg2 <- select(veg1, e)

cnames2 <- colnames(veg2)

apply(veg2, 2, n_distinct)

veg3 <- rename(veg2, 
               Geo = `Geo Level`, 
               State = `State ANSI`,
               Data = `Data Item`,
               Category = `Domain Category`)

cnames3 <- colnames(veg3)

unique(veg3[,"Commodity"])
unique(veg3[,"Data"]) %>% print(n=60)
unique(veg3[,"Domain"])
unique(veg3[,"Category"])
unique(veg3[,"Value"])

ChemData <- separate(veg3, Category, into = c("type", "specs"), sep=",")

n_distinct(ChemData[,2])

unique(ChemData[,"type"])

#Classifying RESTRICTED USE CHEMICALS data

ResChem1 <- filter(ChemData, type=="RESTRICTED USE CHEMICAL")
ResChem2 <- select(ResChem1, type, specs) %>% unique()
ResChem3 <- select(ResChem2, -type) %>%
            separate(specs, into = c("KandN", "num"), sep = "=")
ResChem4 <- separate(ResChem3, KandN, into = c("kind", "name"), sep = ":")
ResChem5 <- separate(ResChem4, num, into = c("num"), sep = "[)]")
ResChemFin <- separate(ResChem5, name, into = c("del", "name"), sep = "[(]") %>%
              select(-del)

#Classifying ALL CHEMICALS in General Data

ChemData$specs

ChemData1 <- separate(ChemData, specs, into = c("Treatment", "Name"), sep= ":")
ChemData2 <- filter(ChemData1, !Value %in% c("(D)", NA, "(Z)", "(NA)"))
ChemData3 <- separate(ChemData2, Data, into = c("Veg", "Measure"), sep = "-")
ChemData4 <- separate(ChemData3, Measure, into = c("Measure", "Unit"), sep = ",")
ChemData5 <- select(ChemData4, -Domain, -Veg)
ChemData6 <- separate(ChemData5, Name, into = c("Name", "ID"), sep = "=")
ChemData7 <- separate(ChemData6, ID, into = c("ID"), sep = "[)]")
ChemDataFin <- separate(ChemData7, Name, into = c("del", "Name"), sep = "[(]") %>%
  select(-del)
ChemDataFin$Value <- as.numeric(ChemDataFin$Value)

#Converting data to CSV Files
write.csv(ChemDataFin, "General Chem Data.csv", row.names = FALSE)

write.csv(ResChemFin, "Restricted Chem Data.csv", row.names = FALSE)

#Toxicity Data for RESTRICTIED USE CHEMICALS
ToxData <- as.tibble(read.csv("chemical_tox.csv"))
ToxData$Name <- as.character(ToxData$Name)
ToxData$ID <- as.character(ToxData$ID)
ToxData$X <- as.integer(ToxData$X)
ToxData1 <- select(ToxData, -ID)
View(ToxData1)

#BROCCOLI Data and Table

Bro <- ChemDataFin %>% filter(type == "RESTRICTED USE CHEMICAL", Commodity == "BROCCOLI", Unit == " MEASURED IN LB")
Bro$Value <- as.numeric(broc$Value)
Bro <- left_join(Bro,ToxData1,by="Name")
Bro <- rename(Bro,Real=Value,LD50=X)
Bro <- Bro %>% gather(Real, LD50 , key="Toxicity", value="Value")

ggplot(Bro, aes(x= Name, y=Value )) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(y = "Values(lb) ", x = "Chemical") +
  coord_flip()+
  labs(title="Toxicity Measurements in Brocoli")

#CAULIFLOWER Data and Table

Cau <- ChemDataFin %>% filter(type == "RESTRICTED USE CHEMICAL", Commodity == "CAULIFLOWER", Unit == " MEASURED IN LB")
Cau$Value <- as.numeric(Cau$Value)
Cau <- left_join(Cau, ToxData1, by = "Name")
Cau <- rename(Cau, Real = Value, LD50 = X)
Cau <- Cau %>% gather(Real, LD50, key = 'Toxicity', value = 'Value')

ggplot(Cau, aes(x= Name, y=Value )) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(y = "Values(lb) ",x = "Chemical") +
  coord_flip()+
  labs(title="Toxicity Measurements in Cauliflower")
