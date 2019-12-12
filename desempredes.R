#script content
#packages needed for this project
install.packages("tidyverse")
library(tidyverse)
tidyverse_update()
sessionInfo(c("tidyverse"))
install.packages("ggnewscale")
#nperf file
nperf<-read.csv("/Users/leandroaurelio/Downloads/my_nPerf_results-2.csv")
nperfres2<-read.csv("/Users/leandroaurelio/Downloads/nPerfres.csv")

#read new csv files
nperftot <- read.csv("/Users/leandroaurelio/Downloads/nperfrestot.csv")
nperf3g <- read.csv("/Users/leandroaurelio/Downloads/nperfres3g.csv")
nperf4g <- read.csv("/Users/leandroaurelio/Downloads/nperfres4g.csv")

###General observation according to weather###

#3G scenario
ggplot(data = nperf3g) + 
  labs(subtitle="Potência de sinal (dBm) x Vazão (Kbps) - Clima",
       title="Ambiente 3G")+
  geom_point(mapping = aes(x = WEATHER, y = NET_NAME, color = MOBILE_RSSI_END, size = SPEED_DOWNLOAD_AVG))


#4G scenario
ggplot(data = nperf4g) + 
  labs(subtitle="Potência de sinal (dBm) x Vazão (Kbps) - Clima",
       title="Ambiente 4G")+
  geom_point(mapping = aes(x = WEATHER, y = NET_NAME, color = MOBILE_RSRP_END, size = SPEED_DOWNLOAD_AVG))

###General observation according to location###
#3G scenario
ggplot(data = nperf3g) + 
  labs(subtitle="Potência de sinal (dBm) x Vazão (Kbps) - Local",
       title="Ambiente 3G")+
  geom_point(mapping = aes(x = LOC_CODE, y = NET_NAME, color = MOBILE_RSSI_END, size = SPEED_DOWNLOAD_AVG))+
guides(size=guide_legend(override.aes=list(colour="red")))

#4G scenario
ggplot(data = nperf4g) + 
  labs(subtitle="Potência de sinal (dBm) x Vazão (Kbps) - Local",
       title="Ambiente 4G")+
  geom_point(mapping = aes(x = LOC_CODE, y = NET_NAME, color = MOBILE_RSRP_END, size = SPEED_DOWNLOAD_AVG))+
guides(size=guide_legend(override.aes=list(colour="red")))


###3G for bar (professor suggestion, test 1)
ggplot(data = nperf3g, aes(x=Period,y=SPEED_DOWNLOAD_AVG))+
  geom_bar(aes(fill = Period), position=position_dodge(), stat="identity", color="grey15") +
  scale_fill_manual(values = c("green","black","blue"))+
  new_scale_fill()+ 
  geom_bar(aes(x=Period,y=SPEED_DOWNLOAD_AVG,fill=NET_NAME),stat="identity",position=position_dodge(), color="grey45")

#3G for bar plot (professor suggestion, test2)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df3 <- data_summary(nperf3g, varname="SPEED_DOWNLOAD_AVG", 
                    groupnames=c("NET_NAME", "Period"))

# Convert dose to a factor variable
df3$Period=as.factor(df3$Period)
head(df3)

# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=Period, y=SPEED_DOWNLOAD_AVG, fill=NET_NAME)) + 
  labs(subtitle="Throuput (Kbps) x Periodo",
       title="Ambiente 3G")+ 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SPEED_DOWNLOAD_AVG-sd, ymax=SPEED_DOWNLOAD_AVG+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()
####

clear
clc
#4G for bar plot (professor suggestion, test3)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
install.packages("plyr")
install.packages("tydiverse")
library(tidyverse)
tidyverse_update()
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df3 <- data_summary(nperf4g, varname="SPEED_DOWNLOAD_AVG", 
                    groupnames=c("NET_NAME", "Period"))

# Convert dose to a factor variable
df3$Period=as.factor(df3$Period)
head(df3)

# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=Period, y=SPEED_DOWNLOAD_AVG, fill=NET_NAME)) + 
  labs(subtitle="Throuput (Kbps) x Periodo",
       title="Ambiente 4G")+ 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SPEED_DOWNLOAD_AVG-sd, ymax=SPEED_DOWNLOAD_AVG+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()
####
#4G for bar plot (professor suggestion, test3)
#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
install.packages("plyr")
install.packages("tydiverse")
library(tidyverse)
tidyverse_update()
library(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df3 <- data_summary(nperf4g, varname="SPEED_DOWNLOAD_AVG", 
                    groupnames=c("NET_NAME", "WEATHER"))

# Convert dose to a factor variable
df3$WEATHER=as.factor(df3$WEATHER)
head(df3)

# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=WEATHER, y=SPEED_DOWNLOAD_AVG, fill=NET_NAME)) + 
  labs(subtitle="Throuput (Kbps) x Clima",
       title="Ambiente 3G")+ 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SPEED_DOWNLOAD_AVG-sd, ymax=SPEED_DOWNLOAD_AVG+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()
####

ggplot(data=nperf4g, aes(x=Period, y=SPEED_DOWNLOAD_AVG, fill=NET_NAME)) +
  geom_bar(stat="identity", position=position_dodge())
###

p <- ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal()



# load package and data
data(nperf3g, package="ggplot2")

nperfselect <- nperfres2[nperfres2$NET_NAME %in% c("Claro", "TIM"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(nperfselect, aes(SPEED_DOWNLOAD_AVG, Period)) + 
  labs(subtitle="dB: Measurement Power and Period (4G)",
       title="Bubble chart")

g + geom_jitter(aes(col=NET_NAME, size=MOBILE_RSRP_END)) + 
  geom_smooth(aes(col=SPEED_UPLOAD_AVG), method="lm", se=F)

########################

# load package and data
library(ggplot2)
nperfres2<-read.csv("/Users/leandroaurelio/Downloads/nPerfres.csv")
nperfres3<-read.csv("/Users/leandroaurelio/Downloads/nPerfres3 - my_nPerf_results-2.csv")
nperfres4<-read.csv("/Users/leandroaurelio/Downloads/nperfres4 - Sheet1.csv")
data(nperfres2, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

nperfselect <- nperfres2[nperfres2$NET_NAME %in% c("Claro", "TIM"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(nperfselect, aes(SPEED_DOWNLOAD_AVG, Period)) + 
  labs(subtitle="dB: Measurement Power and Period (3G)",
       title="Bubble chart")

g + geom_jitter(aes(col=NET_NAME, size=MOBILE_RSSI_END)) #+ 
  #geom_smooth(aes(col=SPEED_UPLOAD_AVG), method="lm", se=F)
#colocar o nome da metrica legivel xlabel,ylabel - (potencia sinal x vazao), separar
#por operadora, tentar separar por dia, (experimentar dar um numero para cada medicao)
#em suma, potencia do sinal throuput (Mbps) focar no objetivo.


####
library(ggplot2)
ggplot(nperfres4, aes(DATETIME_UTC, SPEED_DOWNLOAD_AVG)) + 
  geom_line(aes(group = MOBILE_SIM_OP), colour = "grey50") + 
  geom_point(aes(colour = MOBILE_SIM_OP))
