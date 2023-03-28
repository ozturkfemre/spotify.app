library(jsonlite)
library(tidyverse)
library(ggplot2)
library(wesanderson)
library(paletteer) 
library(ggthemes)
library(circular)

history <- fromJSON("MyData/StreamingHistory0.json", flatten = TRUE)



df <- history


paletteer_d("jcolors::pal11")

cp <- c("#202547FF", "#323649FF", "#41474BFF", "#4E5A4CFF", "#5C6C4CFF", "#68804CFF", "#75944BFF", "#81A949FF", "#8EBE45FF", "#9AD340FF", "#A6E939FF", "#B2FF2EFF")

### artists


df %>%
  count(artistName, sort = TRUE) %>%
  top_n(10) %>%
  mutate(artistName = reorder(artistName, n)) %>%
  ggplot(aes(x = artistName, y = n)) +
  geom_bar(aes(fill=n),   
           stat="identity") +
  scale_fill_distiller(palette= cp) +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Artists",
       title = "Artists you listened most to",
       fill = "Count",
       y = " ") +
  theme_dark()+
  theme(plot.background = element_rect(fill = "gray20")) +
  theme(panel.background = element_rect(fill = "gray20")) + 
  theme(legend.background = element_rect(fill = "gray20")) + 
  theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))


### tracks

df %>% 
  count(trackName, sort = TRUE) %>%
  top_n(10) %>%
  mutate(trackName = reorder(trackName, n)) %>%
  ggplot(aes(x = trackName, y = n)) +
  geom_bar(aes(fill=n),   
           stat="identity") +
  scale_fill_distiller(palette=cp) +
  xlab(NULL) +
  coord_flip() +
  labs(y = " ",
       title = "Tracks I listened most to",
       fill = "Count")+ 
  theme_dark()+
  theme(plot.background = element_rect(fill = "gray20")) +
  theme(panel.background = element_rect(fill = "gray20")) + 
  theme(legend.background = element_rect(fill = "gray20")) + 
  theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))

### top tracks from most listened artist

topartist <- df %>%
  count(artistName, sort = TRUE) %>%
  top_n(1) %>%
  mutate(artistName = reorder(artistName, n))

topartist <- topartist$artistName

df %>% 
  filter(artistName == topartist) %>%
  count(trackName, sort = TRUE) %>%
  top_n(10) %>%
  mutate(trackName = reorder(trackName, n)) %>%
  ggplot(aes(x = trackName, y = n)) +
  geom_bar(aes(fill=n),   
           stat="identity") +
  scale_fill_distiller(palette=cp) +
  xlab(NULL) +
  coord_flip() +
  labs(y = " ",
       title = "Top tracks by your most listened artist",
       fill = "Count") +
  theme_dark()+
  theme(plot.background = element_rect(fill = "gray20")) +
  theme(panel.background = element_rect(fill = "gray20")) + 
  theme(legend.background = element_rect(fill = "gray20")) + 
  theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))



# your listening activity

df1 <- as.data.frame(df$endTime)
colnames(df1) <- "endTime"
df1$endTime <- as.POSIXct(df1$endTime,format="%Y-%m-%d %H:%M",tz="GMT")
df1$weekofday <- as.POSIXlt(df1$endTime)$wday
df1$weekofday <- strftime(df1$endTime,'%A')

df1$period <- format(df1$endTime, "%p")
df1$hour <- format(df1$endTime, "%I")
df1$month <- format(df1$endTime, "%b")


### week of day

df1$weekofday <- weekdays(df1$endTime)
ggplot(data.frame(days = names(table(df1$weekofday)), freq = as.numeric(table(df1$weekofday))),
       aes(x = days, y = freq)) +
  geom_bar(stat = "identity", fill = "#75944BFF") +
  labs(title = "Usage by Day of Week",
       x = " ", y = " ")+  
  theme_dark()+
  theme(plot.background = element_rect(fill = "gray20")) +
  theme(panel.background = element_rect(fill = "gray20")) + 
  theme(legend.background = element_rect(fill = "gray20")) + 
  theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))+
  coord_flip() 


### hour of day

df2 <- df1 %>% add_count(period,hour) %>% distinct(period, hour, n)

ggplot(df2, aes(x = as.factor(hour), y = n, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#B2FF2EFF", "#75944BFF")) +
  labs(title = "Usage by Hour", x = "", y = "") +
  theme_dark()+
  theme(plot.background = element_rect(fill = "gray20")) +
  theme(panel.background = element_rect(fill = "gray20")) + 
  theme(legend.background = element_rect(fill = "gray20")) + 
  theme(text=element_text(color="#75944BFF"), axis.text=element_text(color="#75944BFF"))+
  coord_flip() 
