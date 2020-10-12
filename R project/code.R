library(plyr)
library(ggplot2)
mole_data = read.table("moles_data.txt", sep = "\t", header = TRUE)

# ~~~ Gender pie chart ~~~ 
jpeg("Gender-Pie-Chart.jpg", width = 500, height = 500)
gender_ag = count(mole_data$Mol_gender)
labels = paste(c("Female (", "Male ("), gender_ag$freq, c(")", ")"), sep = "")
pie(gender_ag$freq, labels, init.angle = 70, 
    main = "Mole's gender", col = c("#ff33e3", "blue"))
dev.off()

# ~~~ Time vs occupation chart ~~~
occ_presentator = mapply(grepl, "presentator", mole_data$Mol_occupation)
occ_actor = mapply(grepl, "acteur|actrice", mole_data$Mol_occupation)

x = 1:20
y = rep("Other", 20)
y[occ_actor] = "Actor"
y[occ_presentator] = "TV Anchor"
y = factor(y, levels = c("Other", "TV Anchor", "Actor"))
df = data.frame(x, y)

ggplot(df, aes(x = x, y = y, color = y)) + geom_point(aes(size = 2)) +   
  scale_x_continuous("Season", labels = waiver(), breaks = 1:20) + ylab(NULL) +
  ggtitle("Season vs. Mole occupation") + theme(legend.position="none") +
  theme(plot.title = element_text(hjust=0.5))
ggsave("Time-vs-occupation.jpg", width = 10, height = 2)


# ~~~ Continent bar chart ~~~ 
continent = c("South\nAmerica", "Asia", "Europe", "Australia", "North\nAmerica", "Africa")
frequency = c(6, 6, 4, 2, 2, 1)
df = data.frame(continent, frequency)
df = df[order(frequency),]
ggplot(df, aes(x = reorder(continent, -frequency), y = frequency, fill = "blue")) + 
  geom_bar(stat="identity") + xlab(NULL) + ylab("Count of seasons") + 
  ggtitle("Continents of destination") + theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.position="none")
ggsave("continents.jpg", width = 6, height = 4)

# ~~~ Web scraping for rating plot ~~~ 
library(xml2)
library(rvest)
full_page = read_html("https://en.wikipedia.org/wiki/Wie_is_de_Mol%3F_(Dutch_TV_series)")
ratings_table = full_page %>% html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[6]") %>% html_table(fill = TRUE)
data = data.frame(ratings_table)$"Finale..Reunion..1"
data = data[2:21]
data = as.numeric(gsub(",", "",data))
df = data.frame(1:20, data)
ggplot(df, aes(x = X1.20, y = data)) + geom_point() + 
  xlab("season") + ylab("Number of Viewers") +
  scale_y_continuous(labels = scales::comma) + ggtitle("Finale Rating vs. Season") +
  theme(plot.title = element_text(hjust=0.5))
ggsave("Rating.jpg", width = 6, height = 4)
