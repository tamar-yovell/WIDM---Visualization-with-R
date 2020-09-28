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


