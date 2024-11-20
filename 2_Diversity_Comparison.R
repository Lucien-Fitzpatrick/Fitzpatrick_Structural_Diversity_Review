library(googlesheets4)
library(plyr)
library(ggplot2)
library(forcats)
library(dplyr)
Comp.paps <- read_sheet("https://docs.google.com/spreadsheets/d/1hVUdWC313GWZ7oNKqHix2lwH-fWAWFQ4rCbY90AF8Vk/edit?gid=1370227457#gid=1370227457", sheet = "Diversity_Comparison_List")

Comp.paps <- Comp.paps[!is.na(Comp.paps$`Accesion Number`),]
Comp.paps$Eco.Funct.sum <- gsub("\\(.*", "", Comp.paps$`Ecosystem Function(s)`)
Comp.paps$Region.sum <- gsub("\\(.*", "", Comp.paps$Region)
Comp.paps[Comp.paps$Region.sum == "America ", "Region.sum"] <- "America"

length(unique(Comp.paps$`Accesion Number`))

unique(Comp.paps$Region.sum)

my_colors <- scale_color_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "mixed"="#F0E442", "NS"="#999999"))
my_fill <- scale_fill_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "mixed"="#F0E442", "NS"="#999999"))

Comp.paps$Biome <- as.character(Comp.paps$Biome)

Comp.paps %>%
  ggplot(aes(x = fct_infreq(`Comp.best`))) +
  facet_wrap(~Div.comp, scales = "free_x")+
  geom_bar(position = "dodge") +
  #geom_bar(stat = "count") + 
  stat_count(geom = "text",  size = 5,
             aes(label = ..count..),position=position_dodge(width = 0.9), vjust = -0.1)+
  labs(x = "Type of Diversity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  my_colors+ my_fill+
  ylim(0, 70)+
  theme(text = element_text(size = 20))+
  ggtitle("Vote counts comparing types of diversity")

Comp.paps <- Comp.paps[Comp.paps$Div.comp == "Structural and Taxonomic",]

count.div <- Comp.paps %>%
  group_by(`Div.comp`, `Comp.best` ) %>%
  tally()

count.div <- tidyr::spread(count.div, key = `Comp.best`, value = n)
#count.div <- count.div[,c(1,5,6,3,2,4)]

count.div <- Comp.paps %>%
  group_by(`Eco.Funct.sum`, `Comp.best`) %>%
  tally()

count.div <- tidyr::spread(count.div, key = `Comp.best`, value = n)
#count.div <- count.div[,c(1,4,5,2,3)]
count.div[is.na(count.div)] <- 0

for(i in 1:nrow(count.div)){
  count.div[i, "N"] <- sum(count.div[i, "Structural"], count.div[i, "Taxonomic"], count.div[i, "None"], count.div[i, "All"])
}

count.div <- plyr::arrange(count.div, desc(N))
comp.top <- count.div[count.div$N >= 2,]

write.csv(count.div , file ="C:/Users/lucie/Documents/comp.funct.csv", row.names = F)


my_colors.urb <- scale_color_manual(values = c("Structural" = "magenta4", "Taxonomic"="aquamarine4", "All" ="darkblue", "None"="darksalmon"))
my_fill.urb <- scale_fill_manual(values = c("Structural" = "magenta4", "Taxonomic"="aquamarine4", "All" ="darkblue", "None"="darksalmon"))

count.div <- Comp.paps %>%
  group_by(`Eco.Funct.sum`, `Comp.best`) %>%
  tally()

count.div$Comp.best <- factor(count.div$Comp.best, levels = c("None", "All", "Taxonomic", "Structural"))
  
fig.7 <- count.div[count.div$`Eco.Funct.sum` %in% unique(comp.top$`Eco.Funct.sum`),] %>%
  mutate(`Eco.Funct.sum` = factor(`Eco.Funct.sum`, levels=c("Temperature regulation", "Population Density ", "Biomass stability", 
                                                        "Aboveground biomass", "Forest productivity", "Biodiversity ", "Carbon stock"))) %>%
  ggplot(aes(x = `Eco.Funct.sum`, y = n, fill = `Comp.best`, label = n, color = `Comp.best`)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = n), size = 5, position = position_stack(vjust = 0.5),colour = "white") +
  my_colors.urb+ my_fill.urb+
  ylab("# of evaluations")+
  xlab("Ecosystem function")+
  theme_bw()+
  coord_flip()+
  theme(text = element_text(size = 18),plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_text(size=22), legend.text=element_text(size=20))+
  labs(color = "Diversity metric", fill = "Diversity metric")

ggsave(filename = "Figure_7.pdf", plot=fig.7, device = "pdf", path ="C:/Users/lucie/Documents/", width=12, height=8, units="in", dpi =600)


#Looking at just biodiversity functions
count.div <- Comp.paps %>%
  group_by(`Ecosystem Function(s)`, `Comp.best`) %>%
  tally()

count.div$Comp.best <- factor(count.div$Comp.best, levels = c("Neither", "All", "Taxonomic", "Structural"))

count.div[count.div$`Ecosystem Function(s)` %in% c("Biodiversity (Understory)", "Biodiversity (Fungi)", "Biodiversity (Butterflies)", "Biodiversity (Birds)",
                                                   "Biodiversity (Arthropods)", "Abundance (Deer)", "Abundance (Birds)"),] %>%
  mutate(`Ecosystem Function(s)` = factor(`Ecosystem Function(s)`, levels=c("Abundance (Deer)", "Biodiversity (Butterflies)" ,"Biodiversity (Fungi)",
                                                                            "Biodiversity (Arthropods)", "Biodiversity (Understory)", "Abundance (Birds)", "Biodiversity (Birds)"))) %>%
  ggplot(aes(x = `Ecosystem Function(s)`, y = n, fill = `Comp.best`, label = n, color = `Comp.best`)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = n), size = 5, position = position_stack(vjust = 0.5),colour = "white") +
  my_colors.urb+ my_fill.urb+
  ylab("# of evaluations")+
  xlab("Ecosystem function")+
  theme_bw()+
  coord_flip()+
  theme(text = element_text(size = 18),plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_text(size=22), legend.text=element_text(size=20))+
  labs(color = "Diversity metric", fill = "Diversity metric")


Urb.paps <- Comp.paps[Comp.paps$`Urban/Not` == "Urban",]
#Looking only at the Urban
count.urb <- Urb.paps %>%
  group_by(`Div.comp`, `Comp.best` ) %>%
  tally()

count.urb <- tidyr::spread(count.urb, key = `Comp.best`, value = n)
count.urb <- count.urb[,c(1,4,5, 2, 3)]

count.urb[is.na(count.urb)] <- 0

write.csv(count.urb , file ="C:/Users/lucie/Documents/urb.comp.csv", row.names = F)


#only looking at structure vs taxonomic for urban settings
count.urb <- Urb.paps %>%
  group_by(`Ecosystem Function(s)`, `Comp.best`,  ) %>%
  tally()

count.urb <- tidyr::spread(count.urb, key = `Comp.best`, value = n)
count.urb <- count.urb[,c(1,3,4, 2)]

count.urb[is.na(count.urb)] <- 0

for(i in 1:nrow(count.urb)){
  count.urb[i, "N"] <- sum(count.urb[i, "Structural"], count.urb[i, "Taxonomic"], count.urb[i, "Neither"])
}

count.urb <- plyr::arrange(count.urb, desc(N))

count.urb <- count.urb[c(2,3,7,9,10,1,4,5,6,8),]

write.csv(count.urb , file ="C:/Users/lucie/Documents/urb.funct.comp.csv", row.names = F)

count.urb <- Urb.paps %>%
  group_by(`Ecosystem Function(s)`, `Comp.best` ) %>%
  tally()

count.urb %>%
  mutate(`Ecosystem Function(s)` = factor(`Ecosystem Function(s)`, levels=c("Wildness", "Particulate matter reduction", "Noise reduction", 
                                                                            "Biodiversity (Butterflies)" , "Biodiversity (Arthropods)" , "Appreciation", 
                                                                            "Aboveground biomass", "Temperature regulation", "Biodiversity (Birds)"))) %>%
  ggplot(aes(x = `Ecosystem Function(s)`, y = n, fill = `Comp.best`, label = n, color = `Comp.best`)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = n), size = 5, position = position_stack(vjust = 0.5),colour = "white") +
  my_colors.urb+ my_fill.urb+
  ylab("count")+
  xlab("Ecosystem function")+
  theme_bw()+
  coord_flip()+
  theme(text = element_text(size = 18),plot.title = element_text(hjust = 0.5))+
  labs(color = "Diversity metric", fill = "Diversity metric")

