library(googlesheets4)
library(plyr)
library(ggplot2)
library(forcats)
library(dplyr)
All.paps <- read_sheet("https://docs.google.com/spreadsheets/d/1hVUdWC313GWZ7oNKqHix2lwH-fWAWFQ4rCbY90AF8Vk/edit?gid=1370227457#gid=1370227457", sheet = "List_of_Evaluations")

All.paps <- All.paps[!is.na(All.paps$`Accesion Number`),]
All.paps$Eco.Funct.sum <- gsub("\\(.*", "", All.paps$`Ecosystem Function(s)`)
All.paps$Region.sum <- gsub("\\(.*", "", All.paps$Region)
All.paps[All.paps$Region.sum == "America ", "Region.sum"] <- "America"

length(unique(All.paps$'Accesion Number'))

unique(All.paps$Region.sum)

my_colors <- scale_color_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "mixed"="#F0E442", "NS"="#999999"))
my_fill <- scale_fill_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "mixed"="#F0E442", "NS"="#999999"))

All.paps$`Vote Count` <-  factor(All.paps$`Vote Count`, levels = c("positive", "negative", "mixed", "NS"))
All.paps$`Type of Diversity` <-  factor(All.paps$`Type of Diversity`, levels = c("Structural", "Taxonomic", "Functional", "Phylogenetic"))

All.paps$Biome <- as.character(All.paps$Biome)

#Giving us a way to identify the studies that looked at multiple diversity metrics
acc.vec <- unique(All.paps$`Accesion Number`)
for(i in 1:length(acc.vec)){
  acc <- acc.vec[i]
  div.types <- as.data.frame(unique(All.paps[All.paps$`Accesion Number` == acc, "Type of Diversity"]))
  if(nrow(div.types) < 2){
    All.paps[All.paps$`Accesion Number` == acc, "Div.comp"] <- "Structure only"
    All.paps[All.paps$`Accesion Number` == acc, "Div.number"] <- 1
  } else if (nrow(div.types) == 2){
    All.paps[All.paps$`Accesion Number` == acc, "Div.comp"] <- paste0(div.types[1,1], " and ", div.types[2,1])
    All.paps[All.paps$`Accesion Number` == acc, "Div.number"] <- 2
  }else if (nrow(div.types) == 3){
    All.paps[All.paps$`Accesion Number` == acc, "Div.comp"] <- paste0(div.types[1,1], " and ", div.types[2,1], " and ", div.types[3,1])
    All.paps[All.paps$`Accesion Number` == acc, "Div.number"] <- 3
  }else if (nrow(div.types) == 4){
    All.paps[All.paps$`Accesion Number` == acc, "Div.comp"] <- "All four"
    All.paps[All.paps$`Accesion Number` == acc, "Div.number"] <- 4
  }
}

#Changing names so structure is always first
All.paps$Div.comp <- ifelse(All.paps$Div.comp == "Phylogenetic and Structural and Taxonomic", "Structural, Taxonomic, and Phylogenetic" ,All.paps$Div.comp)
All.paps$Div.comp <- ifelse(All.paps$Div.comp == "Functional and Structural", "Structural and Functional" , All.paps$Div.comp)
All.paps$Div.comp <- ifelse(All.paps$Div.comp == "Functional and Structural and Taxonomic", "Structural, Taxonomic, and Functional" , All.paps$Div.comp)
All.paps$Div.comp <- ifelse(All.paps$Div.comp == "Structural and Taxonomic and Functional", "Structural, Taxonomic, and Functional" , All.paps$Div.comp)
All.paps$Div.comp <- ifelse(All.paps$Div.comp == "Functional and Phylogenetic and Structural", "Structural, Functional, and Phylogenetic" , All.paps$Div.comp)

comp.div.df <- All.paps[All.paps$Div.number >= 2, ]

comp.div.df %>%
  ggplot(aes(x = fct_infreq(`Type of Diversity`), color = `Vote Count`, fill = `Vote Count`)) +
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

comp.unq.df <- comp.div.df %>% group_by(`Accesion Number`, `Type of Diversity`, `Functional response`, `Environment`) %>% sample_n(1)
comp.unq.df <- apply(comp.unq.df,2,as.character)
#write.csv(comp.unq.df, file ="C:/Users/lucie/Documents/comp.csv", row.names = F)

#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#
#Figures used in my paper
#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#

#---------------------------------------------#
# Figure showing the ecosystem functions
#---------------------------------------------#
count.funct <- All.paps[All.paps$`Type of Diversity` == "Structural",] %>%
  group_by(`Eco.Funct.sum`, `Vote Count`) %>%
  tally()

count.funct <- tidyr::spread(count.funct, key = `Vote Count`, value = n)

count.funct$Eco.Funct.sum <- as.factor(count.funct$Eco.Funct.sum)
count.funct[c("positive", "negative", "mixed", "NS")][is.na(count.funct[c("positive", "negative", "mixed", "NS")])] <- 0

colnames(count.funct) <- c("Ecosystem Function","positive", "negative", "mixed", "NS")

for(i in 1:nrow(count.funct)){
  count.funct[i, "N"] <- sum(count.funct[i, "positive"], count.funct[i, "negative"], count.funct[i, "mixed"], count.funct[i, "NS"])
}

#---------------------------------------------#
# S3 (Full function table)
#---------------------------------------------#
print(chisq.test(unlist(count.funct)))
write.csv(count.funct , file ="C:/Users/lucie/Documents/funct.csv", row.names = F)

funct.top <- count.funct[count.funct$N >= 5,]
funct.top <- plyr::arrange(funct.top, desc(N))
write.csv(funct.top , file ="C:/Users/lucie/Documents/funct.top.csv", row.names = F)

count.funct <- All.paps[All.paps$`Type of Diversity` == "Structural",] %>%
  group_by(`Eco.Funct.sum` , `Vote Count`) %>%
  tally()

for(i in 1:nrow(count.funct)){
  FUNC <- as.character(count.funct[i, "Eco.Funct.sum"])
  count.funct[i, "pcent"] <- format(round((count.funct[i,"n"]/sum(count.funct[count.funct$Eco.Funct.sum == FUNC, "n"])) *100, digits=1), nsmall=1)
}


count.funct$text.spot <- count.funct$n + 2

count.funct[count.funct$Eco.Funct.sum == "Forest productivity" & count.funct$`Vote Count` == "negative", "text.spot"] <- count.funct[count.funct$Eco.Funct.sum == "Forest productivity" & count.funct$`Vote Count` == "negative", "text.spot"] + 3 
count.funct[count.funct$Eco.Funct.sum == "Forest productivity" & count.funct$`Vote Count` == "mixed", "text.spot"] <- count.funct[count.funct$Eco.Funct.sum == "Forest productivity" & count.funct$`Vote Count` == "mixed", "text.spot"] + 2 
count.funct[count.funct$Eco.Funct.sum == "Soil fertilizer conversion" & count.funct$`Vote Count` == "negative", "text.spot"] <- count.funct[count.funct$Eco.Funct.sum == "Soil fertilizer conversion" & count.funct$`Vote Count` == "negative", "text.spot"] + 2 
count.funct[count.funct$Eco.Funct.sum == "Soil fertilizer conversion" & count.funct$`Vote Count` == "NS", "text.spot"] <- count.funct[count.funct$Eco.Funct.sum == "Soil fertilizer conversion" & count.funct$`Vote Count` == "NS", "text.spot"] + 2 


#-------------------------------------------#
#Figure 4 (Functions with percent vote count)
#-------------------------------------------#
count.funct <- count.funct[count.funct$Eco.Funct.sum %in% unique(funct.top$`Ecosystem Function`),] 

count.funct$`Vote Count` <-  factor(count.funct$`Vote Count`, levels = c("NS", "negative", "mixed", "positive"))

#count.funct$Eco.Funct.sum <- forcats::fct_recode(count.funct$Eco.Funct.sum, "Forest\nproductivity" = "Forest productivity", "Aboveg\nbiomass" = "Aboveground biomass", "Ecosystemmultifunctionality" = "Ecosystem multifunctionality", )
fig.5 <- count.funct %>%
  mutate(Eco.Funct.sum = factor(Eco.Funct.sum, levels=c("Biomass stability", "Timber production", "Soil fertilizer conversion", "Forest energy/nutrients", "Biotic resistance",
                                                        "Temperature regulation", "Soil nutrients", "Ecosystem multifunctionality", "Population density ", 
                                                        "Aboveground biomass","Forest productivity", "Carbon stock", "Biodiversity "))) %>%
  ggplot(aes(x = Eco.Funct.sum, y = n, fill = `Vote Count`, label = paste0(pcent, "%"), color = `Vote Count`)) +
  geom_bar(width=0.85, stat = "identity", position = "dodge") + 
  geom_text(aes(y = (text.spot+3)), size = 4, position = position_dodge(width = 1),colour = "black") +
  my_colors+ my_fill+
  ylab("# of evaluations")+
  xlab("Ecosystem function")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        legend.text=element_text(size=20),
        legend.title=element_blank(),
        legend.position=c(.8,.5))+
  coord_flip()

ggsave(filename = "Figure_5.pdf", plot=fig.5, device = "pdf", path ="C:/Users/lucie/Documents/", width=12, height=8, units="in", dpi =600)

#---------------------------------------------#
# Figure showing Biodiversity breakdown
#---------------------------------------------#
count.funct <- All.paps[All.paps$`Type of Diversity` == "Structural" & All.paps$Eco.Funct.sum == "Biodiversity ",] %>%
  group_by(`Ecosystem Function(s)` , `Vote Count`) %>%
  tally()

count.funct <- tidyr::spread(count.funct, key = `Vote Count`, value = n)

count.funct$`Ecosystem Function(s)`<- as.factor(count.funct$`Ecosystem Function(s)`)
count.funct[c("positive", "negative", "NS")][is.na(count.funct[c("positive", "negative", "NS")])] <- 0

colnames(count.funct) <- c("Ecosystem Function","positive", "negative", "NS")

for(i in 1:nrow(count.funct)){
  count.funct[i, "N"] <- sum(count.funct[i, "positive"], count.funct[i, "negative"], count.funct[i, "NS"])
}

funct.top <- count.funct[count.funct$N >= 5,]
funct.top <- plyr::arrange(funct.top, desc(N))

count.funct <- All.paps[All.paps$`Type of Diversity` == "Structural" & All.paps$Eco.Funct.sum == "Biodiversity ",] %>%
  group_by(`Ecosystem Function(s)` , `Vote Count`) %>%
  tally()

for(i in 1:nrow(count.funct)){
  FUNC <- as.character(count.funct[i, "Ecosystem Function(s)"])
  count.funct[i, "pcent"] <- format(round((count.funct[i,"n"]/sum(count.funct[count.funct$`Ecosystem Function(s)` == FUNC, "n"])) *100, digits=1), nsmall = 1)
}

#-------------------------------------------#
#Figure 5 (Biodiversity with percent vote count)
#-------------------------------------------#

count.funct <- count.funct[count.funct$`Ecosystem Function(s)` %in% unique(funct.top$`Ecosystem Function`),] 

#count.funct$Eco.Funct.sum <- forcats::fct_recode(count.funct$Eco.Funct.sum, "Forest\nproductivity" = "Forest productivity", "Aboveground\nbiomass" = "Aboveground biomass", "Ecosystemmultifunctionality" = "Ecosystem multifunctionality", )

count.funct$`Ecosystem Function(s)` <- forcats::fct_recode(count.funct$`Ecosystem Function(s)`, "Birds"="Biodiversity (Birds)", "Bats"="Biodiversity (Bats)", "Fungi"="Biodiversity (Fungi)", "Mammals"="Biodiversity (Mammals)", "Plants"="Biodiversity (Plants)", "Trees"="Biodiversity (Trees)")

my_colors <- scale_color_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "NS"="#999999"))
my_fill <- scale_fill_manual(values = c("positive" = "#009E73", "negative"="#D55E00",  "NS"="#999999"))

fig.6 <- count.funct %>%
  mutate(`Ecosystem Function(s)`= factor(`Ecosystem Function(s)`, levels=c("Birds", "Mammals", "Bats", "Trees", "Fungi", "Plants")))%>%
  ggplot(aes(x = `Ecosystem Function(s)`, y = n, fill = `Vote Count`, label = paste0(pcent, "%"), color = `Vote Count`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(y = (n+2)), size = 4.5, position = position_dodge(width = 1),colour = "black") +
  my_colors+ my_fill+
  ylab("# of evaluations")+
  xlab("Biodiversity of Taxon")+
  theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.position=c(.8,.8))

ggsave(filename = "Figure_6.pdf", plot=fig.6, device = "pdf", path ="C:/Users/lucie/Documents/", width=12, height=8, units="in", dpi =600)

#---------------------------------------------#
#Regions of study
#---------------------------------------------#
Yes.paps <- read_sheet("https://docs.google.com/spreadsheets/d/1hVUdWC313GWZ7oNKqHix2lwH-fWAWFQ4rCbY90AF8Vk/edit?gid=1370227457#gid=1370227457", sheet = "List_of_Articles_Included")
Yes.paps$Region.sum <- gsub("\\(.*", "", Yes.paps$Region)
Yes.paps <- Yes.paps[!is.na(Yes.paps$Region.sum),]
Yes.paps[Yes.paps$Region.sum == "America ", "Region.sum"] <- "America"

count.reg <- Yes.paps %>%
  group_by(`Region.sum`) %>%
  tally()

count.reg$perc <- count.reg$n/sum(count.reg$n) * 100

#Creating csv to make the map in excel
#---------------------------------------------#
# S1 (Continent table)
#---------------------------------------------#
write.csv(count.reg , file ="C:/Users/lucie/Documents/Region.count.csv", row.names = F)

All.paps$Biome.sum <- ifelse(grepl(";", All.paps$Biome), "Multiple", All.paps$Biome)

All.paps$Biome.sum <- forcats::fct_recode(All.paps$Biome.sum, "Tropical/Subtropical\nmoist broadleaf forest" = "1", "Temperate\nbroadleaf/mixed forest" = "4", "Multiple"="Multiple", "Temperate\nconiferous forest"="5", 
                                                                      "Tropical/Subtropical\ngrasslands/savannas"="7", "Mediterranean\nforests/woodlands/scrubs"="12", "Mangroves"="14", "Taiga"="6", 
                                                                      "Tropical/Subtropical\ndry broadleaf forest"="2", "Temperate\ngrasslands/savannas"="8", "Flooded\ngrasslands/savannas"="9", "Tundra"="11")

#-----------------------------------#
# Figure 3 (Biome Breakdown)
#-----------------------------------#
fig.4 <- All.paps[All.paps$`Type of Diversity` == "Structural",] %>%
  ggplot(aes(x = fct_infreq(as.character(Biome.sum)), color = `Vote Count`, fill = `Vote Count`)) +
  geom_bar(width=.7, position = "stack") +
  labs(x = "WWF Biome", y = "# of evaluations in biome")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5),
        axis.text=element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18))+
  my_colors+ my_fill

ggsave(filename = "Figure_4.pdf", plot=fig.4, device = "pdf", path ="C:/Users/lucie/Documents/", width=12, height=8, units="in", dpi =600)



Region.df <- All.paps[All.paps$`Type of Diversity` == "Structural",] %>%
  tidyr::separate_rows(Biome) %>%
  group_by(Biome) %>%
  tally()

count.biome <- All.paps[All.paps$`Type of Diversity` == "Structural",] %>%
  group_by(`Biome.sum`) %>%
  tally()

#-----------------------------------#
# S2 (BIome table)
#-----------------------------------#
count.biome$perc <- count.biome$n/sum(count.biome$n) * 100
write.csv(count.biome , file ="C:/Users/lucie/Documents/Biome.count.csv", row.names = F)


#---------------------------------------------#
#Year trend
#---------------------------------------------#
Yes.paps <- read_sheet("https://docs.google.com/spreadsheets/d/1uo5qfpGYk_2aIyLBUmy4OIryZqS2gp0uruC2lh4ujkk/edit#gid=1026126328", sheet = "Initial_Yes")
Yes.paps <- Yes.paps[Yes.paps$`Kept (Y/N)` == "Y",]
Yes.paps$Region.sum <- gsub("\\(.*", "", Yes.paps$Region)
Yes.paps <- Yes.paps[!is.na(Yes.paps$Region.sum),]
Yes.paps[Yes.paps$Region.sum == "America ", "Region.sum"] <- "America"

fig.2 <- Yes.paps %>%
  ggplot(aes(x = as.character(Publication.Year), color = Continent, fill = Continent)) +
  geom_bar(position = "stack") +
  labs(x = "", y = "# of papers published")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5),
        axis.text=element_text(size=16),
        axis.title.y = element_text(size=18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20))
ggsave(filename = "Figure_2.pdf", plot=fig.2, device = "pdf", path ="C:/Users/lucie/Documents/", width=12, height=8, units="in", dpi =600)


#----------------------------------------------------#
#----------------------------------------------------#
#Looking at urban papers only
#----------------------------------------------------#
#----------------------------------------------------#
Urb.paps <- All.paps[All.paps$`Urban/Not` == "Urban",]

length(unique(Urb.paps$`Accesion Number`))

count.div <- Urb.paps[Urb.paps$`Type of Diversity` == "Structural",] %>%
  group_by(`Eco.Funct.sum` , `Vote Count` ) %>%
  tally()

count.div <- tidyr::spread(count.div, key = `Vote Count`, value = n)

count.div[is.na(count.div)] <- 0

colnames(count.div) <- c("Ecosystem Function","positive", "negative", "NS")
for(i in 1:nrow(count.funct)){
  count.div[i, "N"] <- sum(count.div[i, "positive"], count.div[i, "negative"], count.div[i, "NS"])
}
count.div <- plyr::arrange(count.div, desc(N))

write.csv(count.div , file ="C:/Users/lucie/Documents/urb.count.csv", row.names = F)

count.div <- Urb.paps[Urb.paps$`Type of Diversity` == "Structural",] %>%
  group_by(`Ecosystem Function(s)` , `Vote Count` ) %>%
  tally()

my_colors.urb <- scale_color_manual(values = c("positive" = "#009E73", "negative"="#D55E00", "NS"="#999999"))
my_fill.urb <- scale_fill_manual(values = c("positive" = "#009E73", "negative"="#D55E00",  "NS"="#999999"))


count.div %>%
  mutate(Eco.Funct.sum = factor(`Ecosystem Function(s)`, levels=c("Particulate matter reduction", "Noise reduction", "Carbon stock",
                                                        "Appreciation", "Aboveground biomass", "Wildness", 
                                                        "Temperature regulation", "Population density  ", "Biodiversity "))) %>%
  ggplot(aes(x = `Ecosystem Function(s)`, y = n, fill = `Vote Count`, label = n, color = `Vote Count`)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = n), size = 5, position = position_stack(vjust = 0.5),colour = "white") +
  my_colors.urb+ my_fill.urb+
  ylab("count")+
  xlab("Ecosystem function")+
  theme_bw()+
  coord_flip()+
  theme(text = element_text(size = 18),plot.title = element_text(hjust = 0.5))
#ggtitle("Vote counts by ecosystem function in the urban environment")

