#---------------------------------------------------------------------------------------------------
# packages
library(ca)
library(FactoMineR)
library(tidyverse)
library(gtools)
library(factoextra)
library(cluster)
library(cowplot)
library(pheatmap)
library(ape)
library(svglite)
library(epiDisplay)
library(epitools)

#---------------------------------------------------------------------------------------------------
# data
gestantes_f <- readxl::read_xlsx("gestantes_f.xlsx")

gestantes_f$CS_GESTANT3 <- ifelse(gestantes_f$CS_GESTANT2 == "puerpera" | 
                                  gestantes_f$PUERPERA == 1, "puerpera", 
                                  gestantes_f$CS_GESTANT2)

gestantes_f <- dplyr::select(gestantes_f, -c(regiao, CS_GESTANT2, PUERPERA, SIND_DOWN))

gestantes_f[gestantes_f == "1"] <- "yes"
gestantes_f[gestantes_f == "0"] <- "no"

vec_names <- names(gestantes_f)
vec_names <- tolower(vec_names)

vec_names[1] <- "age"
vec_names[2] <- "MV"
vec_names[3] <- "ICU"
vec_names[4] <- "death"
vec_names[5] <- "heart"
vec_names[6] <- "hematologic"
vec_names[7] <- "liver"
vec_names[8] <- "asthma"
vec_names[9] <- "diabetes"
vec_names[10] <- "neurological"
vec_names[11] <- "lung"
vec_names[12] <- "hypertension"
vec_names[13] <- "immuno"
vec_names[14] <- "kidney"
vec_names[15] <- "obesity"
vec_names[16] <- "eclampsia"
vec_names[17] <- "depression"
vec_names[18] <- "smoking"
vec_names[19] <- "miscarriage"
vec_names[20] <- "hypothyroidism"
vec_names[21] <- "chemical"
vec_names[22] <- "fever"
vec_names[23] <- "diarrhea"
vec_names[24] <- "vomiting"
vec_names[25] <- "abdominal"
vec_names[26] <- "cough"
vec_names[27] <- "throat"
vec_names[28] <- "dyspnea"
vec_names[29] <- "respiratory"
vec_names[30] <- "saturation"
vec_names[31] <- "fatigue"
vec_names[32] <- "anosmia"
vec_names[33] <- "ageusia"
vec_names[34] <- "headache"
vec_names[35] <- "tachycardia"
vec_names[36] <- "retro-orbital"
vec_names[37] <- "chest"
vec_names[38] <- "myalgia"
vec_names[39] <- "inappetence"
vec_names[40] <- "malaise"
vec_names[41] <- "congestion"
vec_names[42] <- "race"
vec_names[43] <- "gestacional"

names(gestantes_f)[1:ncol(gestantes_f)] <- vec_names

gestantes_f$MV <- ifelse(gestantes_f$MV == "não", "no/NA",
                         ifelse(gestantes_f$MV == "sim-I", "yes-invasive",
                                ifelse(gestantes_f$MV == "sim-NI","yes-non-invasive","no/NA")))

gestantes_f$ICU <- ifelse(gestantes_f$ICU == "não", "no/NA",
                          ifelse(gestantes_f$ICU == "sim", "yes", "no/NA"))

gestantes_f$death <- ifelse(gestantes_f$death == "não", "no/NA",
                            ifelse(gestantes_f$death == "sim", "yes", "no/NA"))

gestantes_f$race <- ifelse(gestantes_f$race == "Amarela", "white/Asian",
                           ifelse(gestantes_f$race == "Branca", "white/Asian",
                                  ifelse(gestantes_f$race == "Indígena", "indigenous/pardo/black/NA", 
                                         ifelse(gestantes_f$race == "Parda", "indigenous/pardo/black/NA",
                                                ifelse(gestantes_f$race == "Preta", "indigenous/pardo/black/NA",
                                                       "indigenous/pardo/black/NA")))))

gestantes_f$gestacional <- ifelse(gestantes_f$gestacional == "primeiro_tri", "first",
                                  ifelse(gestantes_f$gestacional == "segundo_tri", "second",
                                         ifelse(gestantes_f$gestacional == "terceiro_tri", "third",
                                                ifelse(gestantes_f$gestacional == "puerpera", 
                                                       "puerperium", "NA"))))

gestantes_f <-gestantes_f %>% 
  mutate_if(is.character, as.factor)
summary(gestantes_f)

gestantes_f2 <- gestantes_f %>% 
  dplyr::select(-c(tachycardia, `retro-orbital`, chemical, 
                   smoking, miscarriage, depression, hypothyroidism,
                   liver, neurological, lung, hematologic, kidney, 
                   gestacional, race, age))

#---------------------------------------------------------------------------------------------------
# MCA
res.mca <- MCA(X = gestantes_f2, ncp = 10,  
               graph = FALSE)

vec_names_var <- row.names(res.mca$var$coord)
row.names(res.mca$var$coord)[c(2, 3, 5, 7, #4
                               9, 11, 13, 15, 17, 19, #6
                               21, 23, 25, 27, 29, 31, 33, 35, 37, 39, #10
                               41, 43, 45, 47, 49, 51, 53, 55, 57)] <- c(c("26a", "26b", 27, 28), 
                                                                         c(19:25), 
                                                                         c(10,4,5,3,
                                                                           11,12,
                                                                           6,7,8,
                                                                           13,
                                                                           2,1,
                                                                           16,9,
                                                                           14,15,17,18))  #9

#---------------------------------------------------------------------------------------------------
# Figure 1
fa <- plot(x = res.mca, 
           invisible = c("ind"), 
           xlim = c(-1.5, 2), ylim = c(-1, 2),
           selectMod= c("1","2","3","4","5","6","7","8","9","10",
                        "11","12","13","14","15","16","17","18", 
                        "19","20", "21", "22", "23", "24", "25", "26a", "26b", "27", "28"),
           repel = TRUE,
           graph.type = "ggplot",  
           col.var = "black"
)
plot(fa)


fa.1 <- fa +
  xlab ("1st PC (10.68%)") +
  ylab ("2nd PC (7.37%)") +
  ggtitle("") +
  theme_classic(base_line_size = 0.7, 
                base_rect_size = 0.7, 
                base_size = 14) +
  theme(panel.border = element_blank(),
        text = element_text(size = 14),
        #title = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

plot(fa.1)

ggsave(file = "fig1t.jpeg", 
       plot = fa.1,
       device = "jpeg", dpi = 300,  width = 8, height = 8)

#---------------------------------------------------------------------------------------------------
# Table 1
eig <- res.mca$eig

#---------------------------------------------------------------------------------------------------
# Cluster
res.hcpc <- HCPC(res = res.mca, nb.clust = 3,
                 method = "ward", 
                 metric = "euclidean", 
                 order = TRUE, graph = FALSE)

inert.gain <- res.hcpc$call$t$inert.gain[1:15]
dt <- data.frame(inert = inert.gain, c = as.factor(1:15))

#---------------------------------------------------------------------------------------------------
# Figure 2
colors_grey = c("#999999","#666666", "#333333")
f2 <- fviz_cluster(res.hcpc, ggtheme = theme_minimal(),
             geom = "point", main = "", 
             xlim = c(-1.0, 1.5),
             ylim = c(-1, 1.5), 
             xlab = "1st PC (10.68%)",
             ylab = "2nd PC (7.37%)",
             palette = colors_grey)

plot(f2)

ggsave(file = "figure2.jpeg", plot = f2,
       device = "jpeg", dpi = 300,  width = 10, height = 10)

#---------------------------------------------------------------------------------------------------
# Supplementary Figures

# Graphical Tree
library(dendextend)
library(dplyr)

res.hcpc$call$t$tree %>% 
  as.dendrogram() %>%
  hang.dendrogram(hang_height = 0.05) %>%
  dendextend::color_branches(k = 3, col = colors_grey) %>% 
  plot(horiz = F)

# Inertia Plot
suppB <- ggplot(dt, aes(y = inert, x = c)) +
  ggplot2::geom_col(fill = "gray80") +
  ylab("inertia gain") +
  xlab("number of clusters") +
  theme_classic()

ggsave(file = "S2.jpeg", plot = suppB,
       device = "jpeg", dpi = 300,  width = 10, height = 8)

#---------------------------------------------------------------------------------------------------
data.clust <- res.hcpc$data.clust

#-------------------------------------------------------------------
# Table 2 and Table 3 (example)

# risk of ICU admission for women in cluster 2 or 3 compared with those in cluster 1

data.clust$ICU.2 <- ifelse(data.clust$ICU == "ICU_yes", "yes", "no/not available")
data.clust$ICU.2 <- as.factor(data.clust$ICU.2)

epiDisplay::tabpct(data.clust$clust, data.clust$ICU.2, percent = "row", graph = FALSE)

epitools::epitab(data.clust$clust, data.clust$ICU.2, method = "riskratio")




