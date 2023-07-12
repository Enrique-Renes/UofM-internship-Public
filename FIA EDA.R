#REQUIRED LIBRARIES
install.packages('ggplot2')
install.packages('dplyr')
install.packages('vioplot')
library(ggplot2)
library(tidyverse)
library(dplyr)
library(vioplot)

#IMPORT DATA
setwd("C:/Users/kiker/Desktop/UofM internship/Data")
TN_TREE <- readr::read_csv("TN_TREE.csv")
TN_PLOTSNAP <- readr::read_csv("TN_PLOTSNAP.csv")
TN_COND <- readr::read_csv("TN_COND.csv")

#EXTRACT CORE VARIABLES DATA
TREE_SUB <- TN_TREE[c('CN','PLT_CN', 'INVYR','STATECD', 'PLOT', 'SUBP', 'TREE','CONDID', 'STATUSCD', 'SPCD','SPGRPCD', 'DIA', 'HT', 'ACTUALHT', 'TREECLCD', 'AGENTCD','TOTAGE', 'TPA_UNADJ', 'DRYBIO_AG')]

PLOTSNAP_SUB <- TN_PLOTSNAP[c('CN','LAT','LON','PLOT', 'INVYR','MEASYEAR', 'PLOT', 'WATERCD', 'ELEV')]

COND_SUB <- TN_COND[c('CN','PLT_CN', 'INVYR','STATECD', 'PLOT','CONDID', 'FORTYPCD', 'MAPDEN','HABTYPCD1', 'SITECLCDEST', 'CARBON_LITTER', 'CARBON_SOIL_ORG','CARBON_UNDERSTORY_AG', 'LIVE_CANOPY_CVR_PCT', 'LAND_COVER_CLASS_CD_RET')]

sapply(TREE_SUB, function(x) sum(is.na(x)))
sapply(PLOTSNAP_SUB, function(x) sum(is.na(x)))
sapply(COND_SUB, function(x) sum(is.na(x)))


#PREPARE DATA (CORE VARIABLES)
Inventory_year <- TREE_SUB$INVYR
Plot <- TREE_SUB$PLOT
Subplot <- TREE_SUB$SUBP
Tree_number_subplot <- TREE_SUB$TREE
Status_code <- TREE_SUB$STATUSCD
Species_code <- TREE_SUB$SPCD
Species_group_code <- TREE_SUB$SPGRPCD
Current_Diameter <- TREE_SUB$DIA
Total_height <- TREE_SUB$HT
Actual_height <- TREE_SUB$ACTUALHT
Tree_class_code <- TREE_SUB$TREECLCD
Death_cause_agentcode <- TREE_SUB$AGENTCD
Total_age <- TREE_SUB$TOTAGE
Tree_acre_year_unadjusted <- TREE_SUB$TPA_UNADJ
Above_ground_dry_biomass <- TREE_SUB$DRYBIO_AG

#SUMMARIZE DATA
a <- summary(Inventory_year)
a <- round(a, 2)
a_na <- sum(is.na(Inventory_year) | is.null(Inventory_year)) / length(Inventory_year) * 100
a$NA_Percentage <- a_na

b <- summary(Plot)
b <- round(b, 2)
b_na <- sum(is.na(Plot) | is.null(Plot)) / length(Plot) * 100
b$NA_Percentage <- b_na

c <- summary(Status_code)
c <- round(c, 2)
c_na <- mean(is.na(Status_code)) * 100
c$NA_Percentage <- c_na

d <- summary(Species_code)
d <- round(d, 2)
d_na <- sum(is.na(Species_code) | is.null(Species_code)) / length(Species_code) * 100
d$NA_Percentage <- d_na

e <- summary(Species_group_code)
e <- round(e, 2)
e_na <- sum(is.na(Species_group_code) | is.null(Species_group_code)) / length(Species_group_code) * 100
e$NA_Percentage <- e_na

f <- summary(Current_Diameter)
f <- round(f, 2)
f_na <- mean(is.na(Current_Diameter)) * 100
f$NA_Percentage <- f_na

g <- summary(Total_height)
g <- round(g, 2)
g_na <- mean(is.na(Total_height)) * 100
g$NA_Percentage <- g_na

h <- summary(Actual_height)
h <- round(h, 2)
h_na <- mean(is.na(Actual_height)) * 100
h$NA_Percentage <- h_na

i <- summary(Tree_class_code)
i <- round(i, 2)
i_na <- mean(is.na(Tree_class_code)) * 100
i$NA_Percentage <- i_na

j <- summary(Death_cause_agentcode)
j <- round(j, 2)
j_na <- mean(is.na(Death_cause_agentcode)) * 100
j$NA_Percentage <- j_na

k <- summary(Total_age)
k <- round(k, 2)
k_na <- mean(is.na(Total_age)) * 100
k$NA_Percentage <- k_na

l <- summary(Tree_acre_year_unadjusted)
l <- round(l, 2)
l_na <- mean(is.na(Tree_acre_year_unadjusted)) * 100
l$NA_Percentage <- l_na

m <- summary(Above_ground_dry_biomass)
m <- round(m, 2)
m_na <- mean(is.na(Above_ground_dry_biomass)) * 100
m$NA_Percentage <- m_na

TREE_SUB$TOTAGE <- as.numeric(as.character(TREE_SUB$TOTAGE))
na_percentage <- TN_tree_stat_summary['NA_Percentage', ]
na_percentage <- unlist(na_percentage)na
plot(na_percentage, )

TN_tree_stat_summary <- cbind(a,b,c,d,e,f,g,h,i,j,k,l,m)
colnames(TN_tree_stat_summary) <- c("Inventory year","Plot","Status code","Species code","Species group code","Current diameter","Total height","Actual height","Tree class code","Death cause agent code","Total age","Tree/acre/year unadjusted","Above ground dry biomass")
write.csv(x=TN_tree_stat_summary, file = "TN_tree_stat_summary.csv")
write.table(x = TN_tree_stat_summary, file = "TN_tree_stat_summary.txt", sep = "\t", row.names = FALSE)
rm(a,b,c,d,e,f,g,h,i,j,k,l,m)

a <- summary(PLOTSNAP_SUB$INVYR)
a <- round(a, 2)
a_na <- mean(is.na(PLOTSNAP_SUB$INVYR)) * 100
a$NA_Percentage <- a_na

b <- summary(PLOTSNAP_SUB$MEASYEAR)
b <- round(b, 2)
b_na <- mean(is.na(PLOTSNAP_SUB$MEASYEAR)) * 100
b$NA_Percentage <- b_na

c <- summary(PLOTSNAP_SUB$WATERCD)
c <- round(c, 2)
c_na <- mean(is.na(PLOTSNAP_SUB$WATERCD)) * 100
c$NA_Percentage <- c_na

d <- summary(PLOTSNAP_SUB$ELEV)
d <- round(d, 2)
d_na <- mean(is.na(PLOTSNAP_SUB$ELEV)) * 100
d$NA_Percentage <- d_na

TN_PLOTSNAP_stat_summary <- cbind(a,b,c,d)
colnames(TN_PLOTSNAP_stat_summary) <- c("Inventory year","Measurement Year","Water on plot code","Elevation")
write.csv(x=TN_PLOTSNAP_stat_summary, file = "TN_PLOTSNAP_stat_summary.csv")
write.table(x = TN_PLOTSNAP_stat_summary, file = "TN_PLOTSNAP_stat_summary.txt", sep = "\t", row.names = FALSE)
rm(a,b,c,d)

a <- summary(COND_SUB$INVYR)
a <- round(a, 2)
a_na <- mean(is.na(COND_SUB$INVYR)) * 100
a$NA_Percentage <- a_na

b <- summary(COND_SUB$FORTYPCD)
b <- round(b, 2)
b_na <- mean(is.na(COND_SUB$FORTYPCD)) * 100
b$NA_Percentage <- b_na

c <- summary(COND_SUB$MAPDEN)
c <- round(c, 2)
c_na <- mean(is.na(COND_SUB$MAPDEN)) * 100
c$NA_Percentage <- c_na

d <- summary(COND_SUB$HABTYPCD1)
d <- round(d, 2)
d_na <- mean(is.na(COND_SUB$HABTYPCD1)) * 100
d$NA_Percentage <- d_na

e <- summary(COND_SUB$SITECLCDEST)
e <- round(e, 2)
e_na <- mean(is.na(COND_SUB$SITECLCDEST)) * 100
e$NA_Percentage <- e_na

f <- summary(COND_SUB$CARBON_LITTER)
f <- round(f, 2)
f_na <- mean(is.na(COND_SUB$CARBON_LITTER)) * 100
f$NA_Percentage <- f_na

g <- summary(COND_SUB$CARBON_SOIL_ORG)
g <- round(g, 2)
g_na <- mean(is.na(COND_SUB$CARBON_SOIL_ORG)) * 100
g$NA_Percentage <- g_na

h <- summary(COND_SUB$CARBON_UNDERSTORY_AG)
h <- round(h, 2)
h_na <- mean(is.na(COND_SUB$CARBON_UNDERSTORY_AG)) * 100
h$NA_Percentage <- h_na

i <- summary(COND_SUB$LIVE_CANOPY_CVR_PCT)
i <- round(i, 2)
i_na <- mean(is.na(COND_SUB$LIVE_CANOPY_CVR_PCT)) * 100
i$NA_Percentage <- i_na

j <- summary(COND_SUB$LAND_COVER_CLASS_CD_RET)
j <- round(j, 2)
j_na <- mean(is.na(COND_SUB$LAND_COVER_CLASS_CD_RET)) * 100
j$NA_Percentage <- j_na

TN_COND_stat_summary <- cbind(a,b,c,d,e,f,g,h,i,j)
colnames(TN_COND_stat_summary) <- c("Inventory year","Forest type code","Mapping density","Habitat type code 1", "Site productivity class code estimated", "Carbon in litter", "Carbon in soil organic material", "Carbon in understory above ground", "Live canopy cover percent", "Land cover class, retired")
write.csv(x=TN_COND_stat_summary, file = "TN_COND_stat_summary.csv")
write.table(x = TN_COND_stat_summary, file = "TN_COND_stat_summary.txt", sep = "\t", row.names = FALSE)
rm(a,b,c,d,e,f,g,h,i,j)
rm(a_na,b_na,c_na,d_na,e_na,f_na,g_na,h_na,i_na,j_na,k_na,l_na,m_na,n_na)

#HISTOGRAMS 
dev.off()
par(mfrow=c(2, 2))
hist(TREE_SUB$INVYR, xlab = "Inventory year", main = "")
hist(TREE_SUB$STATUSCD, xlab = "Status code", main = "")
hist(TREE_SUB$SPCD, xlab = "Species code", main = "")
hist(TREE_SUB$SPGRPCD, xlab = "Species group code", main = "", xlim= c(0,50), breaks = 150)
hist(TREE_SUB$DIA, xlab = "Diameter", main = "")
hist(TREE_SUB$HT, xlab = "Total height", main = "", xlim= c(0,200), breaks = 150)
hist(TREE_SUB$ACTUALHT, xlab = "Actual height", main = "", xlim= c(0,200), breaks = 150)
hist(TREE_SUB$TREECLCD, xlab = "Tree class code", main = "")
#IT IS EMPTY hist(TREE_SUB$TOTAGE, xlab = "Total age", main = "", breaks = 10)
hist(TREE_SUB$TPA_UNADJ, xlab = "Tree/acre/year unadjusted", main = "")
hist(TREE_SUB$DRYBIO_AG, xlab = "Above ground dry biomass", main = "", xlim= c(0,5000), breaks = 150)

dev.off()
par(mfrow=c(2, 2))
hist(PLOTSNAP_SUB$INVYR, xlab = "Inventory year", main = "")
hist(PLOTSNAP_SUB$MEASYEAR, xlab = "Measurement year", main = "")
hist(PLOTSNAP_SUB$WATERCD, xlab = "Water on plot code", main = "")
hist(PLOTSNAP_SUB$ELEV, xlab = "Elevation", main = "")

dev.off()
par(mfrow=c(2, 2))
hist(COND_SUB$INVYR, xlab = "Inventory year", main = "")
hist(COND_SUB$FORTYPCD, xlab = "Forest type code", main = "")
hist(COND_SUB$MAPDEN, xlab = "Mapping density", main = "")
#COND_SUB$HABTYPCD1 <- as.numeric(COND_SUB$HABTYPCD1)
#hist(COND_SUB$HABTYPCD1, xlab = "Habitat type code 1", breaks = 10)
hist(COND_SUB$SITECLCDEST, xlab = "Site productivity class code estimated", main = "")
hist(COND_SUB$CARBON_LITTER, xlab = "Carbon in litter", main = "")
hist(COND_SUB$CARBON_SOIL_ORG, xlab = "Carbon in soil organic material", main = "")
hist(COND_SUB$CARBON_UNDERSTORY_AG, xlab = "Carbon in understory aboveground", main = "")
hist(COND_SUB$LIVE_CANOPY_CVR_PCT, xlab = "Live canopy cover percent", main = "")
hist(COND_SUB$LAND_COVER_CLASS_CD_RET, xlab = "Land cover class, retired", main = "")


#VIOLIN PLOTS
dev.off()
par(mfrow=c(2, 2))
vioplot(TREE_SUB$INVYR, main = "Inventory year", ylab = "Inventory year")
vioplot(TREE_SUB$STATUSCD, main = "Status code", ylab = "Status code")
vioplot(TREE_SUB$SPCD, main = "Species code", ylab = "Species code")
vioplot(TREE_SUB$SPGRPCD, main = "Species group code", ylab = "Species group code")
vioplot(TREE_SUB$DIA, main = "Diameter", ylab = "Diameter")
vioplot(TREE_SUB$HT, main = "Total height", ylab = "Total height")
vioplot(TREE_SUB$ACTUALHT, main = "Actual height", ylab = "Actual height")
vioplot(TREE_SUB$TREECLCD, main = "Tree class code", ylab = "Tree class code")
vioplot(TREE_SUB$TPA_UNADJ, main = "Tree per acre per year (unadjusted)", ylab = "Tree/acre/year")
vioplot(TREE_SUB$DRYBIO_AG, main = "Above ground dry biomass", ylab = "Above ground dry biomass")

dev.off()
par(mfrow=c(2, 2))
vioplot(PLOTSNAP_SUB$INVYR, main = "Inventory year", ylab = "Inventory year")
vioplot(PLOTSNAP_SUB$MEASYEAR, main = "Measurement year", ylab = "Measurement year")
vioplot(PLOTSNAP_SUB$WATERCD, main = "Water on plot code", ylab = "Water on plot code")
vioplot(PLOTSNAP_SUB$ELEV, main = "Elevation", ylab = "Elevation")

dev.off()
par(mfrow=c(2, 2))
vioplot(COND_SUB$INVYR, main = "Inventory year", ylab = "Inventory year")
vioplot(COND_SUB$FORTYPCD, main = "Forest type", ylab = "Forest type")
vioplot(COND_SUB$MAPDEN, main = "Mapping density", ylab = "Mapping density")
vioplot(COND_SUB$SITECLCDEST, main = "Site productivity class code", ylab = "Site productivity class code")
vioplot(COND_SUB$CARBON_LITTER, main = "Carbon in litter", ylab = "Carbon in litter")
vioplot(COND_SUB$CARBON_SOIL_ORG, main = "Carbon in soil organic material", ylab = "Carbon in soil organic material")
vioplot(COND_SUB$CARBON_UNDERSTORY_AG, main = "Carbon in understory aboveground", ylab = "Carbon in understory aboveground")
vioplot(COND_SUB$LIVE_CANOPY_CVR_PCT, main = "Live canopy cover percentage", ylab = "Live canopy cover percentage")
vioplot(COND_SUB$LAND_COVER_CLASS_CD_RET, main = "Land cover class code", ylab = "Land cover class code")

#CORRELATION MATRIX

TREE_cor_matrix <- cor(TREE_SUB[c('INVYR', 'STATUSCD', 'SPCD', 'SPGRPCD', 'DIA', 'HT', 'ACTUALHT', 'AGENTCD', 'TPA_UNADJ', 'DRYBIO_AG')], use = "complete.obs")
TREE_cor_matrix <- round(TREE_cor_matrix, 2)
TREE_cor_matrix[is.na(TREE_cor_matrix)] <- 0
TREE_cor_matrix

PLOTSNAP_cor_matrix <- cor(PLOTSNAP_SUB[c('INVYR', 'LAT', 'LON', 'MEASYEAR', 'WATERCD', 'ELEV')], use = "complete.obs")
PLOTSNAP_cor_matrix <- round(PLOTSNAP_cor_matrix, 2)
PLOTSNAP_cor_matrix[is.na(PLOTSNAP_cor_matrix)] <- 0
PLOTSNAP_cor_matrix

COND_cor_matrix <- cor(COND_SUB[c('INVYR', 'FORTYPCD', 'MAPDEN', 'HABTYPCD1', 'CARBON_LITTER', 'CARBON_SOIL_ORG', 'CARBON_UNDERSTORY_AG', 'LIVE_CANOPY_CVR_PCT', 'LAND_COVER_CLASS_CD_RET')], use = "complete.obs")
COND_cor_matrix <- round(COND_cor_matrix, 2)
COND_cor_matrix[is.na(COND_cor_matrix)] <- 0 
COND_cor_matrix


#R markdown and export in PDF or HTML to send to Dr.Kwon 
install.packages("rmarkdown")
install.packages("knitr")
