library(tidyverse)
library(haven)
library(readr)
library(lifecycle)
remotes::install_github("kjhealy/gssr")
library(gssr)
library(readxl)

## load data
data(gss_all) 
data(gss_doc)
data(gss_panel_doc)

## test gssr functions
gss_doc %>% filter(id == "race") %>% 
  select(id, description, text) %>% 
  print(text = Inf)

gss_panel_doc$text[657]


### placeholder test to see marginals
gss_doc %>% filter(id == "natfare") %>%
  select(marginals) %>% 
  unnest(cols = c(marginals))


## increase from -27 to -13
gss_get_marginals(varnames = c("GRASS_1", "GRASS_2", "GRASS_3"), data = gss_panel_doc)

## goes from too little to too high
gss_get_marginals(varnames = c("NATRACE_1", "NATRACE_2", "NATRACE_3"), data = gss_panel_doc)

## also goes from too little to too high
view(gss_get_marginals(varnames = c("NATFARE_1", "NATFARE_2", "NATFARE_3"), data = gss_panel_doc))

## nice big drop off
view(gss_get_marginals(varnames = c("NATHEALY_1", "NATHEALY_2", "NATHEALY_3"), data = gss_panel_doc))




## confirm years
gss_all %>%
  gss_which_years(c(grass, natrace, natfare, nathealy)) %>%
  print(n = Inf)


## see classes of observations
gss_get_props(varnames = c("grass", "natrace", "natfare", "nathealy"))

## Categories chosen: One explicitly social, one explicitly fiscal, two in the middle
## -- one of which is explicitly racial and one of which is subtly racial


## load panel data
data("gss_panel10_long")
data("gss_panel08_long")
data("gss_panel06_long")


## remove attrition

## take wave 3
## vector of ids foor which wave 3 is NA
## remove with for loop

## repeat with wave 2


gss_panel10_long$firstid <- as.character(gss_panel10_long$firstid)
gss_panel10_long$id <- as.character(gss_panel10_long$id)

ten_no_attrition <- gss_panel10_long[1:3,]
ten_no_attrition <- as.data.frame(ten_no_attrition)


ten_ids <- unique(gss_panel10_long$firstid)

for (i in 1:(length(ten_ids))) {
  
  temp <- gss_panel10_long[gss_panel10_long$firstid == i,]
  
  na_check <- temp$id
  
  if (!is.na(na_check[1])) {
    if (!is.na(na_check[2])) {
      if (!is.na(na_check[3])) {
        
        ten_no_attrition <- rbind(ten_no_attrition, temp)
      }
    }
  }
}

ten_relevant <- subset(ten_no_attrition, select = c(firstid, wave, samptype,
                                                    grass, natrace, nathealy, natfare,
                                                    partyid, race, sex, educ, age, wtpannr123))



## 2008
gss_panel08_long$firstid <- as.character(gss_panel08_long$firstid)
gss_panel08_long$id <- as.character(gss_panel08_long$id)

eight_no_attrition <- gss_panel08_long[1:3,]
eight_no_attrition <- as.data.frame(eight_no_attrition)


eight_ids <- unique(gss_panel08_long$firstid)

for (i in 1:(length(eight_ids))) {
  
  temp <- gss_panel08_long[gss_panel08_long$firstid == i,]
  
  na_check <- temp$id
  
  if (!is.na(na_check[1])) {
    if (!is.na(na_check[2])) {
      if (!is.na(na_check[3])) {
        
        eight_no_attrition <- rbind(eight_no_attrition, temp)
      }
    }
  }
}

eight_relevant <- subset(eight_no_attrition, select = c(firstid, wave,
                                                    grass, natrace, nathealy, natfare,
                                                    partyid, race, sex, educ, age, wtpannr123))


eight_relevant$samptype <- "2008"

## 2006
gss_panel06_long$firstid <- as.character(gss_panel06_long$firstid)
gss_panel06_long$id <- as.character(gss_panel06_long$id)

six_no_attrition <- gss_panel06_long[1:3,]
six_no_attrition <- as.data.frame(six_no_attrition)


six_ids <- unique(gss_panel06_long$firstid)

for (i in 1:(length(six_ids))) {
  
  temp <- gss_panel06_long[gss_panel06_long$firstid == i,]
  
  na_check <- temp$id
  
  if (!is.na(na_check[1])) {
    if (!is.na(na_check[2])) {
      if (!is.na(na_check[3])) {
        
        six_no_attrition <- rbind(six_no_attrition, temp)
      }
    }
  }
}

six_relevant <- subset(six_no_attrition, select = c(firstid, wave,
                                                        grass, natrace, nathealy, natfare,
                                                        partyid, race, sex, educ, age, wtpannr123))


six_relevant$samptype <- "2006"

all_panel <- rbind(six_relevant, eight_relevant, ten_relevant)

all_panel <- all_panel[!duplicated(all_panel),]

all_panel$grassmove <- NA
all_panel$racemove <- NA
all_panel$healthmove <- NA
all_panel$welfaremove <- NA



## identify lasting shifts in opinion
for (i in 2:nrow(all_panel)) {

  
  
  if (all_panel$firstid[i] == all_panel$firstid[i-1]) {
    
    all_panel$grassmove[i] <- all_panel$grass[i] - all_panel$grass[i-1]
    all_panel$racemove[i] <- all_panel$natrace[i] - all_panel$natrace[i-1]
    all_panel$healthmove[i] <- all_panel$nathealy[i] - all_panel$nathealy[i-1]
    all_panel$welfaremove[i] <- all_panel$natfare[i] - all_panel$natfare[i-1]
    
  }
  
}

all_panel$grasstotal <- NA
all_panel$racetotal <- NA
all_panel$healthtotal <- NA
all_panel$welfaretotal <- NA

all_panel <- subset(all_panel, select = -c(grasstotal, racetotal, healthtotal, welfaretotal))


all_panel[is.na(all_panel)] <- 0

total_move <- all_panel %>%
  group_by(firstid, samptype) %>%
  summarise(grasstotal = sum(grassmove),racetotal = sum(racemove),
            healthtotal = sum(healthmove), welfaretotal = sum(welfaremove))

## grass: positive move is trend, negative is contrarian.  Trend is liberal (conservative contrarial)
## race: stable trend, positive movement is conservative contrarian, negative movement is liberal contrarian
## healthcare: positive move is trend, negative is contrarian.  Trend is conservative, liberal is contrarian
## welfare: positive move is trend, negative is contrarian.  Trend is conservative, liberal is contrarian

total_move$libcont <- NA
total_move$concont <- NA
total_move$libtrend <- NA
total_move$contrend <- NA

total_move$concont <- ifelse(total_move$grasstotal < 0, 1, total_move$concont)
total_move$libtrend <- ifelse(total_move$grasstotal > 0, 1, total_move$libtrend)

total_move$libcont <- ifelse(total_move$racetotal < 0, 1, total_move$libcont)
total_move$concont <- ifelse(total_move$racetotal > 0, 1, total_move$concont)

total_move$libcont <- ifelse(total_move$healthtotal < 0, 1, total_move$libcont)
total_move$contrend <- ifelse(total_move$healthtotal > 0, 1, total_move$contrend)

total_move$libcont <- ifelse(total_move$welfaretotal < 0, 1, total_move$libcont)
total_move$contrend <- ifelse(total_move$welfaretotal > 0, 1, total_move$contrend)

total_move$contrarian <- NA
total_move$contrarian <- ifelse(total_move$libcont == 1,
                                ifelse(total_move$concont == 1, 1, total_move$contrarian), total_move$contrarian)

total_move$contrarian_wide <- NA
total_move$contrarian_wide <- ifelse(total_move$contrarian == 1, 1, total_move$contrarian_wide)


total_move$contrarian_wide2 <- ifelse(total_move$libcont == 1,
                                     ifelse(total_move$contrend == 1, 1, 0), 0)

total_move$contrarian_wide3 <- ifelse(total_move$libtrend == 1,
                                     ifelse(total_move$concont == 1, 1, 0), 0)

total_move$contrarian_wide4 <- ifelse(total_move$libtrend == 1,
                                     ifelse(total_move$contrend == 1, 1, 0), 0)

total_move$contrarian_wide <- ifelse((total_move$contrarian_wide == 1|
                                        total_move$contrarian_wide2 == 1|
                                        total_move$contrarian_wide3 == 1|
                                        total_move$contrarian_wide4), 1, 0)

total_move <- subset(total_move, select = -c(contrarian_wide2, contrarian_wide3, contrarian_wide4))

all_contrarians <- total_move[!is.na(total_move$contrarian),]
all_contrarians_wide <- total_move[!is.na(total_move$contrarian_wide),]

demographics <- all_panel[all_panel$wave == 2,]

colstojoin <- c("firstid", "samptype")

all_contrarians <- as.data.frame(all_contrarians)
demographics <- as.data.frame(demographics)
all_contrarians_wide <- as.data.frame(all_contrarians_wide)


all_contrarians <- left_join(all_contrarians, demographics, by = colstojoin)
all_contrarians_wide <- left_join(all_contrarians_wide, demographics, by = colstojoin)

trends <- read_excel(path = "Documents/gss for graph.xlsx")

colnames(trends) <- c("Year", "Category", "Mean")

# trendscut <- trends[trends$Year < 2015,]

trends$Category <- factor(trends$Category, levels = c("Welfare", "Race", "Health", "Marijuana"))


ggplot(trends) + geom_line(aes(x=Year, y= Mean, group = Category, colour=Category)) +
  
  scale_colour_manual(values=c("hotpink", "firebrick3","darkolivegreen3","darkorchid4", "firebrick1",
                               "cornflowerblue","darkgreen","darkorchid1", "blue2"
  )) + 
  
  scale_x_continuous(breaks=seq(2006, 2018, 2)) +
  labs(x = 'Year', y = 'Weighted Mean', title = 'Question Shifts between 2006 and 2018') +
  geom_vline(xintercept = 2014, linetype="dashed", 
             color = "blue", size=1) + 
  theme_light() +
  ylim(1,3)

contrarian_narrow_race_matrix <- matrix(c(
  sum(all_contrarians[which(all_contrarians[,"race"]==1),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"race"]==2),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"race"]==3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==3),"wtpannr123"])
), ncol = 3, byrow = TRUE)

rownames(contrarian_narrow_race_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_narrow_race_matrix) <- c("White", "Black", "Other")

chisq.test(contrarian_narrow_race_matrix, correct = FALSE)



contrarian_wide_race_matrix <- matrix(c(
  sum(all_contrarians_wide[which(all_contrarians_wide[,"race"]==1),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"race"]==2),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"race"]==3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"race"]==3),"wtpannr123"])
), ncol = 3, byrow = TRUE)

rownames(contrarian_wide_race_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_wide_race_matrix) <- c("White", "Black", "Other")

chisq.test(contrarian_wide_race_matrix, correct = FALSE)


contrarian_narrow_sex_matrix <- matrix(c(
  sum(all_contrarians[which(all_contrarians[,"sex"]==1),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"sex"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"sex"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"sex"]==2),"wtpannr123"])
), ncol = 2, byrow = TRUE)

rownames(contrarian_narrow_sex_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_narrow_sex_matrix) <- c("Male", "Female")

chisq.test(contrarian_narrow_sex_matrix, correct = FALSE)



contrarian_wide_sex_matrix <- matrix(c(
  sum(all_contrarians_wide[which(all_contrarians_wide[,"sex"]==1),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"sex"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"sex"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"sex"]==2),"wtpannr123"])
), ncol = 2, byrow = TRUE)

rownames(contrarian_wide_sex_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_wide_sex_matrix) <- c("Male", "Female")

chisq.test(contrarian_wide_sex_matrix, correct = FALSE)



contrarian_narrow_partyid_matrix <- matrix(c(
  sum(all_contrarians[which(all_contrarians[,"partyid"]==0),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==1),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==2),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==3),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==4),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==5),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]==6),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==0),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==4),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==5),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==6),"wtpannr123"])
  ), ncol = 7, byrow = TRUE)


rownames(contrarian_narrow_partyid_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_narrow_partyid_matrix) <- c("Strong Dem", "Weak Dem", "Ind Lean Dem",
                                              "Ind", "Ind Lean Rep", "Weak Rep", "Strong Rep"
                                              )

chisq.test(contrarian_narrow_partyid_matrix, correct = FALSE)


contrarian_wide_partyid_matrix <- matrix(c(
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==0),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==1),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==2),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==3),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==4),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==5),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]==6),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==0),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==1),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==2),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==4),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==5),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]==6),"wtpannr123"])
), ncol = 7, byrow = TRUE)


rownames(contrarian_wide_partyid_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_wide_partyid_matrix) <- c("Strong Dem", "Weak Dem", "Ind Lean Dem",
                                                "Ind", "Ind Lean Rep", "Weak Rep", "Strong Rep"
)

chisq.test(contrarian_wide_partyid_matrix, correct = FALSE)



contrarian_narrow_partyid_matrix_simp <- matrix(c(
  sum(all_contrarians[which(all_contrarians[,"partyid"]<3),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]> 2 & all_contrarians[,"partyid"] < 5),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"partyid"]> 4 & all_contrarians[,"partyid"] < 7),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]<3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]> 2 & all_panel[,"partyid"] < 5),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]> 4 & all_panel[,"partyid"] < 7),"wtpannr123"])
), ncol = 3, byrow = TRUE)


rownames(contrarian_narrow_partyid_matrix_simp) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_narrow_partyid_matrix_simp) <- c("Dem", "Ind", "Rep")


chisq.test(contrarian_narrow_partyid_matrix_simp, correct = FALSE)





contrarian_wide_partyid_matrix_simp <- matrix(c(
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]<3),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]> 2 & all_contrarians_wide[,"partyid"] < 5),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"partyid"]> 4 & all_contrarians_wide[,"partyid"] < 7),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]<3),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]> 2 & all_panel[,"partyid"] < 5),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"partyid"]> 4 & all_panel[,"partyid"] < 7),"wtpannr123"])
), ncol = 3, byrow = TRUE)


rownames(contrarian_wide_partyid_matrix_simp) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_wide_partyid_matrix_simp) <- c("Dem", "Ind", "Rep")


chisq.test(contrarian_wide_partyid_matrix_simp, correct = FALSE)







contrarian_narrow_educ_matrix <- matrix(c(
  sum(all_contrarians[which(all_contrarians[,"educ"]<12),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"educ"]> 11 & all_contrarians[,"educ"] < 16),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"educ"]> 15 & all_contrarians[,"educ"] < 17),"wtpannr123"]),
  sum(all_contrarians[which(all_contrarians[,"educ"]>16),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]<12),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]> 11 & all_panel[,"educ"] < 16),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]> 15 & all_panel[,"educ"] < 17),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]>16),"wtpannr123"])
), ncol = 4, byrow = TRUE)


rownames(contrarian_narrow_educ_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_narrow_educ_matrix) <- c("Less than HS", "Some College", "College", "Advanced Degree")


chisq.test(contrarian_narrow_educ_matrix, correct = FALSE)





contrarian_wide_educ_matrix <- matrix(c(
  sum(all_contrarians_wide[which(all_contrarians_wide[,"educ"]<12),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"educ"]> 11 & all_contrarians_wide[,"educ"] < 16),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"educ"]> 15 & all_contrarians_wide[,"educ"] < 17),"wtpannr123"]),
  sum(all_contrarians_wide[which(all_contrarians_wide[,"educ"]>16),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]<12),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]> 11 & all_panel[,"educ"] < 16),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]> 15 & all_panel[,"educ"] < 17),"wtpannr123"]),
  sum(all_panel[which(all_panel[,"educ"]>16),"wtpannr123"])
), ncol = 4, byrow = TRUE)


rownames(contrarian_wide_educ_matrix) <- c("Contrarians", "All Panel Completers")
colnames(contrarian_wide_educ_matrix) <- c("Less than HS", "Some College", "College", "Advanced Degree")


chisq.test(contrarian_wide_educ_matrix, correct = FALSE)


