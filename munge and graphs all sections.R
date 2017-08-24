# By Park Analysis

#libraries
library("tidyverse")
library("foreign")
library("RColorBrewer")



topdir <- paste0("V:/GIS9-Projects/RSSA_Projects/",
                 "Kimberley_Fire_Scar_Analysis_13013A08/VegetationType/",
                 "DATA/Working")
park <- "DRNP"

setwd(paste0(topdir, "/", park, "/metrics"))

#veg age class sections 2 & 3
dfVCzones <- read.dbf(file = "VegAgeClass/zones/vegageclass_zones.dbf",
                      as.is = TRUE)

# 3 to 5 years
dfVCzones35 <- dfVCzones %>%
  arrange(YEAR) %>%
  filter(AGECLASS >= 3 & AGECLASS <= 5) %>%
  group_by(YEAR, ZONE) %>%
  summarise(hectares = sum(SUM_HECTAR)) %>%
  ungroup() %>%
  spread(ZONE, hectares) %>%
  rename(year = YEAR, eucalypt = '1' , sandstone = '2')

dfVCzones35p <- dfVCzones35 %>%
  gather("vegtype", "hectares", 2:3)

brks <- unique(dfVCzones35p$year)

ggplot(dfVCzones35p) +
  #coord_flip() +
  geom_col(aes(year, hectares, fill = vegtype, colour = vegtype), 
           position = position_dodge()) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("olivedrab4", "khaki3")) +
  scale_x_continuous(name = "", breaks = brks) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = paste0(park, " Time Since Last Burn by Veg Type"),
       subtitle = "Annual area with time since last burn between 3 and 5 years",
       caption = "Source data: NAFI",
       y = "hectares (ha)")
  
# 6 years +
dfVCzones6 <- dfVCzones %>%
  arrange(YEAR) %>%
  filter(AGECLASS >= 6) %>%
  group_by(YEAR, ZONE) %>%
  summarise(hectares = sum(SUM_HECTAR)) %>%
  ungroup() %>%
  spread(ZONE, hectares) %>%
  rename(year = YEAR, eucalypt = '1' , sandstone = '2')

dfVCzones6p <- dfVCzones6 %>%
  gather("vegtype", "hectares", 2:3)

brks2 <- unique(dfVCzones6p$year)

ggplot(dfVCzones6p) +
  #coord_flip() +
  geom_col(aes(year, hectares, fill = vegtype, colour = vegtype), 
           position = position_dodge()) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("olivedrab4", "khaki3")) +
  scale_x_continuous(name = "", breaks = brks2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = paste0(park, " Time Since Last Burn by Veg Type"),
       subtitle = "Annual area with time since last burn greater than or equal to 6 years",
       caption = "Source data: NAFI",
       y = "hectares (ha)")

#season burn section 4
seazones <- read.dbf(file = "BurntPatches_season/zones/burntpatches_season_zones.dbf", as.is = TRUE)

seazonesdf <- seazones %>%
  arrange(YEAR) %>%
  filter(ZONE == 1) %>%
  group_by(YEAR, SEASON) %>%
  summarise(hectares = sum(SUM_HECTAR)) %>%
  rename(year = YEAR, season = SEASON) %>%
  ungroup()

brks3 <- unique(seazonesdf$year)

ggplot(seazonesdf) +
  geom_col(aes(year, hectares, fill = season, colour = season),
           position = position_dodge()) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("olivedrab1", "olivedrab4")) +
  scale_x_continuous(name = "", breaks = brks3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = paste0(park, " Area Burnt - Eucalypt Vegtype"),
       subtitle = "Annual area burnt by season",
       caption = "Source data: NAFI",
       y = "hectares (ha)")
  
seazonesdf2 <- seazones %>%
  arrange(YEAR) %>%
  filter(ZONE == 2) %>%
  group_by(YEAR, SEASON) %>%
  summarise(hectares = sum(SUM_HECTAR)) %>%
  rename(year = YEAR, season = SEASON) %>%
  ungroup()

brks4 <- unique(seazonesdf2$year)

ggplot(seazonesdf2) +
  geom_col(aes(year, hectares, fill = season, colour = season),
           position = position_dodge()) +
  scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("khaki2", "khaki4")) +
  scale_x_continuous(name = "", breaks = brks3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
  labs(title = paste0(park, " Area Burnt - Sandstone Vegtype"),
       subtitle = "Annual area burnt by season",
       caption = "Source data: NAFI",
       y = "hectares (ha)")

#burn patch areas section 1
patchzones <- read.dbf(file = "BurntPatches_area/zones/burntpatches_areas_zones.dbf", as.is = TRUE)

patchzonesdf <- patchzones %>%
  arrange(YEAR) %>%
  mutate(sclass = ifelse(HECTARES < 100, 1,
                         ifelse(HECTARES >= 100 & HECTARES <= 1000, 2,
                                ifelse(HECTARES >= 1000 & HECTARES <= 10000, 3,
                                       ifelse(HECTARES >= 10000 & HECTARES <= 100000, 4, 5))))) %>%
  group_by(YEAR, vegtype, sclass) %>%
  summarise(hectares = sum(HECTARES)) %>%
  ungroup()

agecols <- brewer.pal(5,"Set1")

ggplot(filter(patchzonesdf, vegtype == 1)) +
  geom_col(aes(sclass, hectares/100, fill = as.factor(sclass)), 
           position = position_dodge()) +
  facet_grid(YEAR ~ .) +
  scale_fill_manual(values = agecols,
                    name = "Burn Patch\nSize",
                    breaks = c("1", "2", "3", "4", "5"),
                    labels = c(expression("< 1"~km^2),
                               expression("1-10"~km^2),
                               expression("10-100"~km^2),
                               expression("100-1000"~km^2),
                               expression("> 1000"~km^2))) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  labs(title = paste0(park, " Area Burnt by Patch Size"),
       subtitle = "Eucalypt Vegtype",
       caption = "Source data: NAFI",
       x = "",
       y = expression(~km^2))

ggplot(filter(patchzonesdf, vegtype == 1)) +
  geom_col(aes(YEAR, hectares/100, fill = as.factor(sclass)), 
           position = position_dodge()) +
  scale_fill_manual(values = agecols,
                    name = "Burn Patch\nSize",
                    breaks = c("1", "2", "3", "4", "5"),
                    labels = c(expression("< 1"~km^2),
                               expression("1-10"~km^2),
                               expression("10-100"~km^2),
                               expression("100-1000"~km^2),
                               expression("> 1000"~km^2))) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  theme_bw() +
  labs(title = paste0(park, " Area Burnt by Patch Size"),
       subtitle = "Eucalypt Vegtype",
       caption = "Source data: NAFI",
       x = "",
       y = expression(~km^2))
  

ggplot(filter(patchzonesdf, vegtype == 2)) +
  geom_col(aes(sclass, hectares/100, fill = as.factor(sclass)), 
           position = position_dodge()) +
  facet_grid(YEAR ~ .) +
  scale_fill_manual(values = agecols,
                    name = "Burn Patch\nSize",
                    breaks = c("1", "2", "3", "4", "5"),
                    labels = c(expression("< 1"~km^2),
                               expression("1-10"~km^2),
                               expression("10-100"~km^2),
                               expression("100-1000"~km^2),
                               expression("> 1000"~km^2))) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  labs(title = paste0(park, " Area Burnt by Patch Size"),
       subtitle = "Sandstone Vegtype",
       caption = "Source data: NAFI",
       x = "",
       y = expression(~km^2))

ggplot(filter(patchzonesdf, vegtype == 2)) +
  geom_col(aes(YEAR, hectares/100, fill = as.factor(sclass)), 
           position = position_dodge()) +
  scale_fill_manual(values = agecols,
                    name = "Burn Patch\nSize",
                    breaks = c("1", "2", "3", "4", "5"),
                    labels = c(expression("< 1"~km^2),
                               expression("1-10"~km^2),
                               expression("10-100"~km^2),
                               expression("100-1000"~km^2),
                               expression("> 1000"~km^2))) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  theme_bw() +
  labs(title = paste0(park, " Area Burnt by Patch Size"),
       subtitle = "sandstone Vegtype",
       caption = "Source data: NAFI",
       x = "",
       y = expression(~km^2))
