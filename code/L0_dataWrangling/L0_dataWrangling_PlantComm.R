# MH DeSiervo, LG Shoemaker
##Disturbance alters transience but nutrients determine equilibria during grassland succession with multiple global change drivers submitted with DOI https://doi.org/10.5061/dryad.dbrv15f5t. 
# Code to upload the Cedar Creek biomass data and prep the data for analyses####
# Laís altered a few pieces of the code and incorporated origin (native vs. introduced) to the dataset

###PACKAGES####

library(plyr)
library(dplyr)
library(ggplot2)
library(ape)
library(devtools)
library(tidyr)
library(vegan) ##must be the most recent version from github##
library(viridis)
library(stringr)
library(here)
library(tidyverse)
library(tsvr)
library(ggpubr)
library(ecotraj)
library(investr)
library(data.table)
library(trajr)
library(gtable)
library(gridExtra)
library(codyn)
library(indicspecies)
library(BiodiversityR)

############

# source(here("code/1_functions.r"))



# Reading in and cleaning Cedar Creek data###

workingdir <- getwd()
datain <- file.path(workingdir, "data/L0")
dataout <- file.path(workingdir, "data/L1")

#Read in e001 data## Plot data for the unplowed fields##

d1s <- read.csv(file.path(datain,'e001-soil-cn-2019-09-13.csv'), header=T, strip.white=T, skip=1)

# Delete some extra variables and add dummies to stack exp1 & exp2 data
d1s$natm.nadd <- NULL
d1s$fertadd <- NULL
d1s$burn.trt.1992 <- NA
d1s$ntrt.last.year <- 'CurrentYear'
d1s$ntrt.origin <- 1 # No cessation in E001
d1s$disk <- 0 # E001 was not disked before experiment (disturbed vegetation)
d1s$exp <- 1

#Read in e002 data ##Plot data for the plowed fields##

d2s <- read.csv(file.path(datain,'e002-soil-cn-2019-09-13.csv'), header=T, strip.white=T, skip=1)

## Delete some extra variables and add dummies to stack exp1 & exp2 data
d2s$X <- NULL
d2s$date <- NULL
d2s$BurnRecBeginning1992 <- NULL
d2s$NtrtRecBefore1992 <- NULL
d2s$TrtRecAfter1991 <- NULL
d2s$TrtRecAfter2013 <- NULL
d2s$burn <- NA
d2s$fence <- NA
d2s$fence.origin <- NA
d2s$depth <- NA
d2s$disk <- 1 # E002 was disked before experiment (disturbed vegetation)
d2s$exp <- 2


#Additioanl plot info for both experiments##

d3s <- read.csv(file.path(datain,'E001-E002-Soil-CN-2018.csv'), header=T, strip.white=T, skip=0, na.strings=c(""))

# Delete samples that have errors
d3s <- d3s[is.na(d3s$FLAGGED.FOR.RERUN..S.Spillage..E.Error.),]

# Delete some extra variables and add dummies to stack exp1 & exp2 data
d3s$FLAGGED.FOR.RERUN..S.Spillage..E.Error. <- NULL
d3s$Sample <- NULL
d3s$X <- NULL
d3s$burn <- NA
d3s$burn.origin <- NA
d3s$burn.trt.1992 <- NA
d3s$fence <- NA
d3s$fence.origin <- NA
d3s$depth <- NA
d3s$disk <- NA
d3s$nadd <- NA
d3s$ntrt <- NA
d3s$ntrt.last.year <- NA
d3s$ntrt.origin <- NA
d3s$trt.origin <- NA  

d3s$disk[d3s$exp==1] <- 0 # E001 was not disked before experiment (intact vegetation)
d3s$disk[d3s$exp==2] <- 1 # E002 was disked before experiment (disturbed vegetation)

# Check that data sets have the same variables
sort(names(d1s))
sort(names(d2s))
sort(names(d3s))

# Stack exp1 & 2 data
ds <- rbind(d1s,d2s,d3s)


#Read in e001 aboveground plant biomass data ##

d1a <- read.csv(file.path(datain,'e001-aboveground-mass-2019-09-13.csv'), header=T, strip.white=T, skip=1)


# Delete some extra variables and add dummies to stack exp1 & exp2 data
d1a$natm.nadd <- NULL
d1a$fertadd <- NULL
d1a$burn.trt.1992 <- NA
d1a$ntrt.last.year <- "CurrentYear" # No cessation in E001
d1a$ntrt.origin <- 1 # No cessation in E001
d1a$subplot <- NA
d1a$disk <- 0 # E001 was not disked before experiment (disturbed vegetation)
d1a$exp <- 1


#Read in e002 aboveground plant biomass data ##
d2a <- read.csv(file.path(datain,'e002-aboveground-mass-2019-09-13.csv'), header=T, strip.white=T, skip=1)

# Delete some extra variables and add dummies to stack exp1 & exp2 data
d2a$X <- NULL
d2a$date <- NULL
d2a$burn <- NA
d2a$fence <- NA
d2a$fence.origin <- NA
d2a$disk <- 1 # E002 was disked before experiment (intact vegetation)
d2a$exp <- 2


# Check that data sets have the same variables
sort(names(d1a))
sort(names(d2a))


# Stack exp1 & 2 data

da <- rbind(d1a,d2a)

# Delete records with key missing data
da <- da[!is.na(da$mass),]

# Disking not done if field D
da <- da[da$field != 'D',]
da$field <- as.factor(as.character(da$field))


# If subplot is missing specify it as a whole plot
da$subplot[is.na(da$subplot)] <- "Whole"

# Code other Nutrient additions
da$other.add <- 1
da$other.add[da$ntrt == 9] <- 0


################################################################################
# Here we make a clean design file of original treatments                      #
# Make clean subplot and year level file with original treatments only.        #
################################################################################


names(da)

# SUBPLOT SCALE Get a list of plots that have original treatments
design.df <- ddply(da, .(field, exp, plot, subplot, ntrt, nadd, disk, other.add, ntrt.origin), colwise(mean, .(mass.above)))
design.df$mass.above <- NULL
dim(design.df)
with(design.df, table(plot, field, exp))
dim(design.df)

# PLOT SCALE Get a list of plots that have original treatments
design.df.plot <- ddply(da, .(field, exp, plot, ntrt, nadd, disk, other.add, ntrt.origin), colwise(mean, .(mass.above)))
design.df.plot$mass.above <- NULL
dim(design.df.plot)
with(design.df.plot, table(plot, field, exp))
dim(design.df.plot)


# YEAR AND SUBPLOT SCALE Get a list of plots that have original treatments
design.df.yr <- ddply(da, .(field, exp, plot, subplot, year, ntrt, disk, other.add, ntrt.origin, burn.origin, fence.origin), colwise(mean, .(mass.above)))
design.df$mass.above <- NULL
dim(design.df)
with(design.df, table(plot, field, exp))
dim(design.df)

#### Important note: burn.origin column isn't exactly right for B E002. The burn doesn't happen until 1992, so should be 54 plots from 1982-1991 and 27 plots 1992-2004###

summary(design.df.yr)
design.orig <- design.df.yr   #dim 9086 X 12

# Delete cessations plots
design.orig <- design.orig[design.orig$ntrt.origin==1,]  #dim #7799   12#


# Delete subplots with experimental burns (after 1992)
#design.orig <- design.orig[design.orig$burn.origin==1,]
design.orig<-subset(design.orig, year < 1992| year >= 1992 & burn.origin==1) #dim #6153   12#

# Some field level data is not represented in both data sets
# Get this sorted out across merged data set
# Fences removed in 2004 (partial removal in field C but still open generally)
# After 2004 all of E002 is unfenced
design.orig$fence[design.orig$year <= 2004 & design.orig$exp==2] <- 1
design.orig$fence[design.orig$year > 2004 & design.orig$exp==2] <- 0

# After 2004 all of E001 is unfenced except in field C
design.orig$fence[design.orig$year <= 2004 & design.orig$exp==1 & design.orig$field != 'C'] <- 1
design.orig$fence[design.orig$year > 2004 & design.orig$exp==2 & design.orig$field != 'C'] <- 0

# Pull out data that is fenced after 2004 as these were invidual plots 
# fenced in field C (I think) after whole field fences were removed. 
design.orig$sel <- TRUE
design.orig$sel[design.orig$year > 2004 & design.orig$fence == 1] <- FALSE
design.orig <- design.orig[design.orig$sel,]
with(design.orig, table(year,fence, exp))
with(design.orig, table(year, field,fence.origin, exp))
with(design.orig, table(year, fence, field, exp))

dim(design.orig)   #dim #6153   12#
summary(design.orig)

# Get field scale burn record
df.burn <- ddply(d1a, .(field, year), colwise(max, .(burn)), na.rm=T)
# Replace original burn record with field summary
da$burn <- NULL
da <- merge(da, df.burn, by=c("field", "year"), all.x=TRUE)


# MERGE IN DESIGN.ORIG FILE TO ONLY HAVE FILES FOR WHICH TREATMENTS HAVE NOT CHANGED

dim(da)  ##72737  X  19### ##includes N cessation and burned plots###
dim(design.orig)  # Unique plot_years with correct treatments### ##dim 6396##

design.orig2<-design.orig %>% select(field, year, exp, disk, plot, subplot, ntrt, other.add, ntrt.origin, burn.origin, fence.origin)

da_min<-da %>% select(field, year, exp, disk, plot, subplot, ntrt,  species, mass.above) #72737  X 9### ##includes N cessation and burned plots###

da_orig<-merge(design.orig2, da_min, by =c("field", "year", "exp", "disk", "plot", "subplot", "ntrt")) # ##dim 50404    13### 

### da_orig is what we want to use moving foward...##

##### get counts by plot year to crosscheck with excel file "Datasubset_CC Convergence###


da_origsiteyear<-da_orig %>%  distinct(field, year, exp, disk, plot, subplot, ntrt, .keep_all = T)

plotcountcheck<-da_origsiteyear%>%group_by(exp, field, year) %>%  tally()

plotcountcheck2<-da_origsiteyear%>%group_by(exp, year) %>%  tally()


plotcountcheck19822004<-subset(plotcountcheck, year < 2005)

sum(plotcountcheck19822004$n) #  6102 total plots-years 1982 to 2004 #### matches exactly w/ excel file## :-) 

sum(plotcountcheck$n)  ## 6396 total plot  years to be analyzed with full times series (note that this includes the E/W subplots that are compiled later...#### 



################# TAXONOMIC AND OTHER SMALL DATA FIXES####################


# Capitalize species to get rid of capilization differences in spelling
da_orig$species <- toupper(as.character(da_orig$species))


###
da_orig$live <- 1
da_orig$sorted <- 1
da_orig$wood <- 0
da_orig$vasc <- 1

# Do some general substitutions

da_orig$species <- gsub("APOCYNUM CANNABINUM", "APOCYNUM ANDROSAEMIFOLIUM", da_orig$species) ## MD 11/1 based off email with Eric##
da_orig$species <- gsub("MISC. FORB", "MISCELLANEOUS FORB", da_orig$species)
da_orig$species <- gsub("SEDGES", "CAREX SP.", da_orig$species)
da_orig$species <- gsub("QUERCUS RUBRUM", "QUERCUS RUBRA", da_orig$species)

# Find litter and code as not alive
sel<-da_orig$species == 'MISCELLANEOUS LITTER'
da_orig$live[sel] <- 0

sel<-grep("PINE", da_orig$species)
da_orig$live[sel] <- 0

# Code unsorted material as not being sorted
sel<-grep("MISCELLANEOUS", da_orig$species)
da_orig$sorted[sel] <- 0

sel<-grep("FUNGI", da_orig$species)
da_orig$sorted[sel] <- 0

sel<-grep("MOSS", da_orig$species)
da_orig$sorted[sel] <- 0

sel<-grep("LICHEN", da_orig$species)
da_orig$sorted[sel] <- 0

# Woody stuff
sel<-grep("ACER NEGUNDO", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("CEANOTHUS", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("CORYLUS", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("PINUS", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("PINE", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("POPULUS", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("QUERCUS", da_orig$species)
da_orig$wood[sel] <- 1
sel<-grep("ULMUS", da_orig$species)
da_orig$wood[sel] <- 1


##mistake w/ biomass for Asclepias syriaca##
da_orig["mass.above"][da_orig["mass.above"] == 1711.970] <- 1711.970/10


# Read in species attribute look up table
sp.df <- read.csv(file.path(datain,'Cedar_Creek_Plant_Taxon_List.csv'), header=T)



names(sp.df)[] <- tolower(names(sp.df))

# sp.df<-plyr::rename(sp.df, c("ï..species"="species"))
sp.df$species <- toupper(sp.df$species) 
sp.df$functional.group <- toupper(sp.df$functional.group)
sp.df$duration <- toupper(sp.df$duration)
sp.df$lifeform <- toupper(sp.df$lifeform)
sp.df$pathway <- toupper(sp.df$pathway)
# sp.df$origin <- toupper(sp.df$origin) #this column is not present in any of the data published with the paper - either as in GitHub or Dryad
# so I am commenting out any mention to this column so the 
sp.df$family <- toupper(sp.df$family)

da_full <- merge(da_orig,sp.df, by='species', all.x=TRUE)

# Set Unknowns to missing
# da_full$origin[da_full$origin == 'UNKNOWN'] <- NA
# da_full$origin[da_full$origin == 'NATIVE AND/OR INTRODUCED'] <- NA


da_full$functional.group[da_full$functional.group == 'UNKNOW'] <- NA

da_full$duration[da_full$duration == 'UNKNOWN'] <- NA
da_full$duration[da_full$duration == 'BIENNIAL, PERENNIAL'] <- "BIENNIAL"

# Lump biennials in with annuals as they are pretty similar
# mostly weedy forbs 
unique(da_full$species[da_full$duration=="BIENNIAL"])
unique(da_full$species[da_full$duration=="BIENNIAL, PERENNIAL"])
unique(da_full$species[grep('BIENNIAL', da_full$duration)]) 

da_full$duration[grep('BIENNIAL', da_full$duration)] <- "ANNUAL"
unique(da_full$species[da_full$duration=="ANNUAL" & da_full$functional.group=="C4"])
unique(da_full$species[da_full$duration=="ANNUAL" & da_full$functional.group=="F"])

# Add in some species atrributes
sel <- da_full$species == "QUERCUS RUBRA"
da_full$functional.group[sel] <- "W"
da_full$lifeform[sel] <- "WOODY"
# da_full$origin[sel] <- "NATIVE"

# Add in some species atrributes
sel <- da_full$species == "RUDBEKIA SEROTINA"
da_full$functional.group[sel] <- "F"
da_full$lifeform[sel] <- "FORB"
# da_full$origin[sel] <- "NATIVE"

# Add in some species atrributes
sel <- da_full$species == "POA PRATENSIS"
# da_full$origin[sel] <- "INTRODUCED"

sel <- da_full$species == "MISCELLANEOUS CAREX SP."
da_full$functional.group[sel] <- "S"
da_full$lifeform[sel] <- "SEDGE"
# da_full$origin[sel] <- "NATIVE"


# Capitalize species to get rid of capilization differences in spelling
da_full$species <- toupper(as.character(da_full$species))

##deal with some entries where sp. were weighed twice##
summary(freq <- ddply(da_full[da_full$live==1 & da_full$sorted==1, ], .(year, field, exp, plot, subplot, disk, ntrt, species), colwise(length, .(mass.above)))) ##takes a while##

freq$freq <- freq$mass.above

freq$mass.above <- NULL
freq[freq$freq > 1,]

doubles.df <- merge(freq[freq$freq > 1,], da_full[c("field", "exp","plot", "year", "species", "mass.above")], by=c("field", "exp","plot", "year", "species"))
doubles.df[c("field", "exp","plot", "year", "species", "mass.above")]

#####
# There are a few cases where there are multiple species weighed per sample. 
# Options are taking max, min, mean, or sum. 

#da.mn <- ddply(da_full, .(field, exp, plot, subplot, year, disk, ntrt, nadd, species, live, sorted, wood, functional.group, lifeform, duration, origin), colwise(mean, .(mass.above)))
##takes a while##

# subset to live, sorted, herbaceous plants
d2 <- da_full[da_full$sorted ==1 & da_full$live ==1 & da_full$wood==0, c("field", "exp","plot", "subplot", "year", "disk", "ntrt",  "species", "mass.above")]

#subset that includes woody stuff####
d2woody <- da_full[da_full$sorted ==1 & da_full$live ==1, c("field", "exp","plot", "subplot", "year", "disk", "ntrt", "species", "mass.above")]


#Add some columns for herbaceous version####
d3<-d2 %>% mutate(expntrtfieldyear= paste(exp,field, ntrt, year, sep = '_'), expntrtyear= paste(exp,ntrt, year, sep = '_'), ntrt2=ntrt) 

#Add some columns for herbaceous + woody version ####
d3woody<-d2woody %>% mutate(expntrtfieldyear= paste(exp,field, ntrt, year, sep = '_'), expntrtyear= paste(exp,ntrt, year, sep = '_'), ntrt2=ntrt) 


##These data frames (d3 or d3woody) has fields A, B, C, experiment 1 (intact) & exp 2 (disked in 1982) in a long data dataformat ##9 levels of nutrient addition#### years 1982 - 2019 (not every field / exp sampeled in every year## 
#### Has both whole plots and suplots...###


# Transpose herbaceous and woody data to be a site by species matrix
da.widewoody <- reshape(d3woody,
                        v.names="mass.above",
                        idvar=c("field", "exp","plot", "subplot", "year", "disk", "ntrt", "expntrtyear"),
                        timevar="species",
                        direction="wide") 
# Fill in NA's with zeros in wide data set
da.widewoody[is.na(da.widewoody)] <- 0

##make nutrient and years factors#

da.widewoody$ntrt<-as.factor(da.widewoody$ntrt)
da.widewoody$year<-as.factor(da.widewoody$year)

########Combine east / west subplots into "whole" to match up with rest of data (from 2015)#######

##make only the biomasses the numeric variables##
da.widewoody$exp<-as.factor(da.widewoody$exp)
da.widewoody$disk<-as.factor(da.widewoody$disk)


#subset out the east/ west subplots plots and the whole plots ##

da.wideeastwest<-subset(da.widewoody, subplot=="East"|subplot=="West", row.names=NULL)
da.widewhole<-subset(da.widewoody, subplot=="Whole", row.names=NULL)

##Add the values from the subplots together##

EWaddall<-as.data.frame(da.wideeastwest%>%group_by(field,exp,plot,year,disk,ntrt,expntrtyear, expntrtfieldyear, ntrt2) %>% summarise_if(is.numeric,sum)%>% mutate(subplot="Whole"))


###merge the whole plots back together###
da.wide_allwhole<-rbind(da.widewhole,EWaddall)



dim(da.wide_allwhole) ## dimensions of the dataset = 6126  194##

##reorder columns so that all the rows are in the same order across fields etc. 
da.wide5<-da.wide_allwhole%>%arrange(year)%>%arrange(plot)%>%arrange(exp)%>%arrange(field) 


##### Check that the max number of rep within a treatment is 18, since 6 plots in each field and exp recieved the same N treatment##

max(ave(da.wide5$plot, da.wide5$expntrtyear,FUN = seq_along))


####check the plotyears####


plotcountcheck5<-da.wide5%>%group_by(exp, field, year) %>%  tally()

plotcountcheck2<-da_origsiteyear%>%group_by(exp, year) %>%  tally()


##This data frame (da.wide5) has fields A, B, C, experiment 1 (intact) & exp 2 (disked in 1982) ## years 1982 - 2019 for E002 and years 1982 - 2004 for exp E001 (not every field / exp sampeled in every year## INCLUDES WOODY PLANTS
###wide version format###

# N Treatments (Ntrt) Details (from Table 1 Tilman 1987)
# Ntrt Trt g N/m2/yr Other nutrients 
# 1     A       0.0      All 
# 2     B       1.0      All 
# 3     C       2.0      All 
# 4     D       3.4      All 
# 5     E       5.4      All 
# 6     F       9.5      All 
# 7     G      17.0      All 
# 8     H      27.2      All 
# 9     I       0.0      None 

##da.wide5 = fields A, B, C, E001 and E002 1982 to 2019 w/ some missing years###

###some notes about missing data##
## E002 missing data = 2003 field A & B, 2005 all fields , 2006 all fields , 2009 A and C, 2010 all fields, 2011 A, 2012 all fields, 213 A and C, 2014 all fields, 2015 A, 2016 A, 2017 all, ####


############ SUBSETTING FROM FULL DATASET FIELDS ABC E001 and E002########


##subset to years before 2005 for BOTH E001 and E002 ## (bc of change in fire regime)

da.wide5$year<-as.numeric(as.character(da.wide5$year)) 

exp12subset<-subset(da.wide5,year<2005)


#### SELECTING JUST FIELD D TO LOOK AT REMNANT VEGETATION (NEVER PLOWED)###

# Stack exp1 & 2 data
da <- rbind(d1a,d2a)

# Delete records with key missing data
da <- da[!is.na(da$mass),]

# Selecting just field D###

da_fieldD <- subset(da, field=="D")

####

# Capitalize species to get rid of capilization differences in spelling
da_fieldD$species <- toupper(as.character(da_fieldD$species))


# mabye we include woody for field D? not sure yet...###
da_fieldD$live <- 1
da_fieldD$sorted <- 1
da_fieldD$wood <- 0
da_fieldD$vasc <- 1

# Do some general substitutions

da_fieldD$species <- gsub("APOCYNUM CANNABINUM", "APOCYNUM ANDROSAEMIFOLIUM", da_fieldD$species) ## MD 11/1 based off email with Eric##
da_fieldD$species <- gsub("MISC. FORB", "MISCELLANEOUS FORB", da_fieldD$species)
da_fieldD$species <- gsub("SEDGES", "CAREX SP.", da_fieldD$species)
da_fieldD$species <- gsub("QUERCUS RUBRUM", "QUERCUS RUBRA", da_fieldD$species)

# Find litter and code as not alive
sel<-da_fieldD$species == 'MISCELLANEOUS LITTER'|da_fieldD$species == 'WOODY DEBRIS'

da_fieldD$live[sel] <- 0

sel<-grep("PINE", da_fieldD$species)
da_fieldD$live[sel] <- 0

# Code unsorted material as not being sorted
sel<-grep("MISCELLANEOUS", da_fieldD$species)
da_fieldD$sorted[sel] <- 0

sel<-grep("FUNGI", da_fieldD$species)
da_fieldD$sorted[sel] <- 0

sel<-grep("MOSS", da_fieldD$species)
da_fieldD$sorted[sel] <- 0

sel<-grep("LICHEN", da_fieldD$species)
da_fieldD$sorted[sel] <- 0

# Woody stuff
sel<-grep("ACER NEGUNDO", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("CEANOTHUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("CORYLUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("PINUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("PINE", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("POPULUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("QUERCUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1
sel<-grep("ULMUS", da_fieldD$species)
da_fieldD$wood[sel] <- 1


###remove the not live stuff...leave in trees ###

da_fieldD_2<-subset(da_fieldD, live==1)


### Without trees###

# subset to live, sorted, herbaceous plants
da_fieldD_3 <- da_fieldD[da_fieldD$sorted ==1 & da_fieldD$live ==1 & da_fieldD$wood==0, c("field", "plot", "year", "species", "mass.above")]


######### with trees####


# Transpose data to be a site by species matrix
da_fieldD_wide <- reshape(da_fieldD_2,
                          v.names="mass.above",
                          idvar=c("field", "plot", "year"),
                          timevar="species",
                          direction="wide") 

# Fill in NA's with zeros in wide data set
da_fieldD_wide[is.na(da_fieldD_wide)] <- 0

#Laís data checks
# added the lines of code below to check whether the number of plots still match: YES! IT ALL MATCHES!!
##### get counts by plot year to crosscheck with excel file "Datasubset_CC Convergence###

da_full <- da_full %>% as_tibble()

da_full %>%  
  distinct(field, year, exp, disk, plot, subplot, ntrt, .keep_all = T) %>%
  group_by(exp, field, year) %>%  tally() %>% ungroup() %>% summarise(n=sum(n)) ## 6396 total plot  years to be analyzed with full times series (note that this includes the E/W subplots that are compiled later...#### 

da_full %>%  
  distinct(field, year, exp, disk, plot, subplot, ntrt, .keep_all = T) %>%
  filter(year<2005) %>% 
  group_by(exp, field, year) %>%  tally() %>% ungroup() %>% summarise(n=sum(n)) # 6102 total plots-years 1982 to 2004 #### matches exactly w/ excel file## :-) 

#each species is associated with a single set of information on functional group, duration, lifeform, pathway and family: yes!
da_full %>% select(species,functional.group,duration,lifeform,pathway,family) %>% distinct() %>% group_by(species,functional.group,duration,lifeform,pathway,family) %>% add_count() %>% filter(n!=1)

# species
da_full %>% as_tibble() %>% filter(is.na(species)) #no NAs in the species column
length(unique(da_full$species)) #num of different 'species'

# field 
da_full %>% select(field) %>% distinct() #field D is in a different table [da_fieldD]

# year
da_full %>% select(year) %>% distinct() %>% arrange(year) # dataset from 1982 to 2019, but not all year within this range have data
###some notes about missing data##
## E002 missing data = 2003 field A & B, 2005 all fields , 2006 all fields , 2009 A and C, 2010 all fields, 2011 A, 2012 all fields, 2013 A and C, 2014 all fields, 2015 A, 2016 A, 2017 all, ####

# exp
da_full %>% select(exp) %>% distinct() # 1 = E001; 2=E002

# disk
da_full %>% select(disk) %>% distinct() # 0 = not disked (E001); 1 = disked (E002)

# plot
da_full %>% select(plot) %>% distinct() %>% arrange(plot) # 1 to 54, each field has a max of 54 plots followed over years

# subplot
da_full %>% select(subplot) %>% distinct() # whole, east and west. I couldn't find documentation, but here is what it potentially means:
da_full%>% select(subplot,year,field) %>% distinct() %>% arrange(year,field) # the categories East and West were only used in 2015 for some plots in fields B and C

# ntrt
da_full %>% select(ntrt) %>% distinct() %>% arrange(ntrt) # 1 to 9, refer to the treatments below:

# N Treatments (Ntrt) Details (from Table 1 Tilman 1987)
# Ntrt Trt g N/m2/yr Other nutrients 
# 1     A       0.0      All 
# 2     B       1.0      All 
# 3     C       2.0      All 
# 4     D       3.4      All 
# 5     E       5.4      All 
# 6     F       9.5      All 
# 7     G      17.0      All 
# 8     H      27.2      All 
# 9     I       0.0      None 

# other.add
da_full %>% select(other.add,ntrt) %>% distinct() # 0 or 1; this is a dummy variable for no nutrient addition or with nutrient addition

# ntrt.origin
da_full %>% select(ntrt.origin) %>% distinct() # only 1 in all rows, no NAs. Unsure what this one means, but I will drop as it does not add information
da_full %>% filter(is.na(ntrt.origin))

da_full <- da_full %>% select(-ntrt.origin) #dropping column

# burn.origin [try to find documentation of these treatments in the website to code column correctly]
da_full %>% select(field,burn.origin) %>% distinct() # 0 and 1, but their assignment does not make sense given description in the paper
da_full %>% filter(burn.origin==0) %>% select(field,year,exp,burn.origin) %>% distinct() %>% arrange(year)
#### Important note: burn.origin column isn't exactly right for B E002. The burn doesn't happen until 1992, so should be 54 plots from 1982-1991 and 27 plots 1992-2004###
## given that it seems this column is coded wrong (according to the description of the paper and Melissa's note here, I am deleting it)
da_full <- da_full %>% select(-burn.origin)

# fence.origin [try to find documentation of these treatments in the website to code column correctly]
da_full %>% select(exp,fence.origin) %>% distinct() # 1 or NA, 1 is associated with E001, and NA with E002. 
##But my understanding from the description in the paper is that regardless of exp type (E001 or E002) all was fenced before 2004.So, the way this column is likely coded is wrong.
# in general, burning and fencing are hard to understand when and where it started, stopped. I cannot find reliable information

# mass.above
da_full %>% select(mass.above) %>% summary() # high values are either litter or woody plants

# live
da_full %>% select(live) %>% distinct() # 0 or 1
da_full %>% select(live,species) %>% filter(live==0) %>%  distinct() #0 is litter or pine needles
da_full %>% select(live,species) %>% filter(live==1) %>%  distinct() #1 refers to species (at any id level)

# sorted
da_full %>% select(sorted) %>% distinct() # 0 or 1
da_full %>% select(species,sorted) %>% filter(sorted==0) %>% distinct() #0s are miscellaneous and plants not id to genus, 1 all other plants up to genus level

# wood
da_full %>% select(wood,functional.group) %>% distinct() # 0 not woody at all, #1 loose definition of woody stuff (pine needles [NA] and a grass are included [C4])

# vasc
da_full %>% select(vasc) %>% distinct() # all rows are #1 so column is not aggregating any different information, so I am deleting it
da_full <- da_full %>% select(-vasc)

#functional.group
da_full %>% select(functional.group) %>% distinct() # categories are W,F,C3,L,C4,S,O,G,NA,UNKNOWN (exploring them below to find which letter refers to what as there is no )
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="W") #woody species, genus or conglomerate of plants
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="F") #forbs
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="C3") #C3 grasses
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="C4") #C4 grasses
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="L") #legume
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="O") #horsetail, fungi, moss, lichen; but unsure why "O"
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="G") #Genus Panicum, as this genus have C3 and C4 types, so a generic generalization would be wrong, and grass seedlings and miscellaneous grasses
da_full %>% select(functional.group,species) %>% distinct() %>% filter(is.na(functional.group)) #miscellaneous litter and pine needles. The other miscellaneous cat I am changing to "F"

da_full <- da_full %>%
  mutate(functional.group = ifelse(grepl("^MISCELLANEOUS HERB", species),
    "F",
    functional.group
  ))

da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="S") #sedges
da_full %>% select(functional.group,species) %>% distinct() %>% filter(is.na(functional.group)) # fixed
da_full %>% select(functional.group,species) %>% distinct() %>% filter(functional.group=="UNKNOWN") #MISCELLANEOUS

## data check: is each category in the species column associated with a single functional group: yes!
da_full %>% select(functional.group,species) %>% distinct() %>% group_by(species) %>% count() %>% filter(n!=1)

# duration
da_full %>% select(duration) %>% distinct() # annual, perennial and NAs. Investigating NAs:
da_full %>% select(duration,species) %>% distinct() %>% filter(is.na(duration)) # some of the plants ID to the genus level only; and miscellaneous categories - looks good to me

## data check: is each category in the species column associated with a single duration: yes!
da_full %>% select(duration,species) %>% distinct() %>% group_by(species) %>% count() %>% filter(n!=1)

# lifeform
da_full %>% select(lifeform) %>% distinct() %>% arrange(lifeform) # forb, fungi, grass, horsetail, legume, moss & lichens, non-legume forbs, sedge, unknown  woody, NA. Let's investigate a few of these categories
da_full %>% select(lifeform, functional.group) %>% distinct() # does it make sense to have a non-legume forb, and a forb category? with respect to functional group, non-legume forb and forb are the same category "forb". I don't see why having the distinction, so I am fixing it
da_full <- da_full %>%
  mutate(lifeform = ifelse(lifeform=="NON-LEGUME FORB","FORB",lifeform))
da_full <- da_full %>%
  mutate(lifeform = ifelse(is.na(lifeform) & grepl("^MISCELLANEOUS HERB",species),"UNKNOWN",lifeform))
da_full %>% select(lifeform, species) %>% distinct() %>% filter(is.na(lifeform))
da_full %>% select(lifeform, species) %>% distinct() %>% filter(lifeform=="UNKNOWN") # OK, the UNKNOWN category makes sense

## data check: is each category in the species column associated with a single lifeform: yes!
da_full %>% select(lifeform, species) %>% distinct() %>% group_by(species) %>% count() %>% filter(n!=1)

# pathway
da_full %>% select(pathway) %>% distinct() #C3, C4, CX (??), UNKNOWN, NA. Let's explore these categories
da_full %>% select(functional.group,pathway) %>% distinct() %>% arrange(functional.group) # so not all known pathways are represented in the functional groups. I feel like we need to define properly the difference among pathway, lifeform, and functional group 
da_full %>% select(pathway,species) %>% distinct() %>% filter(pathway=="UNKNOWN"|is.na(pathway)) # they are mixed. fixing below
da_full <- da_full %>% #also fixing CX, which is unknown by nature. NAs are only left for non-applicable category
  mutate(pathway = ifelse(grepl("^MISCELLANEOUS HERB", species)|grepl("^MISCELLANEOUS C", species)|grepl("^CX", pathway),"UNKNOWN",pathway))
da_full %>% select(pathway,species) %>% distinct() %>% filter(pathway=="UNKNOWN"|is.na(pathway)) # Now NAs are only for categories that is truly missing data

# family
da_full %>% select(family) %>% distinct() # head looks ok - remember that there is one family that is UNKNOWN
da_full %>% select(family) %>% distinct() %>% tail() # tail also looks ok
da_full %>% select(species,family) %>% distinct() %>% filter(is.na(family)) # of, this make sense
da_full %>% select(species,family) %>% distinct() %>% filter(grepl("MISCELLANEOUS",species)) # these can be refined:
da_full <- da_full %>% 
  mutate(family = ifelse(species == "MISCELLANEOUS CAREX SP.","CYPERACEAE",
                         ifelse(species=="MISCELLANEOUS HERB 1"|species=="MISCELLANEOUS HERB 3","UNKNOWN",family)))

da_full %>% select(species,family) %>% distinct() %>% filter(grepl("MISCELLANEOUS",species)) #CHECKING IT AGAIN, fixed
da_full %>% select(species,family) %>% distinct() %>% filter(family=="UNKNOWN")

###adding provenance to each species in the dataset###

sp.df2 <- read_csv(file.path(datain,'Cedar_Creek_Plant_Taxon_List_Lauren.csv')) %>% 
  select(Species, Origin) %>% 
  distinct()

#only one species x origin combination?
sp.df2 %>% group_by(Species) %>% count() %>% filter(n!=1) #yes!

sp.df2$Species <- toupper(as.character(sp.df2$Species))
sp.df2 <- sp.df2 %>% 
  rename(species=Species,
         origin=Origin)
glimpse(sp.df2)

da_full <- da_full %>% 
  left_join(sp.df2) %>% 
  mutate(origin=ifelse(origin=="Native","N",
                       ifelse(origin=="Introduced","I",
                              ifelse(origin=="Unknown","UNK",
                                     ifelse(origin=="Native and/or Introduced","UNK",NA)))),
         origin=ifelse(species=="POA PRATENSIS", "I",
                       ifelse(species=="MISCELLANEOUS CAREX SP.","N",
                              ifelse(species=="MISCELLANEOUS HERB 1","UNK", 
                                     ifelse(species=="MISCELLANEOUS HERB 3","UNK",origin)))))
glimpse(da_full)

da_full %>% filter(is.na(origin)) %>% select(species,origin) %>% distinct()
da_full %>% filter(origin=="UNK") %>% select(species,origin) %>% distinct()
table(da_full$origin,useNA="ifany")
#categories: N=native, I=introduced, NI=native and/or introduced, UNK=unknown, NA=non-applicable [only for litter]

##### final dataset #####
# adjustment needed
# 1) if we decide to move forward with this is to clearly re-define categories across pathway, lifeform and functional.group
# 2) find reliable information about fencing and fire for all plots and fields and experiments
da_full %>% 
  select(family, everything()) %>% 
  glimpse() %>% 
  write_csv(file.path(dataout,"da_full_010924.csv"))
