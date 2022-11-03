### Re-checking of FIM oyster data 2022_06_13
#	Purpose is to update the measurements for the "plus counts"--oysters counted but not measured. These only occurred in some recent samples, 
#		but it is surprisingly annoying to fix, largely becuase older data (FDACS) did not use plus counts
#	Data must be broken into "Measurement" (shell heights, oysters measured), and "Counts" (total number counted, "num_live_quad")

### Code follows this outline:
#	1. Organize all (fdacs + fwc) MEASUREMENT data (these were previously concatenated by Ed)
#		a. fix time variables (month, season, etc.)
#		b. fix space variables (sites--combining sites!!)
#		c. create samplenumbers using a sitenumkey created
#		d. calculate numbers of oysters measured per size groups (0-25, 26-75, 75> used here, others can be created as needed)
#
#	2. Organize FWC COUNTS (SAMPLE) data
# 		a. fix time variables (month, season, etc.)
#		b. fix space variables (sites--combining sites!!)
#		c. create samplenumbers using the sitenumkey ALREADY created above (so samp numbers agree)
#
#	3. Merge back together
#		3.0. Data splitting--separate measurement data in fdacs and fwc
#		3.1. Aggregate counts per size group (rec sublegal, legal, props, totalcount) by sample
#		3.2. Merge FWC aggregated measurement per sample (3.1.) with FWC sample (count data, num_live_qud)
#		3.3. Separate FDACS into sample and measurement; Aggregate fdacs_dat (rec sublegal, legal, props, total count) by sample; merge with FDACS sample
#		3.4. Merging all data (fwc & fdacs) just an Rbind
#		3.5. Final adjustments--add in any droppped spatiotemporal variables, make any final site adjustments
#
#	4. Write out all data (output of 3.5. above) which now includes a single row for each quadrat
#
#	5. Examples of how to use data as output (or in this code) such to look at numbers per size group and numbers of quad per year as
#		a. bay-wide (all sites combined) per year
#		b. all reefs indvidually per year
#		c. single reef (e.g., cat point)
#
#	6. Plotting
#		a. Functions for plotting
#			i. plotting stuff
#			ii. neg binom stuff for calcing uncertainty
#		b. Plotting examples
#			i. assuming normal (arithmatic mean and sd)
#			ii. assuming neg binom dist (using fitdistr from package MASS)
#		


rm(list=ls());

##############################################################################
### DATA ###
##############################################################################
	### Read data in
	setwd("U:/UF/Projects/2021/FWC oyster project official/Data/FIM data/All oyster FIM FDACS FWC/Merging data into total files");
	dat <- read.csv("testing all data together.csv", stringsAsFactors=FALSE)                       #reading in measurement data previously combined as csv 
	dat_dav_c <- read.csv("fwc_davis_counts_checking.csv", stringsAsFactors=FALSE)                 #reading 
	dat_park_c <- read.csv("fwc_parker_counts_checking.csv", stringsAsFactors=FALSE)                       #reading in the raw files output from SAS 


############
###1.  MEASUREMENT DATA (HEIGHTS) ALL REGIMES ########################################
############ This us using dat--the measurements ###
		names(dat) <- tolower(names(dat))  #lower case

	### Time variables
		#Month
			dat$month <- as.character(dat$month)
			dat$monthf<- ifelse(nchar(dat$month)==2, dat$month, paste0("0",dat$month)) #add leading zeros for eventual sample numbers
		#Day
			dat$day <- as.character(dat$day)
			dat$dayf<- ifelse(nchar(dat$day)==2, dat$day, paste0("0",dat$day))  #add leading zeros for eventual sample numbers
		#Year
			dat$year <- as.character(dat$year)
		#Season, defind by Ed
			dat$season1 <- ifelse(dat$month %in% c("8", "9", "10", "11", "12", "1", "2"), "fall_winter", "spring_summer")
		# "Season year" so you can group the fall 200x samples with the jan/feb 200x+1 samples--e.g., want to be able to consider Dec 2012 & Jan 2013 samps together 
			dat$year_fall_seas <- ifelse(as.numeric(dat$month) <3, (as.numeric(dat$year)-1), dat$year) 


	### Space variables	
		## Site (Station) names
			dat$station_namef <- as.factor(dat$station_name) #change site to factor
	 			levels(dat$station_namef)  				#note many sites which might be combined
	 	## Combining site/station names
			dat$station_namef[which(dat$station_namef=="Bulkhead Plant")] <- "Bulkhead"
			dat$station_namef[which(dat$station_namef %in% c("Cabbage Lump Plant"))] <- "Cabbage Lumps"
			dat$station_namef[which(dat$station_namef %in% c("Cat Point2", "Cat PointPlant", "Cat PointRelay", "Cat PointSummer"))] <- "Cat Point"
			dat$station_namef[which(dat$station_namef %in% c("Dry Bar #10", "Dry Bar North", "Dry Bar Plant"))] <- "Dry Bar"
			dat$station_namef[which(dat$station_namef=="East Hole #69")] <- "Easthole #69"
			dat$station_namef[which(dat$station_namef=="East Hole #7")] <- "Easthole #7"
			dat$station_namef[which(dat$station_namef %in% c("Easthole #12", "Easthole #8", "Easthole Plant", "Easthole Summer"))] <- "Easthole"
			#dat$station_namef[which(dat$station_namef %in% c("Easthole #69", "Easthole #7"))] <- "Easthole"  #can't do this yet becuase it screws with station
			dat$station_namef[which(dat$station_namef=="East Lumps ")] <- "East Lumps"
			dat$station_namef[which(dat$station_namef %in% c("Green Point Plant", "Green Pt"))] <- "Green Point"
			dat$station_namef[which(dat$station_namef=="Hotel Plant")] <- "Hotel"
			dat$station_namef[which(dat$station_namef=="Lighthouse Plant")] <- "Lighthouse"
			dat$station_namef[which(dat$station_namef %in% c("Norman North", "Norman Plant", "Norman South"))] <- "Normans"
			dat$station_namef[which(dat$station_namef=="North Spur Plant")] <- "North Spur"
				dat$station_namef <- as.factor(as.character(dat$station_namef))
				levels(dat$station_namef)  				#note many sites which might be combined
				length(levels(dat$station_namef)) 		#yep so way less than 100 so we'll only have one leading zero

			#dealing with station number mistake found previously
				vv <- dat[which(dat$year > 2015 & dat$station_name=="Easthole #69"& dat$station==12),]   #Easthole #69 is supposed to be station 12.5, but some entered as 12--this gives those
				dat$station[which(dat$station_namef=="Easthole #69" & dat$station==12)] <- 12.5			#turning them back to 12.5
				vv <- dat[which(dat$station_name=="Easthole #69"& dat$station==12),] 					#checking


	### Creating sample number
		## Site number
			dat$sitenum <- as.character(as.numeric(dat$station_namef))  #created char from number list
			dat$sitenum<- ifelse(nchar(dat$sitenum)==2, dat$sitenum, paste0("0",dat$sitenum)) #add leading zeros
		##site sitenum key--can be used to look up what site num is a particular site, if we wanted it. 
			sitenumkey <- unique(cbind(station_namef=as.character(dat$station_namef), sitenum=dat$sitenum))
			sitenumkeydf <- data.frame(sitenumkey)  #create a dataframe for later use3
		## Quadrat
			dat$quadrat <- as.character(dat$quadrat)  #as character
			dat$quadratf<- ifelse(nchar(dat$quadrat)==2, dat$quadrat, paste0("0",dat$quadrat))  #add leading zeros
		## Sample numbers
			#sampnumnosite--probably don't need site if you're subsetting by sites
				dat$sampns <- paste(dat$year, dat$monthf, dat$dayf, dat$quadratf, sep="_")
			#sampnumsite--probably don't need site if you're subsetting by sites
				dat$samps <- paste(dat$year, dat$monthf, dat$dayf, dat$sitenum, dat$quadratf, sep="_")
			#sampnum_station_site--including station, though it's not clear what station always means/whether it matters
				dat$sampss <- paste(dat$year, dat$monthf, dat$dayf, dat$sitenum, dat$station, dat$quadratf, sep="_")
					length(unique(dat$samps))   #fewer samples 
					length(unique(dat$sampss))  #more samples


	### Oyster counts
		## Counts regardless of length
			dat$count <- 1  #create new column of ones for every measurment
			dat$count[which(is.na(dat$sh_new))] = 0 #turn the NA's to zeros, becuase I don't know what the NA's mean other than there were no oysters found
			#checks. Math--is it real?
				tot_oysters <- sum(dat$count)   						#total oysters measures (i.e. no NA)
				tot_records <- length(dat$count)						#length of count column
				na_records <- length(which(is.na(dat$sh_new)))  #how many NA records
				na_records + tot_oysters - tot_records  #this should be zero

		## Counts by shell height
			#first just look at the distribution
				hist(dat$sh_new, xlab="Shell height (mm)", main="Histogram of shell heights, all areas") 

			#manually check for odd things
				#NA's in shell height?
					length(which(is.na(dat$sh_new))) 					#meh over 1000 NA
					length(which(is.na(dat$sh_new)))/dim(dat)[1]  		#but really small ratio of total data (<1%)
				#zeros?
					length(which(dat$sh_new==0)) 							#cool only 1 zero. don't know if real or mistake
				#max shell height?
					max(dat$sh_new, na.rm=T)    							#170. that's huge
					length(which(dat$sh_new > 150)) 						#only 2 over 150
					length(which(dat$sh_new > 120)) 						#...and 65 over 120
					length(which(dat$sh_new > 110)) 						#224 over 110. Maybe have size groups up to 111 then a plus group

		##Creating size-based groups
			#recruits or "spat"
				dat$rec <- 0
				dat$rec[which(dat$sh_new < 26)] = 1  #This should be a flag to let you count "recruits" or "settled" or whatever small size class
			#post-recruits or non-spat
				dat$postrec <- 0
				dat$postrec[which(dat$sh_new >= 26)] = 1
			#sublegal or "seed"
				dat$sublegal <- 0
				dat$sublegal[which(dat$sh_new >= 26 & dat$sh_new < 76)] = 1  #This should be a flag to let you count "recruits" or "settled" or whatever small size class	
			#legal or "market"
				dat$legal <- 0
				dat$legal[which(dat$sh_new >= 76)] = 1  #This should be a flag to let you count "recruits" or "settled" or whatever small size class

			# #5mm size group binning -- again, many better ways to do this, and I've already shown I can make mistakes doing things manually, but here is one way for 5mm groups
			# 	dat$sg25 <- 0; dat$sg25[which(dat$sh_new >= 25 & dat$sh_new < 31)] = 1
			# 	dat$sg31 <- 0; dat$sg31[which(dat$sh_new >= 31 & dat$sh_new < 36)] = 1
			# 	dat$sg36 <- 0; dat$sg36[which(dat$sh_new >= 36 & dat$sh_new < 41)] = 1
			# 	dat$sg41 <- 0; dat$sg41[which(dat$sh_new >= 41 & dat$sh_new < 46)] = 1
			# 	dat$sg46 <- 0; dat$sg46[which(dat$sh_new >= 46 & dat$sh_new < 51)] = 1
			# 	dat$sg51 <- 0; dat$sg51[which(dat$sh_new >= 51 & dat$sh_new < 56)] = 1
			# 	dat$sg56 <- 0; dat$sg56[which(dat$sh_new >= 56 & dat$sh_new < 61)] = 1
			# 	dat$sg61 <- 0; dat$sg61[which(dat$sh_new >= 61 & dat$sh_new < 66)] = 1
			# 	dat$sg66 <- 0; dat$sg66[which(dat$sh_new >= 66 & dat$sh_new < 71)] = 1
			# 	dat$sg71 <- 0; dat$sg71[which(dat$sh_new >= 71 & dat$sh_new < 76)] = 1
			# 	dat$sg76 <- 0; dat$sg76[which(dat$sh_new >= 76 & dat$sh_new < 81)] = 1
			# 	dat$sg81 <- 0; dat$sg81[which(dat$sh_new >= 81 & dat$sh_new < 86)] = 1
			# 	dat$sg86 <- 0; dat$sg86[which(dat$sh_new >= 86 & dat$sh_new < 91)] = 1
			# 	dat$sg91 <- 0; dat$sg91[which(dat$sh_new >= 91 & dat$sh_new < 96)] = 1
			# 	dat$sg96 <- 0; dat$sg96[which(dat$sh_new >= 96 & dat$sh_new < 101)] = 1
			# 	dat$sg101 <- 0; dat$sg101[which(dat$sh_new >= 101 & dat$sh_new < 106)] = 1
			# 	dat$sg106 <- 0; dat$sg106[which(dat$sh_new >= 106 & dat$sh_new < 111)] = 1
			# 	dat$sg111 <- 0; dat$sg101[which(dat$sh_new >= 111 )] = 1

			#10mm size group binning -- Fine to use, just didn't assume we'd start with it. But it's here if we need/want it
				# dat$sg_31 <- 0; dat$sg_31[which(dat$sh_new >= 31 & dat$sh_new < 41)] = 1
				# dat$sg_41 <- 0; dat$sg_41[which(dat$sh_new >= 41 & dat$sh_new < 51)] = 1
				# dat$sg_51 <- 0; dat$sg_51[which(dat$sh_new >= 51 & dat$sh_new < 61)] = 1
				# dat$sg_61 <- 0; dat$sg_61[which(dat$sh_new >= 61 & dat$sh_new < 71)] = 1
				# dat$sg_71 <- 0; dat$sg_71[which(dat$sh_new >= 71 & dat$sh_new < 81)] = 1
				# dat$sg_81 <- 0; dat$sg_81[which(dat$sh_new >= 81 & dat$sh_new < 91)] = 1
				# dat$sg_91 <- 0; dat$sg_91[which(dat$sh_new >= 91 & dat$sh_new < 101)] = 1
				# dat$sg_101 <- 0; dat$sg_101[which(dat$sh_new >= 101 & dat$sh_new < 111)] = 1
				# dat$sg_111 <- 0; dat$sg_101[which(dat$sh_new >= 111 )] = 1




##########
### 2.  SAMPLE DATA (counts not heights) ########################################
##########
		### merge Sample fwc count files
			dat2 <- rbind(dat_dav_c,dat_park_c)


		### Sample Temporal variables
			#Month
				dat2$month <- as.character(dat2$month)
				dat2$monthf<- ifelse(nchar(dat2$month)==2, dat2$month, paste0("0",dat2$month))
			#Day
				dat2$day <- as.character(dat2$day)
				dat2$dayf<- ifelse(nchar(dat2$day)==2, dat2$day, paste0("0",dat2$day))
			#Year
				dat2$year <- as.character(dat2$year)
			#Season
				dat2$season1 <- ifelse(dat2$month %in% c("8", "9", "10", "11", "12", "1", "2"), "fall_winter", "spring_summer")
			# "Season year" so you can group the fall 200x samples with the jan/feb 200x+1 samples--e.g., want to be able to consider Dec 2012 & Jan 2013 samps together 
				dat2$year_fall_seas <- ifelse(as.numeric(dat2$month) <3, (as.numeric(dat2$year)-1), dat2$year) 

		### Sample Spatial variables
			## Site (Station) names
				dat2$station_namef <- as.character(dat2$station_name)
					levels(as.factor(dat2$station_namef))				#note many sites which might be combined
				#Change questionable ones
					dat2$station_namef[which(dat2$station_namef=="Cat Point 2")] <- "Cat Point"
					dat2$station_namef[which(dat2$station_namef=="Dry Bar North")] <- "Dry Bar"
				#Check
					dat2$station_namef <- as.factor(as.character(dat2$station_namef))
					levels(dat2$station_namef)  


		### Giving appropriate site number key
			## merging with sitenumkeydf
				dat3 <- merge(x=sitenumkeydf, y=dat2, by.x="station_namef", by.y="station_namef", all.x=FALSE, all.y=TRUE, sort=FALSE)
				dat3$sitenum<- ifelse(nchar(dat3$sitenum)==2, dat3$sitenum, paste0("0",dat3$sitenum))

				#Checks
					head(dat3)
					sitenumkey2 <- data.frame(unique(cbind(station_namef=as.character(dat3$station_namef), sitenum=dat3$sitenum)))
					#checking inclusion between sets
						xy <- setdiff(sitenumkeydf$station_namef, sitenumkey2$station_namef)  #these are the sites in all measurement not in fwc sample, expected to be > 0
						yx <- setdiff(sitenumkey2$station_namef, sitenumkeydf$station_namef)  #these are the sites in fwc sample not in all measurement !!needs to be 0!!
					#checking the numbers match
						sitenumkey2 <- sitenumkey2[order(sitenumkey2$station_namef),]  #ordering the sample sitenumkey
						xx <- sitenumkeydf[which(sitenumkeydf$station_namef %in% sitenumkey2$station_namef),]  #get measurement stations in the sample df
						xx <- xx[order(xx$station_namef),]  	#order them
						sitenumkey2$checkname <- xx$station_namef
						sitenumkey2$checknum <- xx$sitenum
						sitenumkey2

		### Saptial conintued
			#quadrat as character and with leading zeros
				dat3$quadrat <- as.character(dat3$quadrat)
				dat3$quadratf<- ifelse(nchar(dat3$quadrat)==2, dat3$quadrat, paste0("0",dat3$quadrat))

		### Creating sampling numbers
			#sampnumnosite--probably don't need site if you're subsetting by sites
				dat3$sampns <- paste(dat3$year, dat3$monthf, dat3$dayf, dat3$quadratf, sep="_")
			#sampnumsite--probably don't need site if you're subsetting by sites
				dat3$samps <- paste(dat3$year, dat3$monthf, dat3$dayf, dat3$sitenum, dat3$quadratf, sep="_")
			#check on whether station matters
				dat3$sampss <- paste(dat3$year, dat3$monthf, dat3$dayf, dat3$sitenum, dat3$station, dat3$quadratf, sep="_")
					length(unique(dat3$samps))
					length(unique(dat3$sampss))




#########################################
### 3.  MERGING DATA BACK TOGETHER ###
#########################################
	# create fwc_count_dat (sample not height specific data including total counted, which may be more than those measured)
	# create fwc_height_dat (subset of entire height database that includes parker and davis FWC data)
	# create fdacs_dat (subset of entire height database that includes fdacs and transition data)

	# 1. aggregate fwc_height_dat (rec sublegal, legal, props, totalcount)
	# 2. merge agg_fwc_height_dat with fwc_count_dat (now one row per sample including num_live_quad)
	# 3. aggregate fdacs_dat (rec sublegal, legal, props, total count)
	# 4. add new columns to fdacs_dat (props, num_live_quad--calc props but not num_live_quad)


	### 0. Data splitting
		###  Creating fwc_count_dat
			dat4 <- dat3[,c("year", "monthf", "dayf", "station_namef", "sitenum", "quadratf", "samps", "sampss", "station", "season", "season1", "year_fall_seas", "num_live_quad")]

		###  Create fwc_height and fdacs_height dat
			dat_fwc <- dat[which(dat$regime %in% c("fwc_davis", "fwc_parker")),]  #fwc heights data
					dat_fwc$station_namef <- as.factor(as.character(dat_fwc$station_namef)) #make sure station_namef classed right
					levels(dat_fwc$station_namef)
			dat_fdacs <- dat[which(dat$regime %in% c("fdacs", "transition")),]


	### 1. aggregate fwc_height_dat (rec sublegal, legal, props, totalcount)
		#oysters per sample by life history group
			osl_fwc <- aggregate(list(rec=dat_fwc$rec, sublegal=dat_fwc$sublegal, legal=dat_fwc$legal), 
				by=list(sampss=dat_fwc$sampss), FUN=sum)
			#checking math to make sure no oysters disappear when binning in size groups
				oysters_samp <- aggregate(list(count=dat_fwc$count), by=list(samp=dat_fwc$sampss), FUN=sum) #total counts per sample
				xx<- rowSums(osl_fwc[,c("rec", "sublegal", "legal")])	#sum of size groups
				xx-oysters_samp$count #should be nothing but zeros

			#Proportions of oysters measured per size group
					osl_fwc$totcount=rowSums(osl_fwc[,c("rec", "sublegal", "legal")])
					osl_fwc$prop_rec <- ifelse(osl_fwc$totcount==0,0, osl_fwc$rec/osl_fwc$totcount)
					osl_fwc$prop_sublegal <- ifelse(osl_fwc$totcount==0,0, osl_fwc$sublegal/osl_fwc$totcount)
					osl_fwc$prop_legal <- ifelse(osl_fwc$totcount==0,0, osl_fwc$legal/osl_fwc$totcount)
					head(osl_fwc)


	### 2. Merge FWC measuresment with FWC sample (count data) (now one row per sample including num_live_quad)
		#checks
			dim(osl_fwc); dim(dat4)  #check on dims
			xx <- setdiff(dat4$sampss, osl_fwc$sampss)  #something's screwed up with this sample, not sure what
			#fixes--remove sample found above
				dat5 <- dat4[-which(dat4$sampss==xx[1]),]
					dim(osl_fwc); dim(dat5)
					xx <- setdiff(dat5$sampss, osl_fwc$sampss);xx  ##This should be zero, otherwise you have a lot of samples that won't match
		#merge
			all_fwc <- merge(x=dat5, y=osl_fwc, by.x='sampss', by.y='sampss', all.x=TRUE, all.y=TRUE, sort=TRUE)


	### 3. aggregate fdacs_dat (rec sublegal, legal, props, total count)
		## shortening sample data
			dat_fdacs_samp <- unique(dat_fdacs[,c("year", "monthf", "dayf", "station_namef", "sitenum", "quadratf", "sampss", "samps", "station", "season", "season1", "year_fall_seas")])

		## aggregating by life history group
			osl_fdacs <- aggregate(list(num_live_quad=dat_fdacs$count, rec=dat_fdacs$rec, sublegal=dat_fdacs$sublegal, legal=dat_fdacs$legal), 
				by=list(sampss=dat_fdacs$sampss), FUN=sum)
			#checking math
				oysters_samp <- aggregate(list(count=dat_fdacs$count), by=list(samp=dat_fdacs$sampss), FUN=sum)
				xx<- rowSums(osl_fdacs[,c("rec", "sublegal", "legal")])
				xx-oysters_samp$count  #should be all zeros

		## doing props of measures oysters
				osl_fdacs$totcount=rowSums(osl_fdacs[,c("rec", "sublegal", "legal")])
				osl_fdacs$prop_rec <- ifelse(osl_fdacs$totcount==0,0, osl_fdacs$rec/osl_fdacs$totcount)
				osl_fdacs$prop_sublegal <- ifelse(osl_fdacs$totcount==0,0, osl_fdacs$sublegal/osl_fdacs$totcount)
				osl_fdacs$prop_legal <- ifelse(osl_fdacs$totcount==0,0, osl_fdacs$legal/osl_fdacs$totcount)
				head(osl_fdacs)

		## merge fdacs samp and fdacs oyster measurements
			#checks
			dim(osl_fdacs); dim(dat_fdacs_samp)    		#should be same number of rows
			#merge
			all_fdacs <- merge(x=dat_fdacs_samp, y=osl_fdacs, by.x='sampss', by.y='sampss', all.x=TRUE, all.y=TRUE, sort=TRUE)


	### 4. Merging all data (fwc & fdacs)
			dim(all_fdacs); dim(all_fwc)  #this must be the same
			colnames(all_fdacs); colnames(all_fwc) #so must these
		## Rbind for merge
			all <- rbind(all_fdacs, all_fwc)
				all$num_live_quad[which(all$num_live_quad=="Z")] <- "0"  	#somehow there are still Z's, turn them to 0's
		## Addressing differences between number measured and number counted
			all$num_live_quad <- as.numeric(all$num_live_quad)				#needs to be numeric for math
			all$diff <- all$num_live_quad-all$totcount 						#this are the differences between total measured and "num_live_quad"

			#identify rows to change
				diffs <- all[which(all$diff>0),] 	#these are all dataframe were more counted than measured
				diffrows <- which(all$diff>0) 		#these are the rows
				length(rownames(diffs))
				rowpoints <- as.numeric(rownames(diffs))  #this is what we actually used--the rows that need to be fixed

			#changing the counts per life history group ***I'm positive this could be done better, I was tired but it works
				#matrix for the numbers that need to be added--need a place holder but probably could have created again for each row
				adds=matrix(nrow=length(rowpoints), ncol=3)
				#loop to do it. Uses to loops becuase they're counting differently: i loop tracks row nums (not consecutive), j loop tracks numbers to be added
				for(i in rowpoints){
					for(j in 1:length(rowpoints)){
						adds[j,] <- round(rowMeans(rmultinom(100,size=all$diff[i],prob=c(all$prop_rec[i], all$prop_sublegal[i], all$prop_legal[i]))),0)
						}
					all$rec[i]=all$rec[i]+adds[j,1]; all$sublegal[i]=all$sublegal[i]+adds[j,2]; all$legal[i]=all$legal[i]+adds[j,3]
					}

			#checking
				all$totcount2 <- rowSums(all[,c("rec", "sublegal", "legal")])
				all$diff2 <- all$num_live_quad-all$totcount2
				all$diff2


	### 5. Final adjustments
		## Period got dropped, add back in 
			perdf <- expand.grid(year=seq(min(all$year), max(all$year), 1), season=c("spring_summer","fall_winter"))
			perdf1 <- perdf[order(perdf$year, perdf$season),]
			perdf1$period <- 1:length(perdf1$season)
			perdf1$yr_seas <- paste(perdf1$year, perdf1$season, sep="_")
			perdf2 <- perdf1[,c("period", "yr_seas")]
			all$yr_seas <- paste(all$year_fall_seas, all$season1, sep="_")

			#merging with the period stuff
			all1 <- merge(x=all, y=perdf2, by.x="yr_seas", by.y="yr_seas", all.x=TRUE, all.y=FALSE, sort=FALSE)

		## Change the name of easthole sites for compatability !!don't do this above becuase it screws up sampss!!
			all1$station_namef[which(all1$station_namef %in% c("Easthole #69", "Easthole #7"))] <- "Easthole"
				all1$station_namef <- as.factor(as.character(all1$station_namef))
				levels(all1$station_namef)


		## Checks
			#Looking at quads per sample year, Cat Point
				cp <- all[which(all1$station_namef=="Cat Point"),]
					length(unique(cp$sampss));
				cpq <- aggregate(list(quad=cp$sampss), by=list(year=cp$year_fall_seas), function(x) length(unique(x))) 

			#rename for writing
			all <- all1


##################################
### 4. WRITING OUTPUT FILES ####
##################################

	## Writing data out
			# setwd("U:/UF/Projects/2021/FWC oyster project official/Data/FIM data/All oyster FIM FDACS FWC/Merging data into total files"); 
			# 	write.csv(all, file = "oysters_all_lhg_season_update_v1.csv", row.names=FALSE)


##################################
### 5. EXAMPLES USING OUTPUT ####
##################################

	### Bay-wide (all bars combined), only fall/winter sampling
		## subset for only fall_winter samples
			fw <- all1[which(all1$season1=="fall_winter"),]
		
		## total number per size group per fall_year, along with quads
			opy <- aggregate(list(rec=fw$rec, sublegal=fw$sublegal, legal=fw$legal), 
					by=list(year=fw$year_fall_seas), FUN=sum)
			qpy <- aggregate(list(quads=fw$sampss), by=list(sampss=fw$year_fall_seas), FUN=length)

			opy$quads <- qpy$quads
			head(opy)


	### Reef-specific (all bars individual) only fall/winter sampling
		## total number per size group per fall_year, along with quads
			opyr <- aggregate(list(rec=fw$rec, sublegal=fw$sublegal, legal=fw$legal), 
					by=list(year=fw$year_fall_seas, site=fw$station_namef), FUN=sum)
			qpyr <- aggregate(list(quads=fw$sampss), by=list(sampss=fw$year_fall_seas, site=fw$station_namef), FUN=length)

			opyr$quads <- qpyr$quads
			head(opyr)

		## Subset above for specific reef
			cp <- opyr[which(opyr$site=="Cat Point"),]



	### Single reef only, fall-winter sampling only
		## subset for only fall_winter samples
			fw_cp <- all1[which(all1$season1=="fall_winter" & all1$station_namef=="Cat Point"),]
		
		## total number per size group per fall_year, along with quads
			opy_cp <- aggregate(list(rec=fw_cp$rec, sublegal=fw_cp$sublegal, legal=fw_cp$legal), 
					by=list(year=fw_cp$year_fall_seas), FUN=sum)
			qpy_cp <- aggregate(list(quads=fw_cp$sampss), by=list(sampss=fw_cp$year_fall_seas), FUN=length)

			opy_cp$quads <- qpy_cp$quads
			head(opy_cp)

		## Little check to make sure all works as expected
			opy_cp$quads-cp$quads  #this should be zeros IF using cat point for both


	### all reefs, all seasons, by month and year --this shoudl be what Gabby was using for input
		## total number per size group per fall_year, along with quads
			omyr <- aggregate(list(rec=all$rec, sublegal=all$sublegal, legal=all$legal), 
					by=list(month=all$monthf, year=all$year, site=all$station_namef), FUN=sum)
			qmyr <- aggregate(list(quads=all$sampss), by=list(year=all$year, month=all$monthf, site=all$station_namef), FUN=length)

			omyr$quads <- qmyr$quads
			omyr

		## Writing data out
				# setwd("U:/UF/Projects/2021/FWC oyster project official/Data/FIM data/All oyster FIM FDACS FWC/Merging data into total files"); 
				# 	write.csv(omyr, file = "oysters_month_year_site.csv", row.names=FALSE)


##################################
### 6. PLOTTING ####
##################################			
		
	### Functions
		#function to return mean & sd for individual reefs NOTE these are the **arithmatic** mean and sd assuming normal, which these aren't
			resfun <- function(dat, bar, season){
					pd <- dat[which(dat$station_namef==bar & dat$season1==season),]
				pd_mean <- aggregate(list(mrec=pd$rec, msublegal=pd$sublegal, mlegal=pd$legal), 
						by=list(year=pd$year_fall_seas), FUN=mean)
				pd_sd <- aggregate(list(sdrec=pd$rec, sdsublegal=pd$sublegal, sdlegal=pd$legal), 
						by=list(year=pd$year_fall_seas), FUN=sd)
				pd_res <- cbind(pd_mean, pd_sd[,2:4])
				return(pd_res)
			}

		#function to return mean & sd for ALL reefs NOTE these are the **arithmatic** mean and sd assuming normal, which these aren't
			resfun_all <- function(dat, season){
					pd <- dat[which(dat$season1==season),]
				pd_mean <- aggregate(list(mrec=pd$rec, msublegal=pd$sublegal, mlegal=pd$legal), 
						by=list(year=pd$year_fall_seas), FUN=mean)
				pd_sd <- aggregate(list(sdrec=pd$rec, sdsublegal=pd$sublegal, sdlegal=pd$legal), 
						by=list(year=pd$year_fall_seas), FUN=sd)
				pd_res <- cbind(pd_mean, pd_sd[,2:4])
				return(pd_res)
			}

		#
			# 3-panel plots for different modes, areas
				plotfun3 <- function (results, mean, sd, main){
						lcl <- mean-1.96*sd
				  		ucl <- mean+1.96*sd
				  			x = results$year
				  			y = mean 
				  			ly = lcl
				  			uy = ucl 
				  			ymax <- 1.25*max(y)
							ymin <- 0
							### Transparency function
								col2rgbA<-function(color,transparency) {
								  rgb(t(col2rgb(color))/255,alpha=transparency)
								}

								matplot(x=x,y=cbind(y, ly, uy),type="n", xaxt="n", yaxt="n", 
									xlab="Year",ylab="",col="black", ylim=c(ymin,ymax))
								axis(side=1, at=seq(min(x), max(x), by=1))
								axis(side=2, at=pretty(seq(ymin, ymax, by=(ymax-ymin)/6)), labels=formatC(pretty(seq(ymin, ymax, by=(ymax-ymin)/6)), big.mark=",", format="d"), las=1)
								mtext("Counts per quad", side=2, line=3, adj=.5, outer=FALSE)
								# polygon(x=c(x,rev(x)),y=c(ly, rev(uy)), col=col2rgbA("lightblue", .5), border = "lightblue")
								polygon(x=c(x,rev(x)),y=c(ly, rev(uy)), col="lightblue", border = "lightblue")
								points(x, y, type="b", lwd=2, col="dodgerblue3")
								legend("topright", legend=c("Annual mean", "95% Conf. int."), col=c("dodgerblue3", col2rgbA("lightblue", .5)), lwd=c(2, 8), lty=c(1,1), bty="n")
								text(median(as.numeric(x)), 0.94*ymax, paste(main, sep=""), cex=2) #this works but puts a stupid space in where you don't need it. 
								#mtext(expression(italic("Source: NMFS MRIP Database")), side=1, line=3, adj=1, outer=FALSE)
				}



			nb_mean <- function(x) {
				fitdistr(x, "Negative Binomial")$estimate[2]
			}


			nb_sd <- function(x) {
				mu <- fitdistr(x, "Negative Binomial")$estimate[2]  #this is the mu parm or neg binom mean
				size <- fitdistr(x, "Negative Binomial")$estimate[1]  #this is the mu parm or neg binom mean
				sqrt(mu+(mu^2/size))
			}


			nb_resfun <- function(dat, bar, season){
 				# bar="Cat Point"; season="fall_winter"; 
				pd <- dat[which(dat$station_namef==bar & dat$season1==season),]
				years <- levels(as.factor(pd$year_fall_seas))
					mat <- matrix(nrow=length(years), ncol=6)
					for( i in 1:length(years)){
						tryCatch({
							xx <- pd[which(pd$year_fall_seas==years[i]),]
							mat[i,1]=nb_mean(xx$rec)
							mat[i,2]=nb_sd(xx$rec)
							mat[i,3]=nb_mean(xx$sublegal)
							mat[i,4]=nb_sd(xx$sublegal)
							mat[i,5]=nb_mean(xx$legal)
							mat[i,6]=nb_sd(xx$legal)
						}, error=function(e){})
					}
					mat[which(is.na(mat))]=0
					df <- data.frame(mat);
					colnames(df) <- c("nbmu_rec", "nbsd_rec", "nbmu_sublegal", "nbsd_sublegal", "nbmu_legal", "nbsd_legal")
					df$year <- years
					df
				}


			nb_resfun_all <- function(dat, season){
 				# bar="Cat Point"; season="fall_winter"; 
				pd <- dat[which(dat$season1==season),]
				years <- levels(as.factor(pd$year_fall_seas))
					mat <- matrix(nrow=length(years), ncol=6)
					for( i in 1:length(years)){
						tryCatch({
							xx <- pd[which(pd$year_fall_seas==years[i]),]
							mat[i,1]=nb_mean(xx$rec)
							mat[i,2]=nb_sd(xx$rec)
							mat[i,3]=nb_mean(xx$sublegal)
							mat[i,4]=nb_sd(xx$sublegal)
							mat[i,5]=nb_mean(xx$legal)
							mat[i,6]=nb_sd(xx$legal)
						}, error=function(e){})
					}
					mat[which(is.na(mat))]=0
					df <- data.frame(mat);
					colnames(df) <- c("nbmu_rec", "nbsd_rec", "nbmu_sublegal", "nbsd_sublegal", "nbmu_legal", "nbsd_legal")
					df$year <- years
					df
				}

			# resfun_all <- function(dat, season){
			# 		pd <- dat[which(dat$season1==season),]
			# 	pd_mean <- aggregate(list(mrec=pd$rec, msublegal=pd$sublegal, mlegal=pd$legal), 
			# 			by=list(year=pd$year), FUN=mean)
			# 	pd_sd <- aggregate(list(sdrec=pd$rec, sdsublegal=pd$sublegal, sdlegal=pd$legal), 
			# 			by=list(year=pd$year), FUN=sd)
			# 	pd_res <- cbind(pd_mean, pd_sd[,2:4])
			# 	return(pd_res)
			# }

	### Plots with normal mean/sd ###
		### Cat Point
		bar="Cat Point"
		pd_res <- resfun(dat=all, bar=bar, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=pd_res, mean=pd_res$mrec, sd=pd_res$sdrec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=pd_res, mean=pd_res$msublegal, sd=pd_res$sdsublegal, main="Sublegal (25-75mm")
			plotfun3(results=pd_res, mean=pd_res$mlegal, sd=pd_res$sdlegal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)

		### Dry Bar
		pd_res <- resfun(dat=all, bar="Dry Bar", season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=pd_res, mean=pd_res$mrec, sd=pd_res$sdrec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.("Dry Bar"))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=pd_res, mean=pd_res$msublegal, sd=pd_res$sdsublegal, main="Sublegal (25-75mm")
			plotfun3(results=pd_res, mean=pd_res$mlegal, sd=pd_res$sdlegal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)

		### All
		pd_res <- resfun_all(dat=all, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=pd_res, mean=pd_res$mrec, sd=pd_res$sdrec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.("All AB sites"))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=pd_res, mean=pd_res$msublegal, sd=pd_res$sdsublegal, main="Sublegal (25-75mm")
			plotfun3(results=pd_res, mean=pd_res$mlegal, sd=pd_res$sdlegal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)





############# negbinom fun/scrap ##########################
#install.packages("MASS")
library(MASS)
	#Cat point
		bar="Cat Point"
		nbres <- nb_resfun(dat=all, bar=bar, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)

	#Dry Bar
		bar="Dry Bar"
		nbres <- nb_resfun(dat=all, bar=bar, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)


	#Green Point
		bar="Green Point"
		nbres <- nb_resfun(dat=all, bar=bar, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)

	#Porters
		bar="Porters"
		nbres <- nb_resfun(dat=all, bar=bar, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)

	#All
		bar="All Bars"
		nbres <- nb_resfun_all(dat=all, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)


	#Easthole !!! Note, combinding easthole #69 & easthole #7
	# all$station_namef[which(all$station_namef %in% c("Easthole #69", "Easthole #7"))] <- "Easthole"
		bar="Easthole"
		nbres <- nb_resfun_all(dat=all, season="fall_winter")
		par(mfrow=c(3,1), mgp = c(2.5, .5, 0), mar=c(4,3,2,2), oma=c(.5,2,.5,1))
			plotfun3(results=nbres, mean=nbres$nbmu_rec, sd=nbres$nbsd_rec, main="Recruits/spat (<25mm)")
				mtext(bquote(italic(.(bar))), side=3, line=0, adj=0, cex=1.5, outer=FALSE)
			plotfun3(results=nbres, mean=nbres$nbmu_sublegal, sd=nbres$nbsd_sublegal, main="Sublegal (25-75mm")
			plotfun3(results=nbres, mean=nbres$nbmu_legal, sd=nbres$nbsd_legal, main="Legal (>75mm")
				mtext(expression(italic("Source: FWRI")), side=1, line=3, adj=1, outer=FALSE)


