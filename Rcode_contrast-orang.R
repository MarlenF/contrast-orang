rm(list=ls())
if("openxlsx" %in% rownames(installed.packages()) == FALSE) { install.packages("openxlsx") } ; library(openxlsx)
if("lme4" %in% rownames(installed.packages()) == FALSE) { install.packages("lme4") } ; library(lme4)
if("lmerTest" %in% rownames(installed.packages()) == FALSE) { install.packages("lmerTest") } ; library(lmerTest)
if("car" %in% rownames(installed.packages()) == FALSE) { install.packages("car") } ; library(car)
r <- 1


# ---------------------------------------------------------------------------------------------------------------------------------
# (1) Define path, read data and define species to analyze ------------------------------------------------------------------------
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"
#path <- "H:\\Knief\\Documents\\CoAuthoredManuscripts\\OrangUtan_Infants_RepertoireSimilarityPlasticity\\data\\"
test.data <- read.table(paste0(path,"orangutanR_inteng_repair.csv"), header=TRUE, sep=",")
infant.age.dat <- read.xlsx(xlsxFile=paste0(path,"Orang_infant_age.xlsx"), sheet=1, colNames=FALSE) ; colnames(infant.age.dat) <- c("animal_id","ageYears")

# Set Species to "B"=Bornean, "S"=Sumatran or "BS"=Bornean+Sumatran
Species <- "BS"
# Set Group to "IF"=Infants --> Mothers or "IO"=Infants --> Others
Group <- "IF"
# Set comparisons
Comp <- "WB" # "WB"=within-between, "WC"=wild-captive
# Specify whether you want to perform matrix permutations
MatrixPerm <- TRUE

# ---------------------------------------------------------------------------------------------------------------------------------
# (2) Subset data frame to the specified group ------------------------------------------------------------------------------------
if(Group=="IF") { dat <- subset(test.data, kinship=='motoff' & agediff=='older') }
if(Group=="IO") { dat <- subset(test.data, kinship!='motoff' & age!='Ad') }
if(Group=="IP") { dat <- subset(test.data, kinship!='motoff' & age!='Ad' & agediff!='older')}
if(Group=="IA") { dat <- subset(test.data, kinship!='motoff' & age!='Ad' & agediff=='older')}


colnames(dat)[which(colnames(dat)=="ID_sign")] <- "animal_id"
colnames(dat)[which(colnames(dat)=="Signal")] <- "Behavior2"
dat <- as.data.frame(na.omit(dat[ ,c("Context","Behavior2","animal_id","species","group","setting","ID_obs")]))

# ---------------------------------------------------------------------------------------------------------------------------------
# (3) Omit all levels of animal_id that contributed fewer than 30 cases -----------------------------------------------------------
NID <- data.frame(table(dat$animal_id)) ; colnames(NID) <- c("animal_id","NID")
NID$animal_id <- as.character(NID$animal_id)
NID30 <- subset(NID, NID>=30)
dat <- subset(dat, animal_id %in% NID30$animal_id)

# ---------------------------------------------------------------------------------------------------------------------------------
# (4) Subset data frame to the specified species ----------------------------------------------------------------------------------
if(Species=="B") { dat <- subset(dat, species=="Bor") }
if(Species=="S") { dat <- subset(dat, species=="Sum") }
if(Species=="BS") { dat <- dat }

# ---------------------------------------------------------------------------------------------------------------------------------
# (5) Create a table for the set of behaviors (Behavior2) used by every individual (animal_ID) AT LEAST TWICE ---------------------
## create data frame in which every combination of individual and behaviour is counted
dat.ind.bhv <- data.frame(table(dat$animal_id,dat$Behavior2)) ; colnames(dat.ind.bhv) <- c("animal_id","Behavior2","N")
dat.ind.con <- data.frame(table(dat$animal_id,dat$Context)) ; colnames(dat.ind.con) <- c("animal_id","Context","N")
## subset dat.ind.bhv to include only rows where N>=2
dat.ind.bhv2 <- subset(dat.ind.bhv, N>=2)
dat.ind.con2 <- subset(dat.ind.con, N>=1)
## create data frame with the number of behaviours for each individual
dat.ind.bhv2.m <- data.frame(table(dat.ind.bhv2$animal_id)) ; colnames(dat.ind.bhv2.m) <- c("animal_id","NRep")
dat.ind.con2.m <- data.frame(table(dat.ind.con2$animal_id)) ; colnames(dat.ind.con2.m) <- c("animal_id","NCon")
## add predictor variables to dat.ind.bhv2.m
test.data.dup <- test.data[!duplicated(test.data$animal_id),c("animal_id","setting","species","sex","age", "group")]
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, test.data.dup, by.x="animal_id", by.y="animal_id")
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, NID, by.x="animal_id", by.y="animal_id")
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, dat.ind.con2.m, by.x="animal_id", by.y="animal_id")
if(Group=="IF") { dat.ind.bhv2.m$partner <- "Mother" }
if(Group=="IO") { dat.ind.bhv2.m$partner <- "Others" }
if(Group=="IP") { dat.ind.bhv2.m$partner <- "Peers" }
if(Group=="IA") { dat.ind.bhv2.m$partner <- "Older" }


## merge infant age to table
dat.ind.bhv2.m <- merge(dat.ind.bhv2.m, infant.age.dat, by.x="animal_id", by.y="animal_id", sort=FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------
# (6) Calculate Dice coefficient based on formula: dc = (2 x number of behaviours two inds have in common)/(R of ind1 + R ind2) ---
## create data frame with animal_id as rows and columns
dat.ind.ind <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.ind.ind) <- dat.ind.bhv2.m$animal_id
rownames(dat.ind.ind) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
	for(j in 1:(nrow(dat.ind.bhv2.m))) {
		Ind1 <- subset(dat.ind.bhv2, animal_id==dat.ind.bhv2.m$animal_id[i])
		Ind2 <- subset(dat.ind.bhv2, animal_id==dat.ind.bhv2.m$animal_id[j])
		ovrlp <- length(which(Ind1$Behavior2 %in% Ind2$Behavior2))
		RInd1 <- subset(dat.ind.bhv2.m, animal_id==dat.ind.bhv2.m$animal_id[i])$NRep	# which is the same as nrow(Ind1)
		RInd2 <- subset(dat.ind.bhv2.m, animal_id==dat.ind.bhv2.m$animal_id[j])$NRep	# which is the same as nrow(Ind2)
		dat.ind.ind[which(rownames(dat.ind.ind)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.ind.ind)==dat.ind.bhv2.m$animal_id[j])] <- (2 * ovrlp) / (RInd1 + RInd2)
	}
}
## set diagonal to NA
Dice <- as.matrix(dat.ind.ind)
diag(Dice) <- NA
#Dice <- as.data.frame(Dice)

# ---------------------------------------------------------------------------------------------------------------------------------
# (7-A) Define within and between setting dyads -------------------------------------------------------------------------------------
if(Comp=="WB") {
## create data frame with animal_id as rows and columns
dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
	for(j in 1:(nrow(dat.ind.bhv2.m))) {
		Ind1 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[i])
		Ind2 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[j])
		if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
		if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
		if(Ind1$setting[1]==Ind2$setting[1]) { wth.btw <- "W" } else { wth.btw <- "B" }
		dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
	}
}

## set diagonal to NA
wth.btw <- as.matrix(dat.wth.btw)
diag(wth.btw) <- NA
#wth.btw <- as.data.frame(wth.btw)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (7-B) Define within and between setting dyads ---
if(Comp=="WC") {
## create data frame with animal_id as rows and columns
dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
## loop over all pairs of individuals
for(i in 1:(nrow(dat.ind.bhv2.m))) {
  for(j in 1:(nrow(dat.ind.bhv2.m))) {
    wth.btw <- NA
    Ind1 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[i])
    Ind2 <- subset(dat, animal_id==dat.ind.bhv2.m$animal_id[j])
    if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
    if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
    if(Ind1$setting[1]=="wild" & Ind2$setting[1]=="wild") { wth.btw <- "W" }
    if(Ind1$setting[1]=="captive" & Ind2$setting[1]=="captive") { wth.btw <- "C" }
    dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
  }
}
## set diagonal to NA
wth.btw <- as.matrix(dat.wth.btw)
diag(wth.btw) <- NA
#wth.btw <- as.data.frame(wth.btw)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (8-A) Select within and between group dyads from the Dice data frame --------------------------------------------------------------
if(Comp=="WB") {
Dice.Within <- ifelse(wth.btw=="W",Dice,NA)
Dice.Between <- ifelse(wth.btw=="B",Dice,NA)

Emp.MDiceW <- mean(Dice.Within[lower.tri(Dice.Within, diag = FALSE)],na.rm=TRUE)
Emp.MDiceB <- mean(Dice.Between[lower.tri(Dice.Between, diag = FALSE)],na.rm=TRUE)
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (8-B) Select within and between group dyads from the Dice data frame --------------------------------------------------------------
if(Comp=="WC") {
Dice.Wild <- ifelse(wth.btw=="W",Dice,NA)
Dice.Captive <- ifelse(wth.btw=="C",Dice,NA)

(Emp.MDiceWild <- mean(Dice.Wild[lower.tri(Dice.Wild, diag = FALSE)],na.rm=TRUE))
(Emp.MDiceCaptive <- mean(Dice.Captive[lower.tri(Dice.Captive, diag = FALSE)],na.rm=TRUE))
}

# Bornean N = 4 (C), N = 8 (W)
# Bornean IF within-captive: 0.69
# Bornean IF within-wild: 0.68
#

# Sumatran N = 9
# Sumatran moms within-captive:  0.54
# Sumatran moms within-wild: 0.74


# ---------------------------------------------------------------------------------------------------------------------------------
# (9-A) Create data frame dd with columns Ind1, Ind2, Dice, Within/Between --------------------------------------------------------
if(Comp=="WB") {
yW <- expand.grid(rownames(Dice.Within), colnames(Dice.Within))
labs <- yW[as.vector(upper.tri(Dice.Within, diag = FALSE)), ]
yW <- cbind(labs, Dice.Within[upper.tri(Dice.Within,diag=FALSE)])
colnames(yW) <- c("Ind1","Ind2","Dice")
yW <- yW[!is.na(yW$Dice), ]
yW$WB <- rep("Within",nrow(yW))

yB <- expand.grid(rownames(Dice.Between), colnames(Dice.Between))
labs <- yB[as.vector(upper.tri(Dice.Between, diag = FALSE)), ]
yB <- cbind(labs, Dice.Between[upper.tri(Dice.Between,diag=FALSE)])
colnames(yB) <- c("Ind1","Ind2","Dice")
yB <- yB[!is.na(yB$Dice), ]
yB$WB <- rep("Between",nrow(yB))
dd <- rbind(yW,yB)
dat.sub <- dat[,c("animal_id","species","group","setting")]
dat.sub <- dat.sub[!duplicated(dat.sub), ]
dd <- merge(dd, dat.sub, by.x="Ind1", by.y="animal_id", sort=FALSE, all.y=FALSE)
dd <- merge(dd, dat.sub, by.x="Ind2", by.y="animal_id", sort=FALSE)
colnames(dd)[which(colnames(dd)=="species.x")] <- "species.ind1"
colnames(dd)[which(colnames(dd)=="group.x")] <- "group.ind1"
colnames(dd)[which(colnames(dd)=="setting.x")] <- "setting.ind1"
colnames(dd)[which(colnames(dd)=="species.y")] <- "species.ind2"
colnames(dd)[which(colnames(dd)=="group.y")] <- "group.ind2"
colnames(dd)[which(colnames(dd)=="setting.y")] <- "setting.ind2"
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (9-B) Create data frame dd with columns Ind1, Ind2, Dice, Wild/Captive ----------------------------------------------------------
if(Comp=="WC") {
yW <- expand.grid(rownames(Dice.Wild), colnames(Dice.Wild))
labs <- yW[as.vector(upper.tri(Dice.Wild, diag = FALSE)), ]
yW <- cbind(labs, Dice.Wild[upper.tri(Dice.Wild,diag=FALSE)])
colnames(yW) <- c("Ind1","Ind2","Dice")
yW <- yW[!is.na(yW$Dice), ]
yW$WB <- rep("Wild",nrow(yW))

yC <- expand.grid(rownames(Dice.Captive), colnames(Dice.Captive))
labs <- yC[as.vector(upper.tri(Dice.Captive, diag = FALSE)), ]
yC <- cbind(labs, Dice.Captive[upper.tri(Dice.Captive,diag=FALSE)])
colnames(yC) <- c("Ind1","Ind2","Dice")
yC <- yC[!is.na(yC$Dice), ]
yC$WB <- rep("Captive",nrow(yC))
dd <- rbind(yW,yC)
dat.sub <- dat[,c("animal_id","species","group","setting")]
dat.sub <- dat.sub[!duplicated(dat.sub), ]
dd <- merge(dd, dat.sub, by.x="Ind1", by.y="animal_id", sort=FALSE, all.y=FALSE)
dd <- merge(dd, dat.sub, by.x="Ind2", by.y="animal_id", sort=FALSE)
colnames(dd)[which(colnames(dd)=="species.x")] <- "species.ind1"
colnames(dd)[which(colnames(dd)=="group.x")] <- "group.ind1"
colnames(dd)[which(colnames(dd)=="setting.x")] <- "setting.ind1"
colnames(dd)[which(colnames(dd)=="species.y")] <- "species.ind2"
colnames(dd)[which(colnames(dd)=="group.y")] <- "group.ind2"
colnames(dd)[which(colnames(dd)=="setting.y")] <- "setting.ind2"
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (10-A) Perform matrix permutation test --------------------------------------------------------------------------------------------
if(Comp=="WB" & MatrixPerm == TRUE) {
	dat.inc <- subset(dat, animal_id %in% dat.ind.bhv2.m$animal_id)
	ind.setting <- unique(dat.inc[,c("animal_id","group","species","setting")])
	ind.setting$species_setting <- paste(ind.setting$species, ind.setting$setting, sep="_")
	
	nPerm <- 1000
	dat.Perm <- c()
	for(k in 1:nPerm) {
		## permute setting in dat
		ind.setting.perm <- ind.setting
		ind.setting.perm$setting <- sample(ind.setting.perm$setting, nrow(ind.setting.perm), replace=FALSE)
		dat.inc.perm <- dat.inc[,-which(colnames(dat.inc)=="setting")]
		dat.inc.perm <- merge(dat.inc.perm, ind.setting.perm, by.x="animal_id", by.y="animal_id", sort=FALSE)
		## repeat step 5 & 6
		## create data frame with animal_id as rows and columns
		dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
		colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		## loop over all pairs of individuals
		for(i in 1:(nrow(dat.ind.bhv2.m))) {
			for(j in 1:(nrow(dat.ind.bhv2.m))) {
				Ind1 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[i])
				Ind2 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[j])
				if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
				if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
				if(Ind1$setting[1]==Ind2$setting[1]) { wth.btw <- "W" } else { wth.btw <- "B" }
				dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
			}
		}
		## set diagonal to NA
		wth.btw <- as.matrix(dat.wth.btw)
		diag(wth.btw) <- NA
		Dice.Within <- ifelse(wth.btw=="W",Dice,NA)
		Dice.Between <- ifelse(wth.btw=="B",Dice,NA)
		MDiceW <- mean(Dice.Within[lower.tri(Dice.Within, diag = FALSE)],na.rm=TRUE)
		MDiceB <- mean(Dice.Between[lower.tri(Dice.Between, diag = FALSE)],na.rm=TRUE)
		dat.Perm[k] <- MDiceW - MDiceB
		flush.console()
		if(k %% 10 == 0) { print(paste0("Finished ", k, " out of ", nPerm, " simulations")) }
	}
	
	hist(dat.Perm)
	abline(v=Emp.MDiceW - Emp.MDiceB, col="red")
	Pval.WB <- 1 - sum(dat.Perm<=(Emp.MDiceW - Emp.MDiceB)) / length(dat.Perm)	# This is the P-value
}
# Significance thresholds are P>=0.975 and P<=0.025. 
# This is because we are looking at the deviation from 0, which can either be negative or positive.
# Because the distribution of differences is not necessarily symmetric around zero, we cannot calculate P-values using absolute values.

# ---------------------------------------------------------------------------------------------------------------------------------
# (10-B) Perform matrix permutation test --------------------------------------------------------------------------------------------
if(Comp=="WC" & MatrixPerm == TRUE) {
	dat.inc <- subset(dat, animal_id %in% dat.ind.bhv2.m$animal_id)
	ind.setting <- unique(dat.inc[,c("animal_id","group","species","setting")])
	ind.setting$species_setting <- paste(ind.setting$species, ind.setting$setting, sep="_")
	
	nPerm <- 1000
	dat.Perm <- c()
	for(k in 1:nPerm) {
		## permute setting in dat
		ind.setting.perm <- ind.setting
		ind.setting.perm$setting <- sample(ind.setting.perm$setting, nrow(ind.setting.perm), replace=FALSE)
		dat.inc.perm <- dat.inc[,-which(colnames(dat.inc)=="setting")]
		dat.inc.perm <- merge(dat.inc.perm, ind.setting.perm, by.x="animal_id", by.y="animal_id", sort=FALSE)
		## repeat step 5 & 6
		## create data frame with animal_id as rows and columns
		dat.wth.btw <- data.frame(matrix(rep(NA, nrow(dat.ind.bhv2.m) * nrow(dat.ind.bhv2.m)), ncol=nrow(dat.ind.bhv2.m)))
		colnames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		rownames(dat.wth.btw) <- dat.ind.bhv2.m$animal_id
		## loop over all pairs of individuals
		for(i in 1:(nrow(dat.ind.bhv2.m))) {
			for(j in 1:(nrow(dat.ind.bhv2.m))) {
				wth.btw <- NA
				Ind1 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[i])
				Ind2 <- subset(dat.inc.perm, animal_id==dat.ind.bhv2.m$animal_id[j])
				if(length(table(Ind1$setting))>1) { print("Ohoh, better check my data again!") }
				if(length(table(Ind2$setting))>1) { print("Ohoh, better check my data again!") }
				if(Ind1$setting[1]=="wild" & Ind2$setting[1]=="wild") { wth.btw <- "W" }
				if(Ind1$setting[1]=="captive" & Ind2$setting[1]=="captive") { wth.btw <- "C" }
				dat.wth.btw[which(rownames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[i]),which(colnames(dat.wth.btw)==dat.ind.bhv2.m$animal_id[j])] <- wth.btw
			}
		}
		## set diagonal to NA
		wth.btw <- as.matrix(dat.wth.btw)
		diag(wth.btw) <- NA
		Dice.Wild <- ifelse(wth.btw=="W",Dice,NA)
		Dice.Captive <- ifelse(wth.btw=="C",Dice,NA)
		MDiceW <- mean(Dice.Wild[lower.tri(Dice.Wild, diag = FALSE)],na.rm=TRUE)
		MDiceC <- mean(Dice.Captive[lower.tri(Dice.Captive, diag = FALSE)],na.rm=TRUE)
		dat.Perm[k] <- MDiceW - MDiceC
		flush.console()
		if(k %% 10 == 0) { print(paste0("Finished ", k, " out of ", nPerm, " simulations")) }
	}

	hist(dat.Perm)
	abline(v=Emp.MDiceWild - Emp.MDiceCaptive, col="red")
	Pval.WC <- 1 - sum(dat.Perm<=(Emp.MDiceWild - Emp.MDiceCaptive)) / length(dat.Perm)	# This is the P-value
}
# Significance thresholds are P>=0.975 and P<=0.025. 
# This is because we are looking at the deviation from 0, which can either be negative or positive.
# Because the distribution of differences is not necessarily symmetric around zero, we cannot calculate P-values using absolute values.

# ---------------------------------------------------------------------------------------------------------------------------------
# (11-A) Print out empirical Dice coefficients and P-values of the matrix permutation tests -----------------------------------------
if(Comp=="WB") {
## sample sizes
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }

## empirical Dice within
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Within = ",Emp.MDiceW)) }

## empirical Dice between
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Between = ",Emp.MDiceB)) }

## P-values of the matrix permutation tests
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WB)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WB)) }

if(r==1) { outResults <- data.frame(matrix(rep(NA,6), ncol=6)) ; colnames(outResults) <- c("Species","Group","N","Emp.MDiceW","Emp.MDiceB","Pval.WB") }
outResults[r,"Species"] <- Species
outResults[r,"Group"] <- Group
outResults[r,"N"] <- nrow(dat.ind.bhv2.m)
outResults[r,"Emp.MDiceW"] <- Emp.MDiceW
outResults[r,"Emp.MDiceB"] <- Emp.MDiceB
outResults[r,"Pval.WB"] <- Pval.WB
r <- r + 1

#   Species Group  N Emp.MDiceW Emp.MDiceB Pval.WB
# 1       B    IF 12  0.6858453  0.5750977    0
# 2       S    IF 13  0.5565279  0.4036022    0
# 3       B    IO  9  0.8267473  0.3609093    0
# 4       S    IO 12  0.7190610  0.5772387    0
}

# ---------------------------------------------------------------------------------------------------------------------------------
# (11-B) Print out empirical Dice coefficients and P-values of the matrix permutation tests -----------------------------------------
if(Comp=="WC") {
## sample sizes
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Sample size N = ",nrow(dat.ind.bhv2.m))) }

## empirical Dice captive
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Captive = ",Emp.MDiceCaptive)) }

## empirical Dice wild
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. Empirical Dice Wild = ",Emp.MDiceWild)) }

## P-values of the matrix permutation tests
if(Species == "BS" & Group == "IF") { print(paste0("Borneans + Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "B" & Group == "IF") { print(paste0("Borneans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "S" & Group == "IF") { print(paste0("Sumatrans. Infants --> Mothers. P-value matrix permutation = ",Pval.WC)) }
if(Species == "BS" & Group == "IO") { print(paste0("Borneans + Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }
if(Species == "B" & Group == "IO") { print(paste0("Borneans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }
if(Species == "S" & Group == "IO") { print(paste0("Sumatrans. Infants --> Others. P-value matrix permutation = ",Pval.WC)) }

if(r==1) { outResults <- data.frame(matrix(rep(NA,6), ncol=6)) ; colnames(outResults) <- c("Species","Group","N","Emp.MDiceCaptive","Emp.MDiceWild","Pval.WC") }
outResults[r,"Species"] <- Species
outResults[r,"Group"] <- Group
outResults[r,"N"] <- nrow(dat.ind.bhv2.m)
outResults[r,"Emp.MDiceCaptive"] <- Emp.MDiceCaptive
outResults[r,"Emp.MDiceWild"] <- Emp.MDiceWild
outResults[r,"Pval.WC"] <- Pval.WC
r <- r + 1

#   Species Group  N Emp.MDiceCaptive Emp.MDiceWild Pval.WC
# 1       B    IF 12        0.6817849     0.6867154    0.46
# 2       B    IO  9        0.8289495     0.8230769   0.473
# 3       S    IF 13        0.3749052     0.6862583   0.006
# 4       S    IO 12        0.7138462     0.7300121   0.420
}





# ---------------------------------------------------------------------------------------------------------------------------------
# (12) Store and merge relevant data frames ---------------------------------------------------------------------------------------
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IF"
if(Species == "BS" & Group == "IF") { dat.ind.mot <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IO"
if(Species == "BS" & Group == "IO") { dat.ind.oth <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IP"
if(Species == "BS" & Group == "IP") { dat.ind.pee <- dat.ind.bhv2.m }
## Keep dat.ind.bhv2.m for Species <- "BS" and Group <- "IA"
if(Species == "BS" & Group == "IA") { dat.ind.old <- dat.ind.bhv2.m }


## Merge dat.ind.mot and dat.ind.oth
dat.ind <- rbind(dat.ind.mot,dat.ind.oth)


# Model 1 Repertoire size
setwd("C:/Users/Marlen Fröhlich/Documents/R/roger/")
source("diagnostic_fcns.r"); contr=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

dat.ind.mot$z.age=as.vector(scale(dat.ind.mot$ageYears))	
dat.ind.mot$z.nid=as.vector(scale(dat.ind.mot$NID))
dat.ind.mot$obs.level.re=as.factor(1:nrow(dat.ind.mot))

dat.ind.oth$z.age=as.vector(scale(dat.ind.oth$ageYears))	
dat.ind.oth$z.nid=as.vector(scale(dat.ind.oth$NID))
dat.ind.oth$obs.level.re=as.factor(1:nrow(dat.ind.oth))

dat.ind.pee$z.age=as.vector(scale(dat.ind.pee$ageYears))	
dat.ind.pee$z.nid=as.vector(scale(dat.ind.pee$NID))
dat.ind.pee$obs.level.re=as.factor(1:nrow(dat.ind.pee))

dat.ind.old$z.age=as.vector(scale(dat.ind.old$ageYears))	
dat.ind.old$z.nid=as.vector(scale(dat.ind.old$NID))
dat.ind.old$obs.level.re=as.factor(1:nrow(dat.ind.old))


#### Model 1a: Repertoire size w mothers
#Check collinarity: max VIF = 1.8
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid, data=dat.ind.mot)
library(car)
vif(xres)

mod.NRep.m <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid + 
              (1|group), family = poisson, data = dat.ind.mot, control = contr)
null.NRep.m <- glmer(formula = NRep ~  sex + z.nid + 
                       (1|group), family = poisson, data = dat.ind.mot, control = contr)

summary(mod.NRep.m)
overdisp.test(mod.NRep.m) # dispersion parameter 0.84 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.m)) #25
length(residuals(mod.NRep.m)) #25

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.m, mod.NRep.m, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NRep.m    4 136.9638 141.8393 -64.48191 128.9638       NA NA         NA
#mod.NRep.m     8 141.7896 151.5407 -62.89482 125.7896 3.174179  4  0.5291115


### Model 1b: Repertoire size w others

#Check collinarity: max VIF = 3.1
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid, data=dat.ind.oth)
library(car)
vif(xres)

mod.NRep.o <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex +setting * species + z.nid +
                    (1|group), family = poisson, data = dat.ind.oth, control = contr)
null.NRep.o <- glmer(formula = NRep ~  sex + z.nid +
                      (1|group), family = poisson, data = dat.ind.oth, control = contr)


#Check for overdispersion:
overdisp.test(mod.NRep.o) # dispersion parameter 0.77 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.o), coefficients(mod.NRep.o)+
        t(apply(X=dfbeta(mod.NRep.o), MARGIN=2, FUN=range)))

summary(mod.NRep.o)
#(Intercept)  2.67922    0.15903  16.847  < 2e-16 ***
#z.age        0.09229    0.07328   1.259 0.207878    
#(z.age^2)  -0.14958    0.09049  -1.653 0.098324 .  
#sexM         0.23766    0.11773   2.019 0.043519 *  
#settingwild -0.57042    0.16885  -3.378 0.000729 ***
#speciesSum   0.54394    0.18193   2.990 0.002791 ** 
#z.nid        0.26054    0.09136   2.852 0.004347 ** 


# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.o)) #21
length(residuals(mod.NRep.o)) #21

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.o, mod.NRep.o, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df  Pr(>Chisq)
#null.NRep.o    4 130.2518 134.4299 -61.12589 122.2518       NA NA          NA
#mod.NRep.o     8 122.3404 130.6965 -53.17018 106.3404 15.91141  4 0.003140367


#get chi square and p-values (all)
drop1(mod.NRep.o,  test ="Chisq")
#z.age         1 121.92  1.5843 0.208139   
#I(z.age^2)    1 123.09  2.7457 0.097516 . 
#sex           1 124.30  3.9605 0.046580 * 
#setting       1 130.63 10.2878 0.001339 **
#species       1 127.90  7.5641 0.005954 **
# z.nid         1 125.61  5.2657 0.021750 * 


### Model 1c: Repertoire size w peers

#Check collinarity: max VIF = 3.3
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid, data=dat.ind.pee)
library(car)
vif(xres)

mod.NRep.p <- glmer(formula = NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid +
                      (1|group), family = poisson, data = dat.ind.pee, control = contr)
null.NRep.p <- glmer(formula = NRep ~  sex + z.nid +
                       (1|group), family = poisson, data = dat.ind.pee, control = contr)


#Check for overdispersion:
overdisp.test(mod.NRep.p) # dispersion parameter 0.76 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.p), coefficients(mod.NRep.p)+
        t(apply(X=dfbeta(mod.NRep.p), MARGIN=2, FUN=range)))


summary(mod.NRep.p)
#(Intercept)  2.64697    0.21862  12.108   <2e-16 ***
#z.age        0.05811    0.09170   0.634   0.5263    
#I(z.age^2)  -0.17694    0.10845  -1.632   0.1028    
#sexM         0.08118    0.17461   0.465   0.6420    
#settingwild -0.46358    0.20809  -2.228   0.0259 *  
#speciesSum   0.35134    0.26509   1.325   0.1851    
#z.nid        0.25056    0.13001   1.927   0.0539 .


# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.p)) #15
length(residuals(mod.NRep.p)) #15

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NRep.p, mod.NRep.p, test="Chisq"))
npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
null.NRep.p    4 84.94564 87.77784 -38.47282 76.94564       NA NA         NA
mod.NRep.p     8 86.47238 92.13678 -35.23619 70.47238 6.473261  4  0.1664829

#get chi square and p-values (all)
drop1(mod.NRep.p,  test ="Chisq")
#z.age         1 84.877 0.4048 0.52461  
#I(z.age^2)    1 86.945 2.4726 0.11585  
#sex           1 84.687 0.2142 0.64349  
#setting       1 88.318 3.8459 0.04987 *
#species       1 86.154 1.6815 0.19472  
#z.nid         1 87.646 3.1737 0.07483 .

### Model 1d: Repertoire size w olders

#Check collinarity: max VIF = 3.5
xres=lm(NRep ~ z.age + I(z.age^2) + sex +setting + species + z.nid, data=dat.ind.old)
library(car)
vif(xres)

mod.NRep.a <- glmer(formula = NRep ~  z.age + I(z.age^2) + sex +setting + species + z.nid +
                      (1|group), family = poisson, data = dat.ind.old, control = contr)
null.NRep.a <- glmer(formula = NRep ~  sex +z.nid +
                       (1|group), family = poisson, data = dat.ind.old, control = contr)

#Check for overdispersion:
overdisp.test(mod.NRep.a) # dispersion parameter 0.81 (ok)

#Check model stability # no issue
cbind(coefficients(mod.NRep.p), coefficients(mod.NRep.p)+
        t(apply(X=dfbeta(mod.NRep.p), MARGIN=2, FUN=range)))

summary(mod.NRep.a)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.64208    0.14380  18.373   <2e-16 ***
#z.age        0.03122    0.09072   0.344   0.7307    
#I(z.age^2)  -0.15526    0.13737  -1.130   0.2584    
#sexM         0.21667    0.19774   1.096   0.2732    
#settingwild -0.29039    0.25152  -1.155   0.2483    
#speciesSum   0.19758    0.22957   0.861   0.3894    
#z.nid        0.26880    0.11848   2.269   0.0233 * 

# Check whether full and null model have the same number of residuals
length(residuals(null.NRep.a)) #16
length(residuals(mod.NRep.a)) #16

#Likelihood ratio test
as.data.frame(anova(null.NRep.a, mod.NRep.a, test="Chisq"))
#npar      AIC       BIC    logLik deviance   Chisq Df Pr(>Chisq)
#null.NRep.a    4 90.27675  93.36711 -41.13838 82.27675      NA NA         NA
#mod.NRep.a     8 95.76371 101.94442 -39.88186 79.76371 2.51304  4  0.6423024

#get chi square and p-values (all)
drop1(mod.NRep.a,  test ="Chisq")
z.age         1 93.882 0.1186 0.73059  
I(z.age^2)    1 95.039 1.2752 0.25879  
sex           1 94.971 1.2071 0.27191  
setting       1 95.105 1.3409 0.24687  
species       1 94.513 0.7491 0.38676  
z.nid         1 98.960 5.1960 0.02264 *


#### Model 2a: Context variety w mothers
mod.NCon.m <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex +setting + species + z.nid + 
                      (1|group), family = poisson, data = dat.ind.mot, control = contr)
null.NCon.m <- glmer(formula = NCon ~  sex + z.nid + 
                      (1|group), family = poisson, data = dat.ind.mot, control = contr)


summary(mod.NCon.m)
overdisp.test(mod.NCon.m) # dispersion parameter 0.14 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.m)) #25
length(residuals(mod.NCon.m)) #25

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.m, mod.NCon.m, test="Chisq"))
#            npar       AIC       BIC    logLik deviance     Chisq Df Pr(>Chisq)
#null.NCon.m    4  93.67703  98.55254 -42.83852 85.67703        NA NA         NA
#mod.NCon.m     8 101.46062 111.21163 -42.73031 85.46062 0.2164143  4  0.9945513


#### Model 2b: Context variety w others
mod.NCon.o <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex +setting + species + z.nid+ 
                  (1|group), family = poisson, data = dat.ind.oth, control = contr)
null.NCon.o <- glmer(formula = NCon ~ sex + z.nid + 
                      (1|group), family = poisson, data = dat.ind.oth, control = contr)

summary(mod.NCon.o)
# Over-dispersion test
overdisp.test(mod.NCon.o) # dispersion parameter 0.57 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.o)) #21
length(residuals(mod.NCon.o)) #21

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.o, mod.NCon.o, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.o    4 86.42742 90.60551 -39.21371 78.42742       NA NA         NA
#mod.NCon.o     8 87.88455 96.24073 -35.94227 71.88455 6.542872  4  0.1621091


#### Model 2c: Context variety w peers
mod.NCon.p <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex +setting + species + z.nid+ 
                      (1|group), family = poisson, data = dat.ind.pee, control = contr)
null.NCon.p <- glmer(formula = NCon ~ sex + z.nid + 
                       (1|group), family = poisson, data = dat.ind.pee, control = contr)

summary(mod.NCon.p)
# Over-dispersion test
overdisp.test(mod.NCon.p) # dispersion parameter 0.95 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.p)) #15
length(residuals(mod.NCon.p)) #15

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.p, mod.NCon.p, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.p    4 62.11321 64.94541 -27.05661 54.11321       NA NA         NA
#mod.NCon.p     8 66.44862 72.11302 -25.22431 50.44862 3.664594  4  0.4532964

#### Model 2d: Context variety w olders
mod.NCon.a <- glmer(formula = NCon ~ z.age + I(z.age^2) + sex +setting + species + z.nid+ 
                      (1|group), family = poisson, data = dat.ind.old, control = contr)
null.NCon.a <- glmer(formula = NCon ~ sex + z.nid + 
                       (1|group), family = poisson, data = dat.ind.old, control = contr)

summary(mod.NCon.a)
# Over-dispersion test
overdisp.test(mod.NCon.a) # dispersion parameter 0.29 (ok)

# Check whether full and null model have the same number of residuals
length(residuals(null.NCon.a)) #16
length(residuals(mod.NCon.a)) #16

#Likelihood ratio test, all individuals
as.data.frame(anova(null.NCon.a, mod.NCon.a, test="Chisq"))
#npar      AIC      BIC    logLik deviance    Chisq Df Pr(>Chisq)
#null.NCon.a    4 61.21459 64.30494 -26.60729 53.21459       NA NA         NA
#mod.NCon.a     8 67.49844 73.67915 -25.74922 51.49844 1.716147  4  0.7877825

source("D:/R/lm_course/glmm_stability.r")
mod.NRep.m.stab=glmm.model.stab(model.res=mod.NRep.m, contr=contr, ind.cases=F, para=T)
mod.NRep.o.stab=glmm.model.stab(model.res=mod.NRep.o, contr=contr, ind.cases=F, para=T)
mod.NCon.m.stab=glmm.model.stab(model.res=mod.NCon.m, contr=contr, ind.cases=F, para=T)
mod.NCon.o.stab=glmm.model.stab(model.res=mod.NCon.o, contr=contr, ind.cases=F, para=T)





#Plots
dat.ind.oth$sex =as.factor(dat.ind.oth$sex)
dat.ind.oth$sex.code=as.numeric(dat.ind.oth$sex==levels(dat.ind.oth$sex)[2])
sex.code.c=dat.ind.oth$sex.code-mean(dat.ind.oth$sex.code)

dat.ind.mot$sex =as.factor(dat.ind.mot$sex)
dat.ind.mot$sex.code=as.numeric(dat.ind.mot$sex==levels(dat.ind.mot$sex)[2])
sex.code.c=dat.ind.mot$sex.code-mean(dat.ind.mot$sex.code)

plot.mod.NRep.o <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.nid + (1|group),
                      data = dat.ind.oth)

plot.mod.NRep.m <- lmer(formula = NRep ~ z.age + I(z.age^2) + sex.code.c +setting * species + z.nid  + (1|group), data = dat.ind.mot)

setwd("C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/")
source("local_jitter_AlexHausmann.R")
require(effects)



#Plot RS directed at others
dat.ind.oth$XPos <- ifelse(dat.ind.oth$species=="Bor",1,2)
EF <- Effect(c("species","setting"),plot.mod.NRep.o,se=TRUE)
dat.EF <- as.data.frame(EF)

# Add colour column
dat.ind.oth$colourBG <- ifelse(dat.ind.oth$setting=="wild",rgb(255, 210, 128, maxColorValue=255),rgb(128, 128, 255, maxColorValue=255))
dat.ind.oth$colourL <- ifelse(dat.ind.oth$setting=="wild",rgb(255, 192, 77, maxColorValue=255),rgb(77, 77, 255, maxColorValue=255))

# Open empty plot (IMPORTANT: THE PLOT HAS TO BE OPEN BEFORE RUNNING THE FUNCTION)
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"


svg(filename=paste0(path,"IndRepOth_v4.svg",sep=""), height=90/25.4, width=90/25.4, family="Arial", pointsize=9)
OF <- 0.1

par(mar=c(2.7, 3.2, 0.2, 0.2), mgp=c(1.3, 0.2, 0), tcl=-0.25, cex=1)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="Orang-utan species", ylab="", cex=1.5) ; par(new=TRUE)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="", ylab="Signal repertoire directed at others", cex=1.5, mgp=c(2.2, 0.2, 0))

X0 <-local_jitter(fact_coord = dat.ind.oth$XPos, gradual_coord = dat.ind.oth$NRep, categories = as.character(dat.ind.oth$setting), factorial_axis = 1, buffer = 0.45, sizes = sqrt(dat.ind.oth$NID)/4, verbose=F, iterations=1000)
points(X0,dat.ind.oth$NRep,cex=sqrt(dat.ind.oth$NID)/4, pch=21, bg=dat.ind.oth$colourBG, col=dat.ind.oth$colourL)

arrows(1-OF,dat.EF$lower[1],1-OF,dat.EF$upper[1],code=3,length=0.1,angle=90)
points(x=1-OF,y=dat.EF$fit[1], pch=23, col="black", bg="blue", cex=3)

arrows(1+OF,dat.EF$lower[3],1+OF,dat.EF$upper[3],code=3,length=0.1,angle=90)
points(x=1+OF,y=dat.EF$fit[3], pch=23, col="black", bg="orange", cex=3)

arrows(2-OF,dat.EF$lower[2],2-OF,dat.EF$upper[2],code=3,length=0.1,angle=90)
points(x=2-OF,y=dat.EF$fit[2], pch=23, col="black", bg="blue", cex=3)

arrows(2+OF,dat.EF$lower[4],2+OF,dat.EF$upper[4],code=3,length=0.1,angle=90)
points(x=2+OF,y=dat.EF$fit[4], pch=23, col="black", bg="orange", cex=3)

axis(1,at=c(1,2), label=c("Bornean","Sumatran"), tcl=-0.25, cex=1.5)
axis(2,at=seq(0,40,by=5), label=c("0","5","10","15","20","25", "30", "35", "40"), tcl=-0.25, las=2, mgp=c(1.2, 0.4, 0))
legend("topright", pt.bg=c("blue","orange"), pch=23, legend=c("captive","wild"), bty="n", pt.cex=2)
text(x=0.5,y=40, "B", cex=1.5)

box()
dev.off()
#border=c(rgb(77, 77, 255, maxColorValue=255),rgb(255, 192, 77, maxColorValue=255)), fill=c(rgb(128, 128, 255, maxColorValue=255), rgb(255, 210, 128, maxColorValue=255))


#Plot RS directed at mothers
dat.ind.mot$XPos <- ifelse(dat.ind.mot$species=="Bor",1,2)
EF <- Effect(c("species","setting"),plot.mod.NRep.m,se=TRUE)
dat.EF <- as.data.frame(EF)

# Add colour column
dat.ind.mot$colourBG <- ifelse(dat.ind.mot$setting=="wild",rgb(255, 210, 128, maxColorValue=255),rgb(128, 128, 255, maxColorValue=255))
dat.ind.mot$colourL <- ifelse(dat.ind.mot$setting=="wild",rgb(255, 192, 77, maxColorValue=255),rgb(77, 77, 255, maxColorValue=255))

# Open empty plot (IMPORTANT: THE PLOT HAS TO BE OPEN BEFORE RUNNING THE FUNCTION)
path <- "C:/Users/Marlen Fröhlich/Documents/R/MS Orang Infants/"

svg(filename=paste0(path,"IndRepMot_v3.svg",sep=""), height=90/25.4, width=90/25.4, family="Arial", pointsize=9)
OF <- 0.1

par(mar=c(2.7, 3.2, 0.2, 0.2), mgp=c(1.3, 0.2, 0), tcl=-0.25, cex=1)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="Orang-utan species", ylab="", cex=1.5) ; par(new=TRUE)
plot(c(0.5,2.5),c(0,40), type="n", axes=FALSE, xlab="", ylab="Signal repertoire directed at mothers", mgp=c(2.2, 0.2, 0), cex=1.5)

X0 <-local_jitter(fact_coord = dat.ind.mot$XPos, gradual_coord = dat.ind.mot$NRep, categories = as.character(dat.ind.mot$setting), factorial_axis = 1, buffer = 0.45, sizes = sqrt(dat.ind.mot$NID)/4, verbose=F, iterations=1000)
points(X0,dat.ind.mot$NRep,cex=sqrt(dat.ind.mot$NID)/4, pch=21, bg=dat.ind.mot$colourBG, col=dat.ind.mot$colourL)

arrows(1-OF,dat.EF$lower[1],1-OF,dat.EF$upper[1],code=3,length=0.1,angle=90)
points(x=1-OF,y=dat.EF$fit[1], pch=23, col="black", bg="blue", cex=3)

arrows(1+OF,dat.EF$lower[3],1+OF,dat.EF$upper[3],code=3,length=0.1,angle=90)
points(x=1+OF,y=dat.EF$fit[3], pch=23, col="black", bg="orange", cex=3)

arrows(2-OF,dat.EF$lower[2],2-OF,dat.EF$upper[2],code=3,length=0.1,angle=90)
points(x=2-OF,y=dat.EF$fit[2], pch=23, col="black", bg="blue", cex=3)

arrows(2+OF,dat.EF$lower[4],2+OF,dat.EF$upper[4],code=3,length=0.1,angle=90)
points(x=2+OF,y=dat.EF$fit[4], pch=23, col="black", bg="orange", cex=3)

axis(1,at=c(1,2), label=c("Bornean","Sumatran"), tcl=-0.25)
axis(2,at=seq(0,40,by=5), label=c("0","5","10","15","20","25", "30", "35", "40"), tcl=-0.25, las=2, mgp=c(1.2, 0.4, 0))
legend("topright", pt.bg=c("blue","orange"), pch=23, legend=c("captive","wild"), bty="n", pt.cex=2)
text(x=0.5,y=40, "A", cex=1.5)

box()
dev.off()
#border=c(rgb(77, 77, 255, maxColorValue=255),rgb(255, 192, 77, maxColorValue=255)), fill=c(rgb(128, 128, 255, maxColorValue=255), rgb(255, 210, 128, maxColorValue=255))


#Plots Dice Within-Between

library(ggplot2)
theme_marlen_ss <- theme(panel.background = element_blank(),
                         panel.border =element_rect(colour="black", fill=NA),
                         
                         plot.background = element_blank(),
                         panel.grid = element_blank(),
                         axis.line = element_line(colour ="black"),
                         axis.text.x = element_text (size = 12,colour= "black", family="sans"),
                         axis.text.y = element_text (size = 12,colour= "black", family="sans"),
                         axis.ticks.y = element_line(colour="black"),
                         axis.ticks.x = element_line(colour=NA),
                         axis.title.x = element_text(size = 13, vjust = -0.5, family="sans"),
                         axis.title.y = element_text(size = 13, vjust = 2, family="sans"),
                         legend.text=  element_text(size = 11, family="sans", margin = margin(t = 10)),
                         legend.key = element_blank(),
                         legend.position = "right",
                         legend.spacing.x = unit(0.2, 'cm'),
                         strip.text = element_text(size = 11))

levels(dd$species.ind1) <- c("Bornean", "Sumatran")


#Single Plot (IF and IO separately)
dodge.posn <- position_dodge(.9)
#svg("/Users/mfroehlich/R/self.svg", width=90/25.4, height=90/25.4, pointsize=9,family="arial")
mod.site <- ggplot(dd, aes(x = species.ind1, y = Dice))
mod.site + geom_boxplot(aes(fill = WB), width = 0.9) +
  geom_point(aes(fill = WB),position= dodge.posn, shape = 1, colour = "black", alpha = 0.5) +
  theme_marlen_ss +
  scale_y_continuous("Similarity of mother-directed repertoires", limits = c(0, 1)) +
  scale_x_discrete("Orang-utan species",
                   limits = c("Bor", "Sum"),
                   labels = c("Bornean", "Sumatran"))+
  scale_fill_manual(values=c("gray", "white"),name="Setting",
                    breaks=c("Between","Within"),
                    labels=c("Between","Within"))+
  # facet_wrap(~species.ind1)+
  stat_summary(fun=mean, geom="point",shape =23, fill ="black",aes(group=WB), position=position_dodge(.9), 
               color="black", size=3)


#Combined Plot?
dodge.posn <- position_dodge(.9)
#svg("/Users/mfroehlich/R/self.svg", width=90/25.4, height=90/25.4, pointsize=9,family="arial")
mod.site <- ggplot(dice, aes(x = Partner, y = Dice))
mod.site + geom_boxplot(aes(fill = WB), width = 0.9) +
  geom_point(aes(fill = WB),position= dodge.posn, shape = 1, colour = "black", alpha = 0.5) +
  theme_marlen_ss +
  scale_y_continuous("Repertoire similarity (Dice coefficients)") +
  scale_x_discrete("Interaction Partner",
                   limits = c("Mothers", "Others"),
                   labels = c("Mother-directed", "Other-directed"))+
  scale_fill_manual(values=c("blue", "orange"),name="Setting",
                    breaks=c("Between","Within"),
                    labels=c("Between","Within"))+
  facet_wrap(~species.ind1)+
  stat_summary(fun=mean, geom="point",shape =23, fill ="black",aes(group=WB), position=position_dodge(.9), 
               color="black", size=3)



write.table(dat,file ="D:/R/MS Orang Infants/dat_IO.csv", sep=",", col.names=TRUE, row.names=FALSE)

