rm(list = ls())

# set working directory to file location
# for Travis:
setwd("C:/Users/twwarziniack/OneDrive - USDA/Coverage")
#setwd("~/5_RPA/Coverage")

library(tidyr)
library(ggplot2)
#library(reshape2)
library(dplyr)
#library(data.table)

# - cnrm climate model Net 1

net1.cnrm.1.45 <- read.csv("net1-cnrm-1-45.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.cnrm.1.45)[1] <- 'huc'
cover.net1.cnrm.1.45 <- net1.cnrm.1.45

numrow <- dim(net1.cnrm.1.45)[1]
numcol <- dim(net1.cnrm.1.45)[2]

for (j in 2:numcol){
for (i in 1:numrow) {
  if (net1.cnrm.1.45[i,j] < 100) {
    cover.net1.cnrm.1.45[i,j] <- 1} else {cover.net1.cnrm.1.45[i,j] <- 0}  
  }
}

net1.cnrm.2.85 <- read.csv("net1-cnrm-2-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.cnrm.2.85)[1] <- 'huc'
cover.net1.cnrm.2.85 <- net1.cnrm.2.85

numrow <- dim(net1.cnrm.2.85)[1]
numcol <- dim(net1.cnrm.2.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.cnrm.2.85[i,j] < 100) {
      cover.net1.cnrm.2.85[i,j] <- 1} else {cover.net1.cnrm.2.85[i,j] <- 0}  
  }
}

net1.cnrm.3.85 <- read.csv("net1-cnrm-3-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.cnrm.3.85)[1] <- 'huc'
cover.net1.cnrm.3.85 <- net1.cnrm.3.85

numrow <- dim(net1.cnrm.3.85)[1]
numcol <- dim(net1.cnrm.3.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.cnrm.3.85[i,j] < 100) {
      cover.net1.cnrm.3.85[i,j] <- 1} else {cover.net1.cnrm.3.85[i,j] <- 0}  
  }
}

net1.cnrm.5.85 <- read.csv("net1-cnrm-5-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.cnrm.5.85)[1] <- 'huc'
cover.net1.cnrm.5.85 <- net1.cnrm.2.85

numrow <- dim(net1.cnrm.5.85)[1]
numcol <- dim(net1.cnrm.5.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.cnrm.5.85[i,j] < 100) {
      cover.net1.cnrm.5.85[i,j] <- 1} else {cover.net1.cnrm.5.85[i,j] <- 0}  
  }
}

# ---- hadgem net 1

net1.hadgem.1.45 <- read.csv("net1-hadgem-1-45.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.hadgem.1.45)[1] <- 'huc'
cover.net1.hadgem.1.45 <- net1.hadgem.1.45

numrow <- dim(net1.hadgem.1.45)[1]
numcol <- dim(net1.hadgem.1.45)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.hadgem.1.45[i,j] < 100) {
      cover.net1.hadgem.1.45[i,j] <- 1} else {cover.net1.hadgem.1.45[i,j] <- 0}  
  }
}

net1.hadgem.2.85 <- read.csv("net1-hadgem-2-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.hadgem.2.85)[1] <- 'huc'
cover.net1.hadgem.2.85 <- net1.hadgem.2.85

numrow <- dim(net1.hadgem.2.85)[1]
numcol <- dim(net1.hadgem.2.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.hadgem.2.85[i,j] < 100) {
      cover.net1.hadgem.2.85[i,j] <- 1} else {cover.net1.hadgem.2.85[i,j] <- 0}  
  }
}

net1.hadgem.3.85 <- read.csv("net1-hadgem-3-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.hadgem.3.85)[1] <- 'huc'
cover.net1.hadgem.3.85 <- net1.hadgem.3.85

numrow <- dim(net1.hadgem.3.85)[1]
numcol <- dim(net1.hadgem.3.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.hadgem.3.85[i,j] < 100) {
      cover.net1.hadgem.3.85[i,j] <- 1} else {cover.net1.hadgem.3.85[i,j] <- 0}  
  }
}

net1.hadgem.5.85 <- read.csv("net1-hadgem-5-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.hadgem.5.85)[1] <- 'huc'
cover.net1.hadgem.5.85 <- net1.hadgem.2.85

numrow <- dim(net1.hadgem.5.85)[1]
numcol <- dim(net1.hadgem.5.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.hadgem.5.85[i,j] < 100) {
      cover.net1.hadgem.5.85[i,j] <- 1} else {cover.net1.hadgem.5.85[i,j] <- 0}  
  }
}


#--- ipsl net 1

net1.ipsl.1.45 <- read.csv("net1-ipsl-1-45.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.ipsl.1.45)[1] <- 'huc'
cover.net1.ipsl.1.45 <- net1.ipsl.1.45

numrow <- dim(net1.ipsl.1.45)[1]
numcol <- dim(net1.ipsl.1.45)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.ipsl.1.45[i,j] < 100) {
      cover.net1.ipsl.1.45[i,j] <- 1} else {cover.net1.ipsl.1.45[i,j] <- 0}  
  }
}

net1.ipsl.2.85 <- read.csv("net1-ipsl-2-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.ipsl.2.85)[1] <- 'huc'
cover.net1.ipsl.2.85 <- net1.ipsl.2.85

numrow <- dim(net1.ipsl.2.85)[1]
numcol <- dim(net1.ipsl.2.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.ipsl.2.85[i,j] < 100) {
      cover.net1.ipsl.2.85[i,j] <- 1} else {cover.net1.ipsl.2.85[i,j] <- 0}  
  }
}

net1.ipsl.3.85 <- read.csv("net1-ipsl-3-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.ipsl.3.85)[1] <- 'huc'
cover.net1.ipsl.3.85 <- net1.ipsl.3.85

numrow <- dim(net1.ipsl.3.85)[1]
numcol <- dim(net1.ipsl.3.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.ipsl.3.85[i,j] < 100) {
      cover.net1.ipsl.3.85[i,j] <- 1} else {cover.net1.ipsl.3.85[i,j] <- 0}  
  }
}

net1.ipsl.5.85 <- read.csv("net1-ipsl-5-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.ipsl.5.85)[1] <- 'huc'
cover.net1.ipsl.5.85 <- net1.ipsl.2.85

numrow <- dim(net1.ipsl.5.85)[1]
numcol <- dim(net1.ipsl.5.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.ipsl.5.85[i,j] < 100) {
      cover.net1.ipsl.5.85[i,j] <- 1} else {cover.net1.ipsl.5.85[i,j] <- 0}  
  }
}


#--- mri net 1

net1.mri.1.45 <- read.csv("net1-mri-1-45.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.mri.1.45)[1] <- 'huc'
cover.net1.mri.1.45 <- net1.mri.1.45

numrow <- dim(net1.mri.1.45)[1]
numcol <- dim(net1.mri.1.45)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.mri.1.45[i,j] < 100) {
      cover.net1.mri.1.45[i,j] <- 1} else {cover.net1.mri.1.45[i,j] <- 0}  
  }
}

net1.mri.2.85 <- read.csv("net1-mri-2-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.mri.2.85)[1] <- 'huc'
cover.net1.mri.2.85 <- net1.mri.2.85

numrow <- dim(net1.mri.2.85)[1]
numcol <- dim(net1.mri.2.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.mri.2.85[i,j] < 100) {
      cover.net1.mri.2.85[i,j] <- 1} else {cover.net1.mri.2.85[i,j] <- 0}  
  }
}

net1.mri.3.85 <- read.csv("net1-mri-3-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.mri.3.85)[1] <- 'huc'
cover.net1.mri.3.85 <- net1.mri.3.85

numrow <- dim(net1.mri.3.85)[1]
numcol <- dim(net1.mri.3.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.mri.3.85[i,j] < 100) {
      cover.net1.mri.3.85[i,j] <- 1} else {cover.net1.mri.3.85[i,j] <- 0}  
  }
}

net1.mri.5.85 <- read.csv("net1-mri-5-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.mri.5.85)[1] <- 'huc'
cover.net1.mri.5.85 <- net1.mri.2.85

numrow <- dim(net1.mri.5.85)[1]
numcol <- dim(net1.mri.5.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.mri.5.85[i,j] < 100) {
      cover.net1.mri.5.85[i,j] <- 1} else {cover.net1.mri.5.85[i,j] <- 0}  
  }
}


# ---- noresm

net1.noresm.1.45 <- read.csv("net1-noresm-1-45.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.noresm.1.45)[1] <- 'huc'
cover.net1.noresm.1.45 <- net1.noresm.1.45

numrow <- dim(net1.noresm.1.45)[1]
numcol <- dim(net1.noresm.1.45)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.noresm.1.45[i,j] < 100) {
      cover.net1.noresm.1.45[i,j] <- 1} else {cover.net1.noresm.1.45[i,j] <- 0}  
  }
}

net1.noresm.2.85 <- read.csv("net1-noresm-2-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.noresm.2.85)[1] <- 'huc'
cover.net1.noresm.2.85 <- net1.noresm.2.85

numrow <- dim(net1.noresm.2.85)[1]
numcol <- dim(net1.noresm.2.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.noresm.2.85[i,j] < 100) {
      cover.net1.noresm.2.85[i,j] <- 1} else {cover.net1.noresm.2.85[i,j] <- 0}  
  }
}

net1.noresm.3.85 <- read.csv("net1-noresm-3-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.noresm.3.85)[1] <- 'huc'
cover.net1.noresm.3.85 <- net1.noresm.3.85

numrow <- dim(net1.noresm.3.85)[1]
numcol <- dim(net1.noresm.3.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.noresm.3.85[i,j] < 100) {
      cover.net1.noresm.3.85[i,j] <- 1} else {cover.net1.noresm.3.85[i,j] <- 0}  
  }
}

net1.noresm.5.85 <- read.csv("net1-noresm-5-85.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 3)
names(net1.noresm.5.85)[1] <- 'huc'
cover.net1.noresm.5.85 <- net1.noresm.2.85

numrow <- dim(net1.noresm.5.85)[1]
numcol <- dim(net1.noresm.5.85)[2]

for (j in 2:numcol){
  for (i in 1:numrow) {
    if (net1.noresm.5.85[i,j] < 100) {
      cover.net1.noresm.5.85[i,j] <- 1} else {cover.net1.noresm.5.85[i,j] <- 0}  
  }
}

write.csv(cover.net1.cnrm.1.45, file="coverNet1cnrm-1-45.csv")
write.csv(cover.net1.cnrm.1.45, file="coverNet1cnrm-2-85.csv")
write.csv(cover.net1.cnrm.1.45, file="coverNet1cnrm-3-85.csv")
write.csv(cover.net1.cnrm.1.45, file="coverNet1cnrm-5-85.csv")

write.csv(cover.net1.hadgem.1.45, file="coverNet1hadgem-1-45.csv")
write.csv(cover.net1.hadgem.1.45, file="coverNet1hadgem-2-85.csv")
write.csv(cover.net1.hadgem.1.45, file="coverNet1hadgem-3-85.csv")
write.csv(cover.net1.hadgem.1.45, file="coverNet1hadgem-5-85.csv")

write.csv(cover.net1.ipsl.1.45, file="coverNet1ipsl-1-45.csv")
write.csv(cover.net1.ipsl.1.45, file="coverNet1ipsl-2-85.csv")
write.csv(cover.net1.ipsl.1.45, file="coverNet1ipsl-3-85.csv")
write.csv(cover.net1.ipsl.1.45, file="coverNet1ipsl-5-85.csv")

write.csv(cover.net1.mri.1.45, file="coverNet1mri-1-45.csv")
write.csv(cover.net1.mri.1.45, file="coverNet1mri-2-85.csv")
write.csv(cover.net1.mri.1.45, file="coverNet1mri-3-85.csv")
write.csv(cover.net1.mri.1.45, file="coverNet1mri-5-85.csv")

write.csv(cover.net1.noresm.1.45, file="coverNet1noresm-1-45.csv")
write.csv(cover.net1.noresm.1.45, file="coverNet1noresm-2-85.csv")
write.csv(cover.net1.noresm.1.45, file="coverNet1noresm-3-85.csv")
write.csv(cover.net1.noresm.1.45, file="coverNet1noresm-5-85.csv")
