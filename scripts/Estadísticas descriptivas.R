pkgs <- c("dplyr", "tidyr", "broom", "pastecs")
install.packages(pkgs) #install 
sapply(pkgs, require, character.only = T) #load 

chapi_train_stats <- subset(chapi_train, select=c(price,bedrooms,surface2,tiene_terraza,tiene_garaje,dist_chapi_bus,dist_east))
chapi_train_stats$tiene_terraza <- strtoi(chapi_train_stats$tiene_terraza)
chapi_train_stats$tiene_garaje <- strtoi(chapi_train_stats$tiene_garaje)
options(digits=3)
ch_train_stats <- stat.desc(chapi_train_stats,basic=F,desc=F)
write.table(ch_train_stats, file = "chapi_train.txt", sep = ",", quote = FALSE, row.names = F)

chapi_test_stats <- subset(chapi_test, select=c(bedrooms,surface2,tiene_terraza,tiene_garaje,dist_chapi_bus,dist_east))
chapi_test_stats$tiene_terraza <- strtoi(chapi_test_stats$tiene_terraza)
chapi_test_stats$tiene_garaje <- strtoi(chapi_test_stats$tiene_garaje)
options(digits=3)
ch_test_stats <- stat.desc(chapi_test_stats,basic=F,desc=F)
write.table(ch_test_stats, file = "chapi_test.txt", sep = ",", quote = FALSE, row.names = F)


med_train_stats <- subset(house_train_med, select=c(price,bedrooms,surface2,tiene_terraza,tiene_garaje,dist_med_bus,dist_golf))
med_train_stats$tiene_terraza <- strtoi(med_train_stats$tiene_terraza)
med_train_stats$tiene_garaje <- strtoi(med_train_stats$tiene_garaje)
options(digits=3)
md_train_stats <- stat.desc(med_train_stats,basic=F,desc=F)
write.table(md_train_stats, file = "med_train.txt", sep = ",", quote = FALSE, row.names = F)


med_test_stats <- subset(house_test_med, select=c(bedrooms,surface2,tiene_terraza,tiene_garaje,dist_med_bus,dist_golf))
med_test_stats$tiene_terraza <- strtoi(med_test_stats$tiene_terraza)
med_test_stats$tiene_garaje <- strtoi(med_test_stats$tiene_garaje)
options(digits=3)
md_test_stats <- stat.desc(med_test_stats,basic=F,desc=F)
write.table(md_test_stats, file = "med_test.txt", sep = ",", quote = FALSE, row.names = F)

