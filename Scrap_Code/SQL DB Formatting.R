#SQL Database Formatting
library(dplyr)
library(data.table)
library(RSQLite)

# sqlite_file <- "ukbb.sqlite"
# sqldb <- dbConnect(SQLite(), dbname=sqlite_file)
# system.time(rsid <- dbGetQuery(sqldb, 'select ID from ih_mod1')[,1])

## Import results from different stroke types (is = ischemic, ih = intracerebral hemorrhagic, sh = subarachnoid hemorrhage)
is_annot <- fread("is_comb_results_annotated.txt")
# names(is_annot)[1] <- "CHROM"
# is_annot <- is_annot[order(is_annot[,'P'], -is_annot[,'P']),]
ih_annot <- fread("ih_comb_results_annotated.txt")
# names(ih_annot)[1] <- "CHROM"
# ih_annot <- ih_annot[order(ih_annot[,'P'], -ih_annot[,'P']),]
sh_annot <- fread("sh_comb_results_annotated.txt")
# names(sh_annot)[1] <- "CHROM"
# sh_annot <- sh_annot[order(sh_annot[,'P'], -sh_annot[,'P']),]

## Combine results into one SQL db
annotcombdb <- dbConnect(SQLite(), dbname="annot_comb.sqlite")
dbWriteTable(conn=annotcombdb, name="is_annot", value=is_annot, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)
dbWriteTable(conn=annotcombdb, name="ih_annot", value=ih_annot, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)
dbWriteTable(conn=annotcombdb, name="sh_annot", value=sh_annot, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)

dbExecute(annotcombdb, "create index is_id_index on is_annot(id)")
dbExecute(annotcombdb, "create index is_chrom_index on is_annot(chrom)")
dbExecute(annotcombdb, "create index is_pos_index on is_annot(pos)")
# dbExecute(annotdb, "drop index is_p_index") #P-value index makes table output take a really long time

dbExecute(annotcombdb, "create index ih_id_index on ih_annot(id)")
dbExecute(annotcombdb, "create index ih_chrom_index on ih_annot(chrom)")
dbExecute(annotcombdb, "create index ih_pos_index on ih_annot(pos)")
# dbExecute(annotdb, "drop index ih_p_index")

dbExecute(annotcombdb, "create index sh_id_index on sh_annot(id)")
dbExecute(annotcombdb, "create index sh_chrom_index on sh_annot(chrom)")
dbExecute(annotcombdb, "create index sh_pos_index on sh_annot(pos)")
# dbExecute(annotdb, "drop index sh_p_index")
dbDisconnect(annotcombdb)

## Check SQL db
test <- dbGetQuery(annotcombdb, "select ID from is_annot")

m1_test <- dbGetQuery(annotcombdb, "select * from is_annot where ID = 'rs34148057'")

m1_res <- select(m1_test, c("CHROM","POS","ID","REF","ALT1", starts_with("m1_"), "Func.refGene","Gene.refGene","GeneDetail.refGene","ExonicFunc.refGene","AAChange.refGene"))
names(m1_res) <- sub(".*_","",names(m1_res)) 
test <- is_annot[order(is_annot[,'P'], -is_annot[,'P']),]
test <- filter(test, grepl(pattern='^rs', ID))

## Get list of unique rsIDs from results
## Import rsIDs from different stroke types
# is_rsid <- fread("is_comb_results_annotated.txt", select = 3)
# ih_rsid <- fread("ih_comb_results_annotated.txt", select = 3)
# sh_rsid <- fread("sh_comb_results_annotated.txt", select = 3)

head(sh_rsid)

rsid <- bind_rows(is_rsid, ih_rsid, sh_rsid)
dim(rsid)

ord_rsid <- rsid[order(rsid[,'ID']),]
uniq_rsid <- ord_rsid[!duplicated(ord_rsid$ID, incomparables=NA),]
uniq_rsid <- filter(uniq_rsid, grepl(pattern='^rs', ID))
write.table(uniq_rsid, "uniq_rsid.txt", row.names = FALSE)

tail(uniq_rsid)
