library(data.table)
library(RSQLite)

#http://kbroman.org/blog/2017/04/30/sqlite-feather-and-fst/

##Import Results files and rename CHROM variable
ih_mod1 <- fread(file = "ih_mod1_results.txt")
names(ih_mod1)[1] <- "CHROM"
is_mod1 <- fread(file = "is_mod1_results.txt")
names(is_mod1)[1] <- "CHROM"
sh_mod1 <- fread(file = "sh_mod1_results.txt")
names(sh_mod1)[1] <- "CHROM"

## Create database, repeat Write Table for all files
sqlite_file <- "ukbb.sqlite"
sqldb <- dbConnect(SQLite(), dbname=sqlite_file)
#dbWriteTable(conn=sqldb, name="ih_mod1", value=ih_mod1, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)
#dbWriteTable(conn=sqldb, name="is_mod1", value=is_mod1, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)
#dbWriteTable(conn=sqldb, name="sh_mod1", value=sh_mod1, row.names=FALSE, overwrite=TRUE, append=FALSE, field.types=NULL)
dbDisconnect(sqldb) ## Disconnect when done

##Run Simple Queries
sqldb <- dbConnect(SQLite(), dbname=sqlite_file)
snp_pos <- dbGetQuery(sqldb, 'select POS from ih_mod1')[1:10,1]

##Taking top 100 SNPS based on P-value
P_ih <- dbGetQuery(sqldb, 'select ID,P from ih_mod1 order by P')[1:100,]
P_is <- dbGetQuery(sqldb, 'select ID,P from is_mod1 order by P')[1:100,]
P_sh <- dbGetQuery(sqldb, 'select ID,P from sh_mod1 order by P')[1:100,]

##Indexing
## Strategy 1: Joint index on ID and chrom
dbExecute(sqldb, "create index id_chrom on ih_mod1(id, chrom)")

## Strategy 2: Separate index on ID, chrom and position
dbExecute(sqldb, "create index id_index on ih_mod1(id)")
dbExecute(sqldb, "create index chrom_index on ih_mod1(chrom)")
dbExecute(sqldb, "create index pos_index on ih_mod1(pos)")

##Get window of results based on random RSID
snp_names <- dbGetQuery(sqldb, 'select ID from ih_mod1')[1000000:1001000,1]
snp_choice <- sample(snp_names, 1)
marker <- dbGetQuery(sqldb, paste0("select POS from ih_mod1 where ID = '", snp_choice, "'"))[,1]
max <- marker + 500000
min <- marker - 500000
marker_chrom <- dbGetQuery(sqldb, paste0("select CHROM from ih_mod1 where ID = '", snp_choice, "'"))[1,1]
range <- dbGetQuery(sqldb, paste0("SELECT * FROM ih_mod1 WHERE CHROM = ", marker_chrom, " AND POS > ", min, " AND POS < ", max))

##Remove existing index
dbExecute(sqldb, "drop index id_chrom")



all_chrom <- dbReadTable(sqldb, "ih_mod1")
manhattan(all_chrom, chr = "CHROM", snp = "ID", bp = "POS", highlight = range$ID)
          # main=paste0("Manhattan Plot for Stroke Type=", input$user_stroketype, " and SNP =", input$user_rsID, " range ", min(range$POS), " & ", max(range$POS)))

