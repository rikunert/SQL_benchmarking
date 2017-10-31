#This script benchmarks timing differences between base R and SQLite

#written by Richard Kunert (rikunert[at]gmail.com) Oct 2017

##########################################################################
# LOAD LIBRARIES

if(!require(ggthemes)){install.packages('ggthemes')} #plotting
library(ggthemes)

if(!require(ggplot2)){install.packages('ggplot2')} #plotting
library(ggplot2)

if(!require(microbenchmark)){install.packages('microbenchmark')} #
library(microbenchmark)

if(!require(RSQLite)){install.packages('RSQLite')} #install SQLite engine
library(RSQLite)

if(!require(DBI)){install.packages('DBI')} #interface with SQL
library(DBI)

##########################################################################
# DATA BASE SET UP
#initialise data bases
db_disk <- dbConnect(RSQLite::SQLite(), "")#tmp
db_mem <- dbConnect(RSQLite::SQLite(), "file::memory:")

#populate data bases with data
dbWriteTable(db_disk, "iris", iris)
dbWriteTable(db_mem, "iris", iris)

##########################################################################
# CUSTOM FUNCTIONS
fun_disk <- function(query, db_disk. = db_disk) dbGetQuery(db_disk., query)
fun_mem <- function(query, db_mem. = db_mem) dbGetQuery(db_mem., query)
fun_plot <- function(dat_box=tm, y_lim = c(10, 50000)){

  dat_box$time = dat_box$time/1000
  
  A = ggplot(dat_box, aes(x = expr, group = expr, y = time)) +
    geom_tufteboxplot(median.type = "line", whisker.type = 'line', hoffset = 0, width = 3) +
    xlab("") + 
    ylab("Execution Time (microseconds)") + 
    scale_y_log10(limits = y_lim, breaks = 10^(1:4)) +
    scale_x_discrete(labels = c("SQL\non disk",
                                "SQL\nin memory",
                                "base R\nin memory\n(variation 1)", 
                                "base R\nin memory\n(variation 2)")) +
    theme_tufte(ticks = F) + 
    theme(axis.text.x = element_text(size=12)) +
    labs(caption = '@rikunert') + 
    theme(plot.caption = element_text(size = 10, color = 'grey', face= 'italic'))
  A
  
}
##########################################################################
#1: basic query
query = 'SELECT * FROM iris LIMIT 5'
fun_base1 <- function(data = iris) head(data, 5)
fun_base2 <- function(data = iris) data[1:5,]

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Basic query')
ggsave('SQL_R_bench_1.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_mem, query)

##########################################################################
#2: aggregate
query = 'SELECT species, AVG("Petal.Length") FROM iris GROUP BY species'
fun_base1 <- function(data = iris) aggregate(data$Petal.Length~data$Species, FUN=mean)
fun_base2 <- function(data = iris) by(data$Petal.Length, data$Species, mean)

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Basic aggregation & grouping')
ggsave('SQL_R_bench_2.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_mem, query)

##########################################################################
#3: conditional
query = 'SELECT species, COUNT(*) FROM iris WHERE "Petal.Length" >= 2'
fun_base1 <- function(data = iris) sum(data$Petal.Length > 2)
fun_base2 <- function(data = iris) length(data$Petal.Length[data$Petal.Length > 2])

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Basic aggregation on conditional')
ggsave('SQL_R_bench_3.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_mem, query)

##########################################################################
#4: temporary variable & aggregation & condition
query = 'SELECT species, 
CASE  
WHEN "Petal.Length" < 2 THEN \'small\' 
WHEN "Petal.Length" > 5 THEN \'big\' 
ELSE \'intermediate\'
END AS size,
COUNT(*)
FROM iris
GROUP BY 1, 2;'

fun_base1 <- function(data = iris) {
  size = rep('intermediate', nrow(data))
  size[data$Petal.Length < 2] = 'small'
  size[data$Petal.Length > 5] = 'big'
  aggregate(rep(1, nrow(data)),#just a vector of 1s which will get added up
            by=list(Species = data$Species, size = size),
            FUN=sum)
}

fun_base2 <- function(data = iris) {
  out = data.frame(Species = rep(unique(data$Species), each = 3),
                   size = rep(c('big', 'intermediate', 'small'), 3),
                   count = rep(NA, 3*3))
  for (Species in unique(data$Species)){
    out$count[out$Species == Species & out$size == 'small'] = sum(data$Species == Species & data$Petal.Length < 2)
    out$count[out$Species == Species & out$size == 'intermediate'] = sum(data$Species == Species & data$Petal.Length >= 2 & data$Petal.Length <= 5)
    out$count[out$Species == Species & out$size == 'big'] = sum(data$Species == Species & data$Petal.Length > 5)
  }
}

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Aggregation of temporary variable on conditional')
ggsave('SQL_R_bench_4.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_mem, query)

##########################################################################
#5: create table and add to data base
query1 = 'DROP TABLE IF EXISTS flowers;'
query2 = 'CREATE TABLE flowers (species TEXT, bloom_start TEXT, bloom_end TEXT);'
query3 = 'INSERT INTO flowers (species, bloom_start, bloom_end) 
VALUES 
(\'setosa\', \'June\', \'July\'),
(\'versicolor\', \'May\', \'July\'),
(\'virginica\', \'April\', \'May\'),
(\'pallida\', \'May\', \'June\');'

fun_disk_exe3 <- function(statements, db_disk. = db_disk) lapply(1:3, function(x) dbExecute(db_disk., statements[x]))
fun_mem_exe3 <- function(statements, db_mem. = db_mem) lapply(1:3, function(x) dbExecute(db_mem., statements[x]))

fun_base1 <- function(data = iris) {
  flowers = data.frame(species = c('setosa', 'versicolor', 'virginica', 'pallida'),
                       bloom_start = c('June', 'May', 'April', 'May'),
                       bloom_end = c('July', 'July', 'May', 'June'))
}

fun_base2 <- function(data = iris) {
  flowers = rbind(c('setosa','June','July'),
                  c('versicolor', 'May', 'July'),
                  c('virginica', 'April', 'May'),
                  c('pallida', 'May', 'June')
                  )
  colnames(flowers) = c('species', 'bloom_start', 'bloom_end')
  as.data.frame(flowers)
}

tm = microbenchmark(fun_disk_exe3(c(query1, query2, query3)), fun_mem_exe3(c(query1, query2, query3)), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Create table and add to data base')
ggsave('SQL_R_bench_5.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

##########################################################################
#6: joining 2 tables
query = 'SELECT * FROM flowers
JOIN iris ON
flowers.species = iris.species'

flowers = data.frame(species = c('setosa', 'versicolor', 'virginica', 'pallida'),
                     bloom_start = c('June', 'May', 'April', 'May'),
                     bloom_end = c('July', 'July', 'May', 'June'))

fun_base1 <- function(data1 = iris, data2 = flowers) {
  data1$bloom_start = NA
  data1$bloom_end = NA
  for(i in 1:dim(data1)[1]){#for each row in data1
    data1[i, 6] = as.character(data2[data2$species == as.character(data1$Species[i]), 2])
    data1[i, 7] = as.character(data2[data2$species == as.character(data1$Species[i]), 3])
  }
  return(data1)
}

fun_base2 <- function(data1 = iris, data2 = flowers) {
  merge(data1, data2, by.x = 'Species', by.y = 'species')
}

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Joining two tables')
ggsave('SQL_R_bench_6.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_disk, query)

##########################################################################
#7: combine info from 2 tables and aggregating
query = 'SELECT flowers.bloom_end, AVG(iris."petal.length") FROM flowers
JOIN iris ON
flowers.species = iris.species
GROUP BY flowers.bloom_end'

fun_base1 <- function(data1 = iris, data2 = flowers) {
  out = matrix(rep(NA, length(unique(data2$bloom_end)) * 2), nrow = length(unique(data2$bloom_end)), ncol = 2)
  for (i in 1:length(unique(data2$bloom_end))){
    out[i,] = c(as.character(unique(data2$bloom_end)[i]),
            mean(data1$Petal.Length[
              grepl(paste(data2$species[data2$bloom_end == unique(data2$bloom_end)[i]],collapse = '|'),
                    data1$Species)]))
  }
}

fun_base2 <- function(data1 = iris, data2 = flowers) {
  x = merge(data1, data2, by.x = 'Species', by.y = 'species')
  aggregate(x = x, by=list(x$bloom_end), FUN=mean)[,c(1, 5)]
}

tm = microbenchmark(fun_disk(query), fun_mem(query), fun_base1(), fun_base2())

fun_plot(dat_box = tm) + ggtitle('Joining two tables, aggregation, and grouping')
ggsave('SQL_R_bench_7.png',width = 12.9, height = 5.42, scale = 0.7, dpi = 1000)

dbGetQuery(db_disk, query)