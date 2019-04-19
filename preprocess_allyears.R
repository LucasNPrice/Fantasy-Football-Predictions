# Luke Price 
library(dplyr)
library(stringr)
library(rlist)

offense <- rbind(
  read.csv("QB_DATA_CSV_3.csv"), read.csv("RB_DATA_CSV_3.csv"), read.csv("WR_DATA_CSV_3.csv")
)


# cleaning up identifiers in offense for joining data sets 
off.stats <- strsplit(as.character(offense$off.stats.lookup), "-")
# rm(t) if below throws error
rm(t)
off.stats <- as.data.frame(t(as.data.frame(off.stats)))
rownames(off.stats) <- NULL
colnames(off.stats) <- c("player", "position", "year", "week")
off.stats$year <- as.integer(as.character(off.stats$year)) 
off.stats$week <- as.integer(as.character(off.stats$week)) 
offense <- cbind(offense, off.stats)
# offense <- offense[offense$year == 2018, ]

def.team = sub("-.*","",as.character(offense$def.stats.lookup))
offense <- cbind(offense, def.team)

# set player IDS
players <- unique(select(offense, player, position))
IDS <- seq(1:dim(players)[1])
player.id <- cbind(players, IDS)
offense <- merge(offense, player.id, by = c('player', 'position'))

# create offensive averages 
off.stats <- offense[, c(9:22,36,37,39)]
off.stats <- off.stats[, order(names(off.stats))]
avg.off <- off.stats[0,]
weeks <- avg.off[0,c(17,16,2)]

for (i in min(off.stats$year):max(off.stats$year)) {
  stats <- filter(off.stats, year == i)
  for (j in min(stats$week):max(stats$week)) {
    weeks <- rbind(weeks, arrange(select(filter(stats, week == j), year, week, IDS), IDS))
    iter <- stats[stats$week <= j, ] %>%
      group_by(IDS) %>%
      summarise_all(funs(mean))
    avg.off <- rbind(avg.off, as.data.frame(iter[, order(names(iter))]))
  }
}

avg.off <- unique(avg.off)
avg.off$weeks == weeks$week
offense <- merge(offense[,c(1:8,36:39)], avg.off, by = c("IDS", "year", "week"))

filter(off.stats, year == 2015, week == 5, IDS == 199)
filter(avg.off, year == 2015, week == 5, IDS == 199)

# create defensive averages 
def <- read.csv("def.data.csv")
def <- def[, -2]
avg.def <- def[0,]
weeks <- def[0,c(13,14,1)]

for (i in min(def$year):max(def$year)) {
  
  stats <- filter(def, year == i)

  for (j in min(stats$week):max(stats$week)) {
    
    weeks <- rbind(weeks, arrange(select(filter(stats, week == j), year, week, team), team))

    iter <- stats[stats$week <= j, ] %>%
      group_by(team) %>%
      summarise_all(funs(mean))
    avg.def <- rbind(avg.def, as.data.frame(iter[, order(names(iter))]))
  }
}
avg.def <- unique(avg.def)
avg.def$week <- weeks$week
# order by years, then order by pairs weeks, then order by team

colnames(avg.def)[colnames(avg.def) == "team"] <- "def.team"
colnames(avg.def)[colnames(avg.def) == "int"] <- "def.int"
colnames(avg.def)[colnames(avg.def) == "TD"] <- "def.TD"
colnames(offense)[colnames(offense) == "int"] <- "pass.int"

offense <- merge(offense, avg.def, by = c("def.team", "year", "week"))
offense$week <- offense$week + 1
offense <- offense[,c(4, 5, 2, 3, 10, 11, 6, 12:26, 1, 27:30, 32:37, 7:9)]

write.csv(offense, "fball_all_years.csv")


