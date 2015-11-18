if (require("ggplot2") == FALSE) install.packages("ggplot2")
if (require("grid") == FALSE) install.packages("grid")
if (require("gridExtra") == FALSE) install.packages("gridExtra")
library(scales)  # for labels = percent

# Get data
# Data from hotslogs dump 2015/05/04-14, post-Kael'thas, pre-Johanna
setwd("C://Users//yue.GLOBAL//Documents//R//hots")    # Your working directory
x <- read.csv("Replays2015-05-14.csv")
y <- read.csv("ReplayCharacters2015-05-14.csv")

# create df of unique names
df.names <- as.data.frame(unique(y[,3]))

# number of unique names
names.unique <- as.numeric(nrow(df.names))

# empty matrix to be filled
df.empty <- matrix(nrow=names.unique, ncol=5)
colnames(df.empty) <- c("Hero", "Wins", "Losses", "Played", "Winrate")

# populate empty matrix
for(i in 1:names.unique){
  df.empty[i,1] <- as.character(df.names[i,])
  df.empty[i,2] <- sum(y$Is.Winner == "True" & y$Hero == df.names[i,] 
                       #& y$Hero.Level = 20
                       )
  df.empty[i,3] <- sum(y$Is.Winner == "False" & y$Hero == df.names[i,] 
                       #& y$Hero.Level = 20
                       )
  df.empty[i,4] <- as.numeric(df.empty[i,2]) + as.numeric(df.empty[i,3])
  df.empty[i,5] <- as.numeric(df.empty[i,2]) / as.numeric(df.empty[i,4])
}

# convert to dataframe for sorting
df.data <- as.data.frame(df.empty)
# sort by winrate
df.data <- df.data[order(df.data$Winrate),]

# get min/max levels for popularity scale
min.levels <- round(min(as.numeric(as.matrix(df.data$Played))),-3)
max.levels <- round(max(as.numeric(as.matrix(df.data$Played))),-3)
new.levels <- round((max.levels - min.levels) / 5, -3)

# ggplot of total games played per Hero
ggPlayed <- 
  ggplot(data = df.data, aes(x = reorder(Hero, as.numeric(Winrate)),
                             fill = Hero)) +    
    geom_bar(stat = "identity",
             aes(y = as.numeric(as.matrix(df.data$Played)))) +  
    scale_y_continuous(breaks = seq(0, max.levels, by = new.levels),
                       trans = 'reverse',      # flip horizontally
                       oob = rescale_none) +   
    scale_x_discrete(breaks = NULL) +          # remove hero labels
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(6,-9,1,0),"mm")) +
    ggtitle("Total games played") +
    coord_flip()

# ggplot of winrates per Hero
ggWinrate <- 
  ggplot(data = df.data, aes(x = reorder(Hero, as.numeric(Winrate)),
                             fill = Hero)) +
    geom_bar(stat = "identity", 
             aes(y = as.numeric(as.matrix(df.data$Winrate)))) +  
    scale_y_continuous(labels = percent,
                       limits = c(.3, .7), 
                       oob = rescale_none) +
    geom_hline(yintercept = .5, 
               alpha = .2,
               linetype = "longdash") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(6,1,1,-5),"mm")) +
    scale_x_discrete(breaks = NULL) +
    ggtitle("%-Winrate") +
    coord_flip()

# ggplot of just Hero names
ggHeroes <- 
  ggplot(data = df.data, aes(x = reorder(Hero, as.numeric(Winrate)),
                             y = 1)) +
    geom_text(aes(label = as.character(Hero))) +
    geom_segment(aes(y = 0.94, yend = 0.96,  xend = Hero)) +
    geom_segment(aes(y = 1.04, yend = 1.065, xend = Hero)) +
    coord_flip() +
    #ggtitle("Heroes") +
    xlab(NULL) + 
    ylab(NULL) +
    scale_x_discrete(breaks = NULL) +     #remove hero labels
    scale_y_continuous(expand = c(0,0),
                       limits = c(.94, 1.065), 
                       breaks = c(.94, 1.065),
                       oob = rescale_none) +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x  = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(12.5,-2,7.5,-2),"mm"))

gg1    <- ggplot_gtable(ggplot_build(ggPlayed))
gg2    <- ggplot_gtable(ggplot_build(ggWinrate))
gg.mid <- ggplot_gtable(ggplot_build(ggHeroes))

grid.arrange(gg1, gg.mid, gg2, ncol = 3, widths = c(4/9, 1.3/9, 4/9))

grid.text("Hero vs Popularity & Winrate (Post Kael'Thas, Pre-Johanna)", 
          x = .5, y = .98, gp = gpar(fontsize = 15, col = "grey20"))
