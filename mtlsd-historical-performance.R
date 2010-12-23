#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of Historical Performance of
## the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2001-10 PSSA data
##
## Analysis by the Mt. Lebanon Accountability Organization
## Tom Moertel <tom@mlao.org>
## http://www.mlao.org/
##
## 2010-05-30 / Updated 2010-12-23
##
## This analysis is an R program:  http://www.r-project.org/
##=============================================================================
##=============================================================================


options(digits = 2)

library(ggplot2)


##=============================================================================
## Configuration
##=============================================================================


## Graphical preferences

interest_alpha <- 0.5
other_alpha <- 0.25
other_color <- "darkgrey"
other_sd <- "Other"


## Which schools to highlight

school_districts_of_interest <- local({
  x <- matrix(ncol=4, byrow=T,
              c("MT LEBANON SD",        "MTL",    "blue",      interest_alpha,
                "UPPER SAINT CLAIR SD", "USC",    "red",       interest_alpha,
                "NORTH ALLEGHENY SD",   "N ALG",  "brown",     interest_alpha,
#                "BETHEL PARK SD",       "BTHL PK","green",     interest_alpha,
#                "FOX CHAPEL AREA SD",   "FOX CH", "orange",    interest_alpha,
#                "QUAKER VALLEY SD",     "QKR VLY","purple",    interest_alpha,
                NA,                     other_sd, other_color, other_alpha))
  x <- as.data.frame(x)
  names(x) <- c("district", "sd", "color", "alpha")
  x
})

subject_districts <- subset(school_districts_of_interest, !is.na(district))



##=============================================================================
## Prepare the data for analysis
##
## Here I load the data sets and prepare them for my analysis.  I use
## 2002-2010 PSSA data from the PA Dept. of Education.
##=============================================================================

## First, I read in the data sets

load_pssa <- function(name,
                      year,
                      math,
                      reading,
                      skip = 0,
                      selector = function(df) df,
                      aun = 1,
                      county = 2,
                      district = 4,
                      grade = 5) {
  df <- read.csv(paste(sep="", "data/", name),
                 skip = skip, na.strings = "#NULL!",
                 as.is=c(grade, math, reading))
  df <- subset(df, ! grepl("total", df[[grade]], ignore.case=T))
  df <- selector(df)
  df <- data.frame(year = year,
                   aun = df[[aun]],
                   county = df[[county]],
                   district = df[[district]],
                   grade = as.numeric(df[[grade]]),
                   math = as.numeric(df[[math]]),
                   reading = as.numeric(df[[reading]]))
}

pssa_2002 <- load_pssa("2002MathandReadingperformancelevelsalldistricts.csv",
                       2002, math=8, reading=12)

pssa_2003 <- load_pssa("2003MathandReadingperformancelevelsalldistricts.csv",
                       2003, math=8, reading=12)

pssa_2004 <- load_pssa("2004MathandReadingperformancelevelsalldistricts.csv",
                       2004, math=8, reading=12)

pssa_2005 <- load_pssa("2005MathandReadingperformancelevelsalldistricts.csv",
                       2005, math=9, reading=14)

pssa_2006 <- load_pssa("2006 District Level PSSA Results.csv",
                       2006, math=8, reading=14, skip=3)

pssa_2007 <- load_pssa("2007 District Level PSSA Results.csv",
                       2007, math=7, reading=12, skip=4)

pssa_2008 <- load_pssa("2008 PostAYP District Level PSSA Results.csv",
                       2008, math=7, reading=12, skip=4)

pssa_2009 <- load_pssa("PSSA_Results_Math_and_Reading_District_2009.csv",
                       2009, math=8, reading=13, skip=3,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

pssa_2010 <- load_pssa("PSSA_Results_Math_and_Reading_District_2010.csv",
                       2010, math=8, reading=13, skip=2,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })


pssa_merged <- rbind(pssa_2002,
                     pssa_2003,
                     pssa_2004,
                     pssa_2005,
                     pssa_2006,
                     pssa_2007,
                     pssa_2008,
                     pssa_2009,
                     pssa_2010)

pssa_merged_extended <- local({
  df <- merge(pssa_merged, school_districts_of_interest, all.x=T)
  within(df, {
    sd <- factor(sd, levels = school_districts_of_interest$sd)
    # grade <- factor(grade, labels = paste("Grade", sort(as.numeric(unique(grade)))))
    others <- is.na(sd)
    color[others] <- other_color
    alpha[others] <- other_alpha
    sd[others] <- other_sd
  })
})


pssa_melted <- melt(pssa_merged_extended, measure.vars = c("math", "reading"))

pssa_melted <- subset(pssa_melted, !is.na(value))

pssa_ecdf <- ddply(pssa_melted, .(year, grade, variable),
                   transform,
                   value_ecdf = ecdf(value)(value))


p <-
qplot(year, value_ecdf,
      main = "Mt. Lebanon Schools Academic Rank: 11th Grade PSSA Scores",
      ylab = paste(sep="\n",
        "Ranking among Pennsylvania school districts",
        "by portion of students testing at advanced level"),
      xlab = "Year",
      colour = sd,
      geom = c("line"),
      facets = variable ~ .,
      data = subset(pssa_ecdf, sd != "Other" & grade == 11 & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  geom_text(aes(label = district, x = year + 0.075),
                data = subset(pssa_ecdf,
                  sd != "Other" & grade == 11 & year == 2010),
                colour = "black",
                hjust = 0,
                size = 3) +
  scale_x_continuous(breaks = 2004:2010, limits = c(2004, 2012),
                     minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_point(shape=17)

p + ylim(.95, 1)  # focus on our comparable group: top 5% of school districts

ggsave(file="mtlsd-pssa-rank-2004_2010-grade_11.pdf",
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-pssa-rank-2004_2010-grade_11.png")
