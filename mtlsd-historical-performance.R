#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of Historical Performance of
## the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2002-10 PSSA data
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


## Load the data and prepare it to be plotted

pssa_merged <- read.csv("data/pssa-merged-and-cleaned.csv")

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

## Plot the data

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
  geom_point(shape=17) +
  ylim(.95, 1)  # focus on comparable group: top 5% of school districts

ggsave(file="mtlsd-pssa-rank-2004_2010-grade_11.pdf",
       plot=p,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-pssa-rank-2004_2010-grade_11.png",
       plot=p)
