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
library(directlabels)


##=============================================================================
## Analysis
##=============================================================================

## Load the merged, cleaned multi-year PSSA data and prepare it to be plotted

pssa_merged <- read.csv("data/pssa-merged-and-cleaned.csv")
pssa_merged <- transform(pssa_merged, grade=factor(grade), aun=factor(aun))

achievement_labels <- c("advanced", "proficient", "basic", "below basic")

pssa_melted <- melt(pssa_merged, 1:5)
pssa_melted <- subset(pssa_melted, !is.na(value))  # trim sds w/o results
pssa_melted <-
  transform(pssa_melted,
            subject = laply(strsplit(as.character(variable),"_"),identity)[,1],
            achievement = factor(
              laply(strsplit(as.character(variable), "_"), identity)[,2],
              levels = c("a", "p", "b", "bb"),
              labels = achievement_labels))

mtlsd_melted    <- subset(pssa_melted, district == "MT LEBANON SD")
mtlsd_melted_11 <- subset(mtlsd_melted, grade == "11")


## Plot MTLSD's 11th-grade PSSA results by achievement level (stacked ribbons)

p <-
qplot(year, value/100, data=mtlsd_melted_11,
      facets = subject ~ grade, geom="area", fill=achievement,
      main = "Mt. Lebanon School District: 11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1
      ) +
  geom_text(aes(label = achievement, x = year + 0.25),
            size = 3,
            hjust = 0,
            data = ddply(
              ddply(mtlsd_melted_11, 2:6,
                    subset, year == min(year)),
              .(grade, subject),
              transform,
              value = cumsum(value) - value/2)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("mtlsd-pssa-11gr-historical-achievement-stacked.pdf",
       plot=p, width=8.5, height=11)


## Plot rate of MTLSD's achievement decline since 2005

mtlsd_melted_11_2005_on <- subset(mtlsd_melted_11, year >= 2005)

p <-
qplot(year, value/100, data=mtlsd_melted_11_2005_on,
      facets = subject ~ grade, color=achievement,
      main = "Mt. Lebanon School District: 11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1,
      geom = c("point", "smooth"),
      method = "lm",
      shape = I(17)
      ) +
  geom_text(aes(label = achievement, x = year + 0.1),
            size = 3,
            hjust = 0, vjust = 0.5,
            data = ddply(
              ddply(subset(mtlsd_melted_11_2005_on), 2:6,
                    subset, year == min(year)),
              .(grade, subject),
              transform,
              value = value)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("mtlsd-pssa-11gr-historical-achievement-trend-since-2005.pdf",
       plot=p, width=8.5, height=11)


## Extract the rate of change, since 2005, in the portion of students
## at each level of achievement (11th grade)

daply(mtlsd_melted_11_2005_on, .(subject, achievement), function(df) {
  ## center on current yr to make intercept predict current yr's achievement
  df$year <- df$year - max(df$year)
  lm(value ~ year, data=df)$coeff
})



##############################################################################
# Comparative analysis to other top school districts

peers_melted <- subset(pssa_melted,
                       district == "UPPER SAINT CLAIR SD" |
                       district == "MT LEBANON SD" |
                       district == "NORTH ALLEGHENY SD" )

peers_melted_11 <- subset(peers_melted, grade == "11")


p <-
qplot(year, value/100, data=peers_melted_11,
      facets = subject ~ district, geom="area", fill=achievement,
      main = "Achievement in 11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1
      ) +
  geom_text(aes(label = achievement, x = year + 0.25),
            size = 3,
            hjust = 0,
            data = ddply(
              ddply(subset(peers_melted_11, district == "MT LEBANON SD"),
                    2:6, subset, year == min(year)),
              .(grade, subject, district),
              transform,
              value = cumsum(value) - value/2)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("peers-pssa-11gr-historical-achievement-stacked.pdf",
       plot=p, width=11, height=8.5)

peers_melted_11_2005_on <- subset(peers_melted_11, year >= 2005)

p <-
qplot(year, value/100, data=peers_melted_11_2005_on,
      facets = subject ~ district, color=achievement,
      main = "Achievement in 11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1,
      geom = c("point", "smooth"),
      method = "lm",
      se = F,
      shape = 17
      ) +
  geom_text(aes(label = achievement, x = year + 0.1),
            size = 3,
            color = "black",
            hjust = 0, vjust = 0.5,
            data = ddply(
              ddply(subset(peers_melted_11_2005_on, district == "MT LEBANON SD"),
                    2:6, subset, year == min(year)),
              .(grade, subject, district),
              transform,
              value = value)) +
  scale_y_continuous(formatter="percent") +
  theme_bw() +
  opts(legend.position = "none")

ggsave("peers-pssa-11gr-historical-achievement-trend-since-2005.pdf",
       plot=p, width=11, height=8.5)


##############################################################################
## Plot MTLSD's rank (among all state school districts) by portion of
## students who tested at the "advanced" achievement level



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


## Compute rankings
pssa_melted_a <- subset(pssa_melted, achievement == "advanced")
pssa_ecdf <- ddply(pssa_melted_a, .(year, grade, variable),
                   transform,
                   value_ecdf = ecdf(value)(value))

pssa_ecdf_extended <- local({
  df <- merge(pssa_ecdf, school_districts_of_interest, all.x=T)
  within(df, {
    sd <- factor(sd, levels = school_districts_of_interest$sd)
    others <- is.na(sd)
    color[others] <- other_color
    alpha[others] <- other_alpha
    sd[others] <- other_sd
  })
})


p <-
qplot(year, value_ecdf,
      main = "Mt. Lebanon Schools Academic Rank: 11th Grade PSSA Scores",
      ylab = paste(sep="\n",
        "Ranking among Pennsylvania school districts",
        "by portion of students testing at advanced level"),
      xlab = "Year",
      colour = sd,
      geom = c("line"),
      facets = subject ~ .,
      data = subset(pssa_ecdf_extended,
        sd != "Other" & grade == 11 & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  geom_text(aes(label = district, x = year + 0.075),
                data = subset(pssa_ecdf_extended,
                  sd != "Other" & grade == 11 & year == 2010),
                colour = "black",
                hjust = 0,
                size = 3) +
  scale_x_continuous(breaks = 2004:2010, limits = c(2004, 2012),
                     minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_point(shape=17) +
  ylim(.95, 1)  # focus on comparable group: top 5% of school districts

ggsave(file="mtlsd-pssa-11gr-rank-2004_2010.pdf",
       plot=p,
       width=8, height=10, dpi=100)

ggsave(file="mtlsd-pssa-11gr-rank-2004_2010.png",
       plot=p)



## Plot the ECDF

pssa_ecdf_reduced <-
  ddply(pssa_ecdf, .(year, grade, subject, achievement), function(df) {
    not_duped <- !duplicated(df$value)
    data.frame(value = sort(df$value[not_duped]),
               value_ecdf = sort(df$value_ecdf[not_duped]))
  })

p <-
qplot(value/100, value_ecdf,
      main = paste(sep="\n",
        "PSSA Advanced Performance (11th Grade)",
        "Relating local performance to statewide rankings"),
      ylab = "Ranking among Pennsylvania school districts",
      xlab = "Portion of students testing at advanced level",
      colour = sd,
      asp = 1,
      geom = c("step"),
      facets = grade ~ subject,
      data = subset(pssa_ecdf_reduced,
        grade == "11" & achievement == "advanced" & year == max(year))) +
  scale_x_continuous(formatter="percent") +
  scale_y_continuous(formatter="percent")

ggsave(file="mtlsd-pssa-11gr-ecdf.pdf",
       plot=p, width=11, height=7)



##############################################################################
## National Merit Scholarship Qualifying Test

nmsqt <- read.csv("data/mtlsd-nmsqt.csv", comment="#")

nmsqt_rel <- with(nmsqt, {
  data.frame(class         = class,
             semifinalist  = semifinalists / class_pool,
             commended     = commended / class_pool,
             any_honor     = (semifinalists + commended) / class_pool)
})

nmsqt_rel_m <- melt(nmsqt_rel, id="class")

p <-
qplot(class, value, data=nmsqt_rel_m,
      main = paste(sep="\n",
        "Mt. Lebanon School District Historical Performance",
        "on National Merit Scholarship Qualifying Test"),
      ylab = "Portion of class receiving honor",
      xlab = "Class",
      color=variable,
      shape=I(17),
      geom=c("point", "smooth"), method="lm", se=F) +
  geom_line(alpha=0.25) +
  scale_y_continuous(formatter="percent")

p <- direct.label(p, list("first.points", hjust=-.1, fontsize=4))

ggsave(file="mtlsd-nmsqt-2002_2011.png", plot=p)

ggsave(file="mtlsd-nmsqt-2002_2011.pdf", plot=p)



##############################################################################
##   W O R K S P A C E
##############################################################################

## Plot the MTLSD ranking among Pennsylvania school districts by
## portion of students testing at "advanced" level of achievement
## (all grades)

p <-
qplot(year, value_ecdf,
      main = "Mt. Lebanon Schools Academic Rank: PSSA Scores",
      ylab = paste(sep="\n",
        "Ranking among Pennsylvania school districts",
        "by portion of students testing at advanced level"),
      xlab = "Year",
      colour = sd,
      shape=I(17),
      data = subset(pssa_ecdf_extended, sd == "MTL" & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  scale_x_continuous(breaks = 2004:2010, limits = c(2004, 2012),
                     minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_line(alpha=0.1) +
  facet_grid(grade ~ variable)


## Plot the portion of MTLSD students who tested at the "advanced"
## level of achievement (11th grade)

p <-
qplot(year, value,
      main = "Mt. Lebanon Schools Academic Performance: PSSA Scores",
      ylab = "Portion of students testing at advanced level",
      xlab = "Year",
      colour = sd,
      geom = c("point", "smooth"),
      method = "lm",
      asp = 1,
      data = subset(pssa_ecdf_extended,
        grade == 11 & sd == "MTL" & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  scale_x_continuous(minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_line(alpha=0.1) +
  facet_grid(grade ~ variable)


## Plot the portion of MTLSD students who tested at the "advanced"
## level of achievement (11th grade)

p <-
qplot(year, value,
      main = "Mt. Lebanon Schools Academic Performance: PSSA Scores",
      ylab = "Portion of students testing at advanced level",
      xlab = "Year",
      colour = sd,
      geom = c("point", "smooth"),
      method = "lm",
      data = subset(pssa_ecdf_extended, sd == "MTL" & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  scale_x_continuous(minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_line(alpha=0.1) +
  facet_grid(grade ~ variable)

p <-
qplot(year, value_ecdf,
      main = "Mt. Lebanon Schools Academic Rank: PSSA Scores",
      ylab = paste(sep="\n",
        "Ranking among Pennsylvania school districts",
        "by portion of students testing at advanced level"),
      xlab = "Year",
      colour = sd,
      geom = c("point", "smooth"),
      method = "lm",
      data = subset(pssa_ecdf_extended, sd == "MTL" & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  scale_x_continuous(breaks = 2004:2010, limits = c(2004, 2012),
                     minor_breaks = F) +
  scale_y_continuous(minor_breaks = F) +
  geom_line(alpha=0.1) +
  facet_grid(grade ~ variable)

p <-
qplot(year, value_ecdf,
      main = "Mt. Lebanon Schools Academic Rank: 11th Grade PSSA Scores",
      ylab = paste(sep="\n",
        "Ranking among Pennsylvania school districts",
        "by portion of students testing at advanced level"),
      xlab = "Year",
      colour = sd,
      geom = c("point", "smooth"),
      method = "lm",
      facets = variable ~ .,
      data = subset(pssa_ecdf_extended,
        sd == "MTL" & grade == 11 & year >= 2004)) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  scale_x_continuous(minor_breaks = F) +
  scale_y_continuous(minor_breaks = F)


##=============================================================================
## writing
##=============================================================================

wr_merged <- read.csv("data/writing-merged-and-cleaned.csv")
wr_merged <- transform(wr_merged, grade=factor(grade), aun=factor(aun))

achievement_labels <- c("advanced", "proficient", "basic", "below basic")

wr_melted <- melt(wr_merged, 1:5)
wr_melted <- subset(wr_melted, !is.na(value))  # trim sds w/o results
wr_melted <-
  transform(wr_melted,
            subject = laply(strsplit(as.character(variable),"_"),identity)[,1],
            achievement = factor(
              laply(strsplit(as.character(variable), "_"), identity)[,2],
              levels = c("a", "p", "b", "bb"),
              labels = achievement_labels))

peers_wr_melted <- subset(wr_melted,
                          district == "UPPER SAINT CLAIR SD" |
                          district == "MT LEBANON SD" |
                          district == "NORTH ALLEGHENY SD" )

peers_wr_melted_11 <- subset(peers_wr_melted, grade == "11")


p <-
qplot(year, value/100, data=peers_wr_melted_11,
      facets = subject ~ district, geom="area", fill=achievement,
      main = "Achievement in 11th Grade PSSA Writing",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1
      ) +
  geom_text(aes(label = achievement, x = year + 0.25),
            size = 3,
            hjust = 0,
            data = ddply(
              ddply(subset(peers_wr_melted_11, district == "MT LEBANON SD"),
                    2:6, subset, year == min(year)),
              .(grade, subject, district),
              transform,
              value = cumsum(value) - value/2)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("peers-wr-11gr-historical-achievement-stacked.pdf",
       plot=p, width=11, height=8.5)



p <-
qplot(year, value/100, data=peers_wr_melted_11,
      facets = subject ~ district, color=achievement,
      main = "Achievement in 11th Grade PSSA Writing",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1,
      geom = c("point", "smooth"),
      method = "lm",
      se = F,
      shape = 17
      ) +
  geom_text(aes(label = achievement, x = year + 0.1),
            size = 3,
            color = "black",
            hjust = 0, vjust = 0.5,
            data = ddply(
              ddply(subset(peers_wr_melted_11,
                           district == "MT LEBANON SD"),
                    2:6, subset, year == min(year)),
              .(grade, subject, district),
              transform,
              value = value)) +
  scale_y_continuous(formatter="percent") +
  theme_bw() +
  opts(legend.position = "none")

ggsave("peers-wr-11gr-historical-achievement-trend-since-2005.pdf",
       plot=p, width=11, height=8.5)
