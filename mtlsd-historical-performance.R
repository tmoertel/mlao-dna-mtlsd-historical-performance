#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of Historical Performance of
## the Mt. Lebanon, Pennsylvania, School District.
##
## Primary sources:  2002-12 PSSA data
##
## Analysis by the Mt. Lebanon Accountability Organization
## Tom Moertel <tom@mlao.org>
## http://www.mlao.org/
##
## 2010-05-30 / Updated 2010-12-23 / Updated 2011-10-03 / Updated 2012-11-23
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

achievement_labels <- c("advanced", "proficient", "basic", "below basic")

pssa <- read.csv("data/pssa-all-merged-and-cleaned.csv")
pssa <- transform(pssa, aun = NULL, county = NULL)
pssa <- melt(pssa, 1:4)
pssa <- subset(pssa, !is.na(value))  # trim sds w/o results
pssa <- transform(pssa,
                  grade = factor(grade),
                  value = value/100,  # 34 => 34% = 0.34
                  variable = NULL,
                  achievement = factor(
                    variable,
                    levels = c("a", "p", "b", "bb"),
                    labels = achievement_labels))


## Extract convenient subsets for MTSLD and peers

mtlsd           <- "MT LEBANON SD"
peers           <- c("UPPER SAINT CLAIR SD", "NORTH ALLEGHENY SD")
mtlsd_and_peers <- c(mtlsd, peers)

pssa_peers      <- subset(pssa, district %in% mtlsd_and_peers)
pssa_peers_11   <- subset(pssa_peers, grade == "11")

pssa_mtlsd      <- subset(pssa_peers, district == mtlsd)
pssa_mtlsd_11   <- subset(pssa_mtlsd, grade == "11")


## Plot MTLSD's 11th-grade PSSA results by achievement level (stacked ribbons)

p <-
qplot(year, value, data=pssa_mtlsd_11,
      facets = subject ~ ., geom="area", fill=achievement,
      main = "Mt. Lebanon School District:\n11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1
      ) +
  geom_text(aes(label = achievement, x = year + 0.25),
            size = 3,
            hjust = 0,
            data = transform(
              subset(pssa_mtlsd_11,
                     year == min(year) & subject == min(levels(subject))),
              value = cumsum(value) - value/2)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("mtlsd-pssa-11gr-historical-achievement-stacked.pdf",
       plot=p, width=3.75, height=11, useDingbats=F)


## Plot rate of change in MTLSD achievement portions since 2005

do_mtlsd_analysis_from_base_year <- function(base_year, min_year = 2005) {

  pssa_mtlsd_11_base_year_on <- subset(pssa_mtlsd_11, year >= base_year)

  x_max <- max(pssa_mtlsd_11_base_year_on$year)
  y_max <- max(subset(pssa_mtlsd_11, year >= min_year)$value)

  p <-
  qplot(year, value, data=pssa_mtlsd_11_base_year_on,
        facets = subject ~ grade, color=achievement,
        main = paste("Mt. Lebanon 11th Grade\nPSSA since", base_year),
        xlab = "Year",
        ylab = "Portion of students testing at given level of achievement",
        asp = 1,
        geom = c("point", "smooth"),
        method = "lm"
        ) +
    geom_text(aes(label = achievement, x = year + 0.1),
              size = 3,
              hjust = 0, vjust = 0.5,
              data = subset(pssa_mtlsd_11_base_year_on,
                       year == min(year) & subject == min(levels(subject)))) +
    scale_y_continuous(formatter = "percent",
                       limits = c(0, y_max)) +
    xlim(min_year, x_max) +
    opts(legend.position = "none") +
    opts(axis.text.x = theme_text(angle = -90, hjust = 0))


  ggsave(paste(sep = "",
               "mtlsd-pssa-11gr-historical-achievement-trend-since-",
               base_year, ".pdf"),
         plot=p, width=3.75, height=11, useDingbats=F)


  ## Extract the rate of change, since base_year, in the portion of students
  ## at each level of achievement (11th grade)

  print(paste("Analysis from linear model for base year =", base_year))
  daply(pssa_mtlsd_11_base_year_on, .(subject, achievement), function(df) {
    ## center on current yr to make intercept predict current yr's achievement
    df$year <- df$year - max(df$year)
    lm(value ~ year, data=df)$coeff
  })
}

do_mtlsd_analysis_from_base_year(2005)
do_mtlsd_analysis_from_base_year(2006)
do_mtlsd_analysis_from_base_year(2007)
do_mtlsd_analysis_from_base_year(2008)



##############################################################################
# Comparative analysis to other top school districts


p <-
qplot(year, value, data=pssa_peers_11,
      facets = subject ~ district, geom="area", fill=achievement,
      main = "Achievement in 11th Grade PSSA",
      xlab = "Year",
      ylab = "Portion of students testing at given level of achievement",
      asp = 1
      ) +
  geom_text(aes(label = achievement, x = year + 0.25),
            size = 3,
            hjust = 0,
            data = transform(
              subset(pssa_mtlsd_11,
                     year == min(year) & subject == min(levels(subject))),
              value = cumsum(value) - value/2)) +
  scale_y_continuous(formatter="percent") +
  opts(legend.position = "none")

ggsave("peers-pssa-11gr-historical-achievement-stacked.pdf",
       plot=p, width=6.5, height=8.5, useDingbats=F)


do_peers_analysis_from_base_year <- function(base_year, min_year = 2005) {

  pssa_peers_11_base_year_on <- subset(pssa_peers_11, year >= base_year)
  pssa_mtlsd_11_base_year_on <- subset(pssa_mtlsd_11, year >= base_year)

  x_max <- max(pssa_peers_11_base_year_on$year)
  y_max <- max(subset(pssa_peers_11, year >= min_year)$value)

  p <-
    qplot(year, value, data=pssa_peers_11_base_year_on,
          facets = subject ~ district, color=achievement,
          main = paste("Achievement in 11th Grade PSSA since", base_year),
          xlab = "Year",
          ylab = "Portion of students testing at given level of achievement",
          asp = 1,
          geom = c("point", "smooth"),
          method = "lm",
          se = F
          ) +
      geom_text(aes(label = achievement, x = year + 0.1),
                size = 3,
                color = "black",
                hjust = 0, vjust = 0.5,
                data = subset(pssa_mtlsd_11_base_year_on,
                  year == min(year) & subject == min(levels(subject)))) +
      scale_y_continuous(formatter="percent") +
      xlim(min_year, x_max) +
      theme_bw() +
      coord_cartesian(ylim = c(0, y_max + 0.05)) +
      opts(legend.position = "none") +
      opts(axis.text.x = theme_text(angle = -90, hjust = 0))

  ggsave(paste(sep = "",
               "peers-pssa-11gr-historical-achievement-trend-since-",
               base_year, ".pdf"),
         plot=p, width=6.5, height=8.5, useDingbats=F)
}

do_peers_analysis_from_base_year(2005)
do_peers_analysis_from_base_year(2006)
do_peers_analysis_from_base_year(2007)
do_peers_analysis_from_base_year(2008)



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


## Compute rankings for advanced portions

pssa_adv      <- subset(pssa, achievement == "advanced")
pssa_adv_ecdf <- ddply(pssa_adv, .(year, grade, subject, achievement),
                       transform,
                       value_ecdf = ecdf(value)(value))

pssa_adv_ecdf_extended <- local({
  df <- merge(pssa_adv_ecdf, school_districts_of_interest, all.x=T)
  within(df, {
    sd <- factor(sd, levels = school_districts_of_interest$sd)
    others <- is.na(sd)
    color[others] <- other_color
    alpha[others] <- other_alpha
    sd[others] <- other_sd
  })
})

pssa_rank_11 <- subset(pssa_adv_ecdf_extended, sd != "Other" & grade == 11)
pssa_rank_11_year_min <- min(pssa_rank_11$year)
pssa_rank_11_year_max <- max(pssa_rank_11$year)

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
      data = pssa_rank_11) +
  scale_colour_manual(name = "School District",
                      values = subject_districts$color,
                      breaks = subject_districts$sd,
                      legend = F) +
  geom_text(aes(label = district, x = year + 0.075),
                data = subset(pssa_rank_11,
                  sd != "Other" & grade == 11 & year == 2011),
                colour = "black",
                hjust = 0,
                size = 3) +
  scale_x_continuous(breaks = pssa_rank_11_year_min:pssa_rank_11_year_max,
                     limits = c(pssa_rank_11_year_min,
                                pssa_rank_11_year_max + 3),
                     minor_breaks = F) +
  scale_y_continuous(minor_breaks = F, breaks = 95:100 / 100) +
  geom_point() +
  coord_cartesian(ylim = c(0.945, 1.005))  # focus on peer group in top 5%

ggsave(file="mtlsd-pssa-11gr-rank-2004_2011.pdf",
       plot=p,
       width=8, height=10, dpi=100, useDingbats=F)

ggsave(file="mtlsd-pssa-11gr-rank-2004_2011.png",
       plot=p)



## Plot the ECDF

pssa_adv_ecdf_reduced <-
  ddply(pssa_adv_ecdf, .(year, grade, subject, achievement), function(df) {
    not_duped <- !duplicated(df$value)
    data.frame(value = sort(df$value[not_duped]),
               value_ecdf = sort(df$value_ecdf[not_duped]))
  })

p <-
qplot(value, value_ecdf,
      main = paste(sep="\n",
        "PSSA Advanced Performance (11th Grade)",
        "Local performance vs. statewide rankings"),
      ylab = "Ranking among Pennsylvania school districts",
      xlab = "Portion of students testing at advanced level",
      colour = sd,
      asp = 1,
      geom = c("step"),
      facets = subject ~ grade,
      data = subset(pssa_adv_ecdf_reduced,
        grade == "11" & achievement == "advanced" & year == max(year))) +
  scale_x_continuous(formatter="percent") +
  scale_y_continuous(formatter="percent")

ggsave(file="mtlsd-pssa-11gr-ecdf.pdf",
       plot=p, width=4, height=7, useDingbats=F)



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
      color = variable,
      geom=c("point", "smooth"), method="lm", se=F) +
  geom_line(alpha=0.25) +
  scale_y_continuous(formatter="percent")

p <- direct.label(p, list("first.points", hjust=-.1, fontsize=4))

ggsave(file="mtlsd-nmsqt-2002_2011.png", plot=p)

ggsave(file="mtlsd-nmsqt-2002_2011.pdf", plot=p, useDingbats=F)


quit(save = "no")  # end of batch analysis
