##=============================================================================
## Prepare the raw PSSA data for analysis
## Tom Moertel <tom@mlao.org>
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
  df <- read.csv(name,
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

# Then I merge them into a single, composite data set

pssa_merged <- rbind(pssa_2002,
                     pssa_2003,
                     pssa_2004,
                     pssa_2005,
                     pssa_2006,
                     pssa_2007,
                     pssa_2008,
                     pssa_2009,
                     pssa_2010)


# Next, I write this composite data set into a file that I can
# further clean using Google Refine

write.csv("pssa_merged_raw.csv")

# After saving this data, I load it into Google Refine and apply the
# data-cleaning transformations specified in the file
# google-refine-prep.json, exporting the result, finally, as the file
# pssa-merged-and-cleaned.csv.
