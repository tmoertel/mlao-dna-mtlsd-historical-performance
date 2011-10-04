#!/usr/bin/Rscript

##=============================================================================
## Prepare the raw PSSA data for analysis
## Tom Moertel <tom@mlao.org>
##
## Here I load the data sets and prepare them for my analysis.  I use
## 2002-2010 PSSA data from the PA Dept. of Education.
##=============================================================================

##=============================================================================
## First, I read in the data sets for math and reading
##=============================================================================

load_pssa <- function(name,
                      year,
                      math,    # math columns, advanced to below basic
                      reading, # reading columns, advanced to below basic
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
                   math_a     = as.numeric(df[[math[1]]]),
                   math_p     = as.numeric(df[[math[2]]]),
                   math_b     = as.numeric(df[[math[3]]]),
                   math_bb    = as.numeric(df[[math[4]]]),
                   reading_a  = as.numeric(df[[reading[1]]]),
                   reading_p  = as.numeric(df[[reading[2]]]),
                   reading_b  = as.numeric(df[[reading[3]]]),
                   reading_bb = as.numeric(df[[reading[4]]]))
  df
}

pssa_2002 <- load_pssa("2002MathandReadingperformancelevelsalldistricts.csv",
                       2002, math=8:11, reading=12:15)

pssa_2003 <- load_pssa("2003MathandReadingperformancelevelsalldistricts.csv",
                       2003, math=8:11, reading=12:15)

pssa_2004 <- load_pssa("2004MathandReadingperformancelevelsalldistricts.csv",
                       2004, math=8:11, reading=12:15)

pssa_2005 <- load_pssa("2005MathandReadingperformancelevelsalldistricts.csv",
                       2005, math=9:12, reading=14:17)

pssa_2006 <- load_pssa("2006 District Level PSSA Results.csv",
                       2006, math=8:11, reading=14:17, skip=3)

pssa_2007 <- load_pssa("2007 District Level PSSA Results.csv",
                       2007, math=7:10, reading=12:15, skip=4)

pssa_2008 <- load_pssa("2008 PostAYP District Level PSSA Results.csv",
                       2008, math=7:10, reading=12:15, skip=4)

pssa_2009 <- load_pssa("PSSA_Results_Math_and_Reading_District_2009.csv",
                       2009, math=8:11, reading=13:16, skip=3,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

pssa_2010 <- load_pssa("PSSA_Results_Math_and_Reading_District_2010.csv",
                       2010, math=8:11, reading=13:16, skip=2,
                       selector = function(df) {
                         subset(df, Group=="All Students")
                       })

pssa_2011 <- load_pssa("PSSA_Results_Math_and_Reading_District_2011.csv",
                       2011, math=8:11, reading=13:16, skip=3,
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
                     pssa_2010,
                     pssa_2011)


# Next, I write this composite data set into a file that I can
# further clean using Google Refine

write.csv(pssa_merged, file="pssa_math_reading_merged_raw.csv")

# After saving this data, I load it into Google Refine and apply the
# data-cleaning transformations specified in the file
# google-refine-prep.json, exporting the result, finally, as the file
# pssa-math-reading-merged-and-cleaned.csv.



##=============================================================================
## Next, I read in the data sets for writing
##=============================================================================

load_pssa_subj <-
  function(subject,
           file,
           year,
           cols,
           skip = 0,
           selector = function(df) df,
           aun = 1,
           county = 2,
           district = 4,
           grade = 5) {
    df <- read.csv(file,
                   skip = skip, na.strings = "#NULL!",
                   as.is=c(grade, cols))
    df <- subset(df, ! grepl("total", df[[grade]], ignore.case=T))
    df <- selector(df)
    df <- data.frame(year = year,
                     aun = df[[aun]],
                     county = df[[county]],
                     district = df[[district]],
                     grade = as.numeric(df[[grade]]),
                     a  = as.numeric(df[[cols[1]]]),
                     p  = as.numeric(df[[cols[2]]]),
                     b  = as.numeric(df[[cols[3]]]),
                     bb = as.numeric(df[[cols[4]]]))
    names(df)[6:9] <- paste(sep="_", subject, c("a", "p", "b", "bb"))
    df
  }

wr_2005 <-
  load_pssa_subj("writing",
                 "2005Districtlevelperformancelevelresults11thwriting.csv",
                 2005, cols=7:10)

wr_2006 <-
  load_pssa_subj("writing",
                 "2006 District Level Writing PSSA Results.csv",
                 2006, cols=7:10, skip=3)

wr_2007 <-
  load_pssa_subj("writing",
                 "2007 Writing District Level PSSA Results.csv",
                 2007, cols=7:10, skip=4)

wr_2008 <-
  load_pssa_subj("writing",
                 "2008 District Level Writing PSSA Results.csv",
                 2008, cols=7:10, skip=4)

wr_2009 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2009.csv",
                 2009, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

wr_2010 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2010.csv",
                 2010, cols=8:11, skip=2,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

wr_2011 <-
  load_pssa_subj("writing",
                 "PSSA_Results_Writing_District_2011.csv",
                 2011, cols=7:10, district=3, grade=4, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

# Then I merge them into a single, composite data set

wr_merged <- rbind(wr_2005,
                   wr_2006,
                   wr_2007,
                   wr_2008,
                   wr_2009,
                   wr_2010,
                   wr_2011)


# Next, I write this composite data set into a file that I can
# further clean using Google Refine

write.csv(wr_merged, file="pssa_writing_merged_raw.csv")

# After saving this data, I load it into Google Refine and apply the
# data-cleaning transformations specified in the file
# google-refine-prep.json, exporting the result, finally, as the file
# pssa-writing-merged-and-cleaned.csv.




##=============================================================================
## Next, I read in the data sets for science
##=============================================================================


sc_2008 <-
  load_pssa_subj("science",
                 "2008 District Level Science PSSA Results.csv",
                 2008, cols=7:10, skip=4)

sc_2009 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2009.csv",
                 2009, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

sc_2010 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2010.csv",
                 2010, cols=8:11, skip=2,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

sc_2011 <-
  load_pssa_subj("science",
                 "PSSA_Results_Science_District_2011.csv",
                 2011, cols=8:11, skip=3,
                 selector = function(df) {
                   subset(df, Group=="All Students")
                 })

# Then I merge them into a single, composite data set

sc_merged <- rbind(sc_2008,
                   sc_2009,
                   sc_2010,
                   sc_2011)


# Next, I write this composite data set into a file that I can
# further clean using Google Refine

write.csv(sc_merged, file="pssa_science_merged_raw.csv")

# After saving this data, I load it into Google Refine and apply the
# data-cleaning transformations specified in the file
# google-refine-prep.json, exporting the result, finally, as the file
# pssa-science-merged-and-cleaned.csv.
