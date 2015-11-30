# Stat E139 Project Proposal

Team:

Nathaniel Burbank nathaniel.burbank@gmail.com

Pooja Singh goodwillyoga@gmail.com

David Wihl davidwihl@gmail.com

November 30, 2015

### Outline

We plan to analyze Boston Marathon finish data for the years 2010 to 2015 in order to:

* Compare strata across years such as gender or age groups 
* Attempt to predict runners who drop out of the race based on increasing split times
* Perform a regression to predict finish time based only on 5k split, age and gender
* Analyze the effects of heat and running course by comparing runner performance between different years of the Boston Marathon and by contrasting the Boston marathon with the Chicago Marathon, which features a much flatter course. 

### Completed So Far

* Set up team organization ([Trello board](https://trello.com/b/i6X6vm4s/statse139-group-project), [github repository](https://github.com/wihl/statse139-project))
* Secured split time data for the three years (2010,2011,2012), and finish time data for five years (2010-2015)
* Downloaded historical weather data for Boston Marathon dates 
* Decided on toolset (R Markdown)
* Performed initial regressions with log and polynomial transforms


### Remaining To Do

* Perform initial Anova analysis between the five years of Boston Marathon data, comparing the finish times from 2010,2011,2013,2014, and 2014 with 2012, when temperatures reached more than 90 degrees to study the affect the heat had on running performance 
* Obtain finish time and splittime data for the Chicago Marathon 
* Improve regressions by both transforms and attempting different regression models such as Random Forest
* Write up conclusions

### Challenges Faced

* The split time data we have access to has already been filtered to remove runners who started but did not complete the 2010 and 2011 data. This will make attempting to predict runners who won't finish difficult. 
* add 2
* Finish times are not linear with regards to split times, so some form of transformation will necessary

