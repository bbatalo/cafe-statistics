# Student cafe statistics and data analysis

Basic statistical analysis of a cafe in Indiana for a student project in statistics. The premise of the project is to recreate statistical analysis performed in a scientific paper, and add some extra on top of it. This project is based on a paper that analyses data from a cafe run by a group of students enrolled in business course.

## Getting started

To get started with running the project, it is recommended to first read the paper (link below). Understanding of data and the circumstances surrounding it can be prove invaluable.

The paper - https://ww2.amstat.org/publications/jse/v19n1/depaolo.pdf
The data - http://www.amstat.org/publications/jse/v19n1/cafedata.xls

### Prerequisites

* R - latest version prefered

### Project structure

* stats.R - main script containing all the analysis
* util.R - utility script for functions that make life easier

## Statistical analysis performed

A brief overview of analysis reproduced from the paper and added on top of that.

Descriptive statistics:

* Statistics for total coffees and sodas sold
* Statistics for coffees and sodas sold per days of the week
* Statistics for other items per days of the week

Hypothesis testing:
* Normality testing for most of items, total and per days
* Correlation testing of soda and coffee sales against temperature and time (and for some other items)
* Analysis of variance of soda and coffee sales per days of the week (and for some other items)

Time-series:
* Construction of basic time-series models for most items

Regressions:
* Construction of basic reggresion models for most items
* Construction of multiple-regression models for most items

