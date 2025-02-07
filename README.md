# EC 421, Winter 2025

Welcome to **Economics 421: Introduction to Econometrics** (Winter 2025) at the University of Oregon (taught by [Edward Rubin](https://edrub.in)).

## Syllabus

For information on the course specifics, please see the [syllabus](https://raw.githack.com/edrubin/EC421W25/master/syllabus/syllabus.pdf).

## Assignments

**Problem Set 0: Review**
<br>Due: Tuesday, 28 January 2025 by 11:59 PM. Submit via [Canvas](https://canvas.uoregon.edu/courses/254921/assignments/1793531).
<br>Files: [assignment](https://raw.githack.com/edrubin/EC421W25/master/problem-sets/000/000-questions.html) | [data](https://github.com/edrubin/EC421W25/tree/master/problem-sets/000/data-acs.csv)

**Problem Set 1: Heteroskedasticity, Clustering, and OLS Assumptions**
<br>Due: Saturday, 08 February 2025 by 11:59 PM. Submit via [Canvas](https://canvas.uoregon.edu/courses/254921/assignments/1793531).
<br>Files: [assignment](https://raw.githack.com/edrubin/EC421W25/master/problem-sets/001/001-questions.html) | [data](https://github.com/edrubin/EC421W25/tree/master/problem-sets/001/data-acs.csv)

*Note:* You can use the box in the top right corner with the `⋯` to download the files. (You can also use the button with the download arrow.)

## Lecture slides

The slides below (linked by their topic) are .html files that will only work properly if you are connected to the internet. If you're going off grid, grab the PDFs (you'll miss out on gifs and interactive plots, but the equations will render correctly).

**Note:** Links to topics that we have not yet covered lead to older slides. I will update links to the new slides as we work our way through the term/slides.

In case you're interested, I created the slides with [`xaringan`](https://github.com/yihui/xaringan/wiki) in [R](cran.r-project.org). If you are thinking of making your own slides/documents, I would suggest [quarto](https://quarto.org/).

1. [Introduction to "Introduction to Econometrics"](https://raw.githack.com/edrubin/EC421W25/master/notes/01-intro/slides.html) <br> [PDF](https://raw.githack.com/edrubin/EC421W25/master/notes/01-intro/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W25/blob/master/notes/01-intro/slides.rmd)

2. [Review of key math/stat/metrics topics](https://raw.githack.com/edrubin/EC421W25/master/notes/02-review/slides.html)<br>Density functions, deriving the OLS estimators, properties of estimators, statistical inference (standard errors, confidence intervals, hypothesis testing), simulation <br> [PDF](https://raw.githack.com/edrubin/EC421W25/master/notes/02-review/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W25/blob/master/notes/02-review/slides.rmd)

3. [Review of key topics from EC320](https://raw.githack.com/edrubin/EC421W25/master/notes/03-review/slides.html)<br>(the first course in our intro-to-metrics sequence) <br> [PDF](https://raw.githack.com/edrubin/EC421W25/master/notes/03-review/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W25/blob/master/notes/03-review/slides.rmd)

4. [Heteroskedasticity: Tests and implications](https://raw.githack.com/edrubin/EC421W22/master/notes/04-heteroskedasticity/slides.html) <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/04-heteroskedasticity/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/04-heteroskedasticity/slides.rmd)

5. [Heteroskedasticity: Living with it](https://raw.githack.com/edrubin/EC421W22/master/notes/05-heteroskedasticity/slides.html) <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/05-heteroskedasticity/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/05-heteroskedasticity/slides.rmd)

6. [Consistency and OLS in asymptopia](https://raw.githack.com/edrubin/EC421W22/master/notes/06-consistency/slides.html) <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/06-consistency/slides.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/06-consistency/slides.rmd)

7. [Introduction to time series](https://raw.githack.com/edrubin/EC421W22/master/notes/07-time-series/07-time-series.html) <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/07-time-series/07-time-series.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/07-time-series/07-time-series.Rmd)

8. [Autocorrelated disturbances](https://raw.githack.com/edrubin/EC421W22/master/notes/08-autocorrelation/08-autocorrelation.html)<br>Implications, testing, and estimation. Also: introduction `ggplot2` and user-defined functions. <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/08-autocorrelation/08-autocorrelation.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/08-autocorrelation/08-autocorrelation.Rmd)

9. [Nonstationarity](https://raw.githack.com/edrubin/EC421W22/master/notes/09-nonstationarity/09-nonstationarity.html)<br>Introduciton, implications for OLS, testing, and estimation. Also: in-class exercise for model selection. <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/09-nonstationarity/09-nonstationarity.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/09-nonstationarity/09-nonstationarity.Rmd)

10. [Causality](https://raw.githack.com/edrubin/EC421W22/master/notes/10-causality/10-causality.html)<br>Introduction to causality and the Neymam-Rubin causal model. Also: Recap of in-class model-selection exercise. <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/10-causality/10-causality.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/10-causality/10-causality.Rmd)

11. [Instrumental Variables](https://raw.githack.com/edrubin/EC421W22/master/notes/11-iv/11-iv.html)<br>Review the Neymam-Rubin causal model; introduction to instrumental variables (IV) and two-stage least squares (2SLS). Applications to causal inference and measurement error. Venn diagrams. <br> [PDF](https://raw.githack.com/edrubin/EC421W22/master/notes/11-iv/11-iv.pdf) | [.Rmd](https://github.com/edrubin/EC421W22/blob/master/notes/11-iv/11-iv.Rmd)

## Exams

See the [syllabus](https://raw.githack.com/edrubin/EC421W25/master/syllabus/syllabus.pdf) for specific information on the exams and grades.

Here are some exams from previous years:

| Term | Midterm | Final |
|:----:|:-------:|:-----:|
| Winter 2019 | [exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2019w.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2019w-key.pdf) | [exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2019w.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2019w-key.pdf) |
| Spring 2019 | [exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2019s.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2019s-key.pdf) | [exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2019s.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2019s-key.pdf) |
| Winter 2020 | [exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2020w.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2020w-key.pdf) | [exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2020w.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2020w-key.pdf) |
| Winter 2021 | [exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2021w.pdf) |  |
| Spring 2020 | [exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2020s.pdf) | [exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2020s.pdf) [key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2020s-key.pdf) |
| Winter 2022 |  | [home exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2022w-home.pdf) [home key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2022w-home-key.html) |
| Spring 2022 |  [home exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2022s-home.pdf) [home key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2022s-home-key.pdf) |  |
| Winter 2023 | [home key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023w-home-key.html) [in-class exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023w-inclass.pdf) [in-class key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023w-inclass-key.pdf) | [home exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023w-home.html) [home key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023w-home-key.html) [in-class exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023w-inclass.pdf) [in-class key](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023w-inclass-key.pdf) |
| Spring 2023 | [home exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023s-home.html) [home key](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023s-home-key.html) [in-class exam](https://raw.githack.com/edrubin/EC421W25/master/midterm/past/midterm-2023s-inclass.pdf) | [home exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023s.html) [in-class exam](https://raw.githack.com/edrubin/EC421W25/master/final/past/final-2023s-inclass.pdf) |

**Note:** If there is no key posted, then I do not have it and will not distribute it.

## Previous years

Here are links to previous years' course materials as well:

- [Winter 2022](https://github.com/edrubin/EC421W22)
- [Winter 2021](https://github.com/edrubin/EC421W21)
- [Spring 2020](https://github.com/edrubin/EC421S20)
- [Winter 2020](https://github.com/edrubin/EC421W20)
- [Spring 2019](https://github.com/edrubin/EC421S19)
- [Winter 2019](https://github.com/edrubin/EC421W19)

## Homework

Please also see the [syllabus](https://raw.githack.com/edrubin/EC421W25/master/syllabus/syllabus.pdf) for specific information on the homework and grade policies.


