
# 🗳️ Plausi Package
**Detect Anomalies in Vote-Results - with the power of Statistics & Machine Learning**

![](https://opendata.swiss/content/uploads/2016/02/kt_zh.png)

<details>
<summary>Contents</summary>

- [Usage](#usage)
- [What does the Plausi-Package do?](#what-does-the-plausi-package-do)
- [Licensing](#licensing)
- [Project team](#project-team)
- [Feedback and contributing](#feedback-and-contributing)

## Usage
- Install R [https://www.r-project.org/](https://www.r-project.org/) 

**1. Install the plausi-Package**

Installation from gitlab:

__remotes::install_url("https://gitlab.com/plausi_pkg/plausi.git")__

**2. Explore the methodology in the Documentation**

https://plausi.gitlab.io/plausi_pkg/

## What does the Plausi Package do?

The Plausi package is designed for R-supported election forensics. It provides functions that enable the identification of statistical irregularities and anomalies in vote results.

Key features include:

- Robust outlier detection for small sample sizes and skewed distributions
- Calculation of differences between all possible combinations of turnout-levels (e.g., for systematic comparison of voter turnout across all voting districts)
- Prediction of expected results using by machine learning algorithms (e.g., yes-vote proportions, voter turnout, etc.)

It serves as a basis for the ___PlausiApp___, which is used for vote result quality controll in different Cantons (TG / SG / ZH).

For the moment, the ___PlausiApp___ is made available upon request via a private Repo (mailto:wahlen@statistik.zh.ch). 

## Licensing
The package is licensed under the MIT license. 

## Project team
This is a joint project of the [Vote & Election-Team](https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/wahlen-und-abstimmungen.html) and [Team Data](https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/data.html) of the Statistical Office of the Canton of Zurich. Responsible: Simon Graf, Thomas Lo Russo and Thomas Knecht

## Feedback and contributing
We would love to hear from you. Please share your feedback and let us know how you use the code. You can [write an email](mailto:wahlen@statistik.zh.ch) or share your ideas by opening an issue or a pull requests.

