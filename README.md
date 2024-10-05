
# 🗳️ Plausi Package
** Detect Anomalies in Vote-Results - with a little help of Statistics & Machine Learning**

![](https://opendata.swiss/content/uploads/2016/02/kt_zh.png)

<details>
<summary>Contents</summary>

- [Usage](#usage)
- [What does the score mean?](#what-does-the-score-mean)
- [How does the score work?](#how-does-the-score-work)
- [Background](#background)
- [Licensing](#licensing)
- [Project team](#project-team)
- [Feedback and contributing](#feedback-and-contributing)

## Usage
- Install R [https://www.r-project.org/](https://www.r-project.org/) 
- Install the requiered dependencies : `python -m spacy download de_core_news_sm`

**1. Install the plausi-Package**

Installation from gitlab:

__remotes::install_url("https://gitlab.com/plausi_pkg/plausi.git")__

**2. Explore the methodology in the Documentation**

## R-Package für die Plausibilisierung von Abstimmungsresultaten

Das plausi-package dient der R-gestützten Abstimmungsforensik. Es enthält Funktionen die es erlauben statistische Auffälligkeiten und Anomalien in Abstimmungsresultaten zu identifizieren. 

- robuste Ausreissererkennung für kleine Fallzahlen wie auch schiefe Verteilungen
- Berechnung von Differenzen zwischen allen möglichen Kombinationen von Vorlagen (z.B. zwecks systematischem Vergleich der Stimmbeteiligung verschiedener Vorlagen über alle Auszählkreise hinweg)
- Vorhersage von zu erwartenden Resultaten via unterschiedlichen Machine-Learning Algorithmen (Ja-Anteile, Stimmbeteiligung etc.)

## Licensing
- All code, the index formula and the synthetic text data are licensed under the MIT license. 

## Project team
This is a project of [Team Data of the Statistical Office of the Canton of Zurich](https://www.zh.ch/de/direktion-der-justiz-und-des-innern/statistisches-amt/data.html). Responsible: Simon Graf, Thomas Lo Russo and Thomas Knecht

## Feedback and contributing
We would love to hear from you. Please share your feedback and let us know how you use the code. You can [write an email](mailto:datashop@statistik.zh.ch) or share your ideas by opening an issue or a pull requests.

