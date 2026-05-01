# Spatial Criminology: An Undergraduate Reading Course

**Prepared for:** Summer / Fall Independent Study
**Level:** Advanced Undergraduate
**Prerequisites:** Introductory statistics; basic familiarity with R helpful but not required

---

## Overview

This course introduces spatial data science tools in R and applies them to criminological questions. The structure moves from computational foundations (how to wrangle, map, and analyze spatial data) toward substantive criminological modeling (hotspots, risk terrain, spatiotemporal prediction). Students should expect to read actively — not just follow tutorials passively — and to begin building a small empirical project by mid-course.

---

## Part I: Spatial Data Science Foundations

The goal of this section is to get comfortable with spatial data in R: reading Census variables, making maps, detecting spatial patterns, and eventually fitting a simple spatial model. Work through these roughly in order.

---

### 1.1 R Basics (skip if already comfortable)

**[A Beginner's Guide to R](https://dattahub.github.io/beginneR/)**

A concise tutorial covering the basics of R syntax, data frames, and visualization. Work through this quickly if R is new to you; otherwise skim to confirm your footing.

---

### 1.2 Core Textbooks on Spatial Data Science in R

Pick **one** as your primary reference and treat the other as supplementary. Both are free and online.

**[Spatial Data Science: With Applications in R](https://r-spatial.org/book/)**
*Pebesma & Bivand*

The authoritative modern treatment of spatial data in R. Covers the `sf` and `stars` packages, coordinate reference systems, spatial operations, and geostatistics. Particularly useful for understanding the data model underlying everything else you will do. Chapters 1–7 cover the foundations you need; Chapter 14 (spatial regression) becomes relevant when you start modeling.

**[Geocomputation with R](https://geocompr.robinlovelace.net/index.html)**
*Lovelace, Nowosad & Muenchow*

More applied and tutorial-oriented than the Pebesma book. Good chapters on raster data (Chapter 4) and spatial operations (Chapter 5) that are directly relevant to understanding risk terrain modeling. Chapter 12 on statistical learning is a useful preview of the modeling section.

---

### 1.3 Census Data in R

**[Analyzing US Census Data: Methods, Maps, and Models in R](https://walker-data.com/census-r/index.html)**
*Kyle Walker*

The practical guide to pulling demographic data through the Census API using `tidycensus`. Chapter 7 on spatial analysis includes [local spatial autocorrelation and hotspot maps](https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html?q=hots#local-spatial-autocorrelation) — this is required reading. The [basic usage vignette for tidycensus](https://walker-data.com/tidycensus/articles/basic-usage.html) is a good 30-minute warm-up.

**Key skills to acquire from this book:** pulling ACS variables (population, poverty, housing tenure) at the tract or block-group level for a city of interest; projecting and plotting them; computing Moran's I and LISA statistics.

---

### 1.4 Visualizing Spatial Patterns

**[Bivariate Mapping with `biscale`](https://chris-prener.github.io/biscale/articles/biscale.html)**

A short vignette for producing bivariate choropleth maps — showing two variables simultaneously using a 3×3 color matrix. Useful for visualizing the joint distribution of, e.g., poverty rate and assault rate before modeling.

---

### 1.5 Dynamic Population as a Covariate

**[Nationwide Hourly Population Estimating at the Neighborhood Scale in the United States Using Stable-Attendance Anchor Calibration](https://www.researchgate.net/publication/400812042_Nationwide_Hourly_Population_Estimating_at_the_Neighborhood_Scale_in_the_United_States_Using_Stable-Attendance_Anchor_Calibration)**
*Ning et al., 2026. arXiv: 2602.12291*
**[Data on HuggingFace](https://huggingface.co/datasets/gladcolor/hourly_population_US)**

A methodologically important paper for crime analysis. Standard Census data counts where people *sleep*, not where they *are*. This paper proposes a Stable-Attendance Anchor Calibration (SAAC) framework that uses smartphone mobility data, calibrated against locations with predictable attendance patterns (e.g., high schools), to estimate population presence at the Census block-group level for every hour of the day. The resulting product enables crime-rate denominators that vary by time of day — a meaningful advance over static exposure measures. Read the abstract and methods section carefully; the data are publicly available.

---

### Milestone: Part I 

By the end of Part I, you should be able to:

1. Download 2–3 ACS variables (e.g., poverty rate, percent renters, racial composition) for your chosen city or county at the block-group or tract level.
2. Merge those with a publicly available crime or incident dataset.
3. Produce a clean choropleth map of the outcome variable and at least one covariate.
4. Compute and map local Moran's I (LISA) hotspot clusters for the outcome.
5. Optionally: produce a bivariate map pairing the outcome with one predictor.

A good model project to study: [Predicting Spatial Risk of Opioid Overdoses in Providence, RI](https://pennmusa.github.io/MUSA_801.io/project_5/#1_introduction) (from the Penn MUSA 801 practicum — see also the [full project gallery](https://pennmusa.github.io/MUSA_801.io/)).

---

## Part II: Criminological Modeling

This section introduces how criminologists think about space, place, and crime. The papers move from conceptual foundations to specific modeling strategies.

---

### 2.1 Conceptual Orientation: Crime at Places

**[Ockham's Razor and the Measurement of Crime Concentrations at Places](https://link.springer.com/article/10.1007/s10940-025-09636-4)**
*Andresen, Journal of Quantitative Criminology, 2025*

A good entry point into the "crime at places" literature. The paper asks a deceptively simple question: how should we measure whether crime is concentrated at a small number of locations? It applies the principle of parsimony to evaluate competing indices of crime concentration, with implications for how we define and interpret hotspots. Read alongside the hotspot mapping you did in Part I.

---

### 2.2 Risk Terrain Modeling and Machine Learning

**[Mapping the Risk Terrain for Crime Using Machine Learning](https://link.springer.com/article/10.1007/s10940-020-09457-7)**
*Wheeler & Steenbeek, Journal of Quantitative Criminology, 2021*

The core methodological paper for the research agenda underlying this course. Risk terrain modeling (RTM) divides a city into a regular fishnet grid, layers on environmental and sociodemographic rasters (liquor stores, vacant lots, transit stops, poverty), and models which features predict crime risk. Wheeler and Steenbeek extend this framework with machine learning, comparing regularized regression, random forests, and gradient boosting against traditional RTM. Pay attention to their discussion of feature selection and out-of-sample validation — these are the sticking points where CJ applications often go wrong. The [PAP Little Rock Report](https://papreports.org/little-rock-ar/) is a real-world application of RTM to child maltreatment prevention in Arkansas, applied alongside community resource mapping — good for contextualizing the method.

---

### 2.3 A Recent Spatial Methods Paper (with Field-Context Note)

**[*Criminology*, 2025 — Sagepub, DOI: 10.1177/00111287251391199](https://journals.sagepub.com/doi/full/10.1177/00111287251391199)**

Note: This paper appears in one of the field's flagship journals with a primary focus on a spatial or quantitative method — notable because criminology/CJ journals are slower than statistics or geography journals to feature purely methodological contributions. Read it with this context in mind: what does the paper offer that was absent from the field's toolkit, and how does the framing differ from how the same method would be introduced in a statistics or epidemiology journal?

---

### 2.4 Data Resources for Applied Projects

The following are not papers but are actively useful for building your own analysis.

**[Census Reporter](https://censusreporter.org/)**
A clean interface for browsing and downloading ACS data — faster for exploration than `tidycensus` when you're still deciding which variables matter.

**[Dallas ISD Community Resource Explorer](https://dallasisd.resourceexplorer.org/get-the-data/)**
A well-structured open dataset linking school catchment areas with community resource inventories, sociodemographic variables, and student outcomes. Useful for understanding how resource exposure is operationalized in practice.

**[Child Poverty Action Lab (CPAL) GitHub](https://github.com/childpovertyactionlab)**
Open-source R workflows for neighborhood-level poverty and wellbeing analysis in the Dallas area. Good for studying how a research-oriented CJ organization structures reproducible spatial pipelines.

**[Virginia Crash Data (VDOT)](https://www.virginiaroads.org/datasets/VDOT::crashdata-details-1/explore)**
Incident-level crash records from Virginia, including alcohol involvement flags and geolocation. Interesting spatiotemporal patterns remain underexplored — this dataset could support a student project focused on alcohol-related crash clustering.

---

### 2.5 Multilevel Regression and Poststratification

**[MRP Case Studies](https://bookdown.org/jl5522/MRP-case-studies/)**
*Juan Lopez-Martin et al.*

An online bookdown resource walking through Multilevel Regression and Poststratification (MRP) in several applied contexts. MRP is the Bayesian workhorse for making area-level estimates from individual survey data — increasingly used to estimate neighborhood-level characteristics when direct data are sparse. Skim the introductory chapters; return to the relevant case study if your project requires small-area estimation of a demographic or attitudinal variable.

---

## Part III: Project Ideas

Two broad directions are suggested. Both connect to ongoing research.

---

### Project A: Spatiotemporal Risk Terrain Analysis — Texas

The primary ongoing research program involves spatiotemporal modeling of crime across Texas cities using a risk terrain framework. The core question is: which built environment and sociodemographic factors act as risk amplifiers or protective buffers for crime, and how do these relationships vary across space and time?

**Entry points for a student project:**

- Select one Texas city with open crime data (*this is something I can help with - JD*). Construct a fishnet grid at an appropriate resolution (e.g., 250m or 500m cells). Pull ACS covariates (poverty, housing vacancy, renter share) and OpenStreetMap features (bars, transit stops, parks) as raster layers.
- Fit a spatial regression model (e.g., spatial lag or spatial error, or a Besag-York-Mollié model if pursuing a Bayesian approach) predicting crime counts per cell.
- Compare model performance (predictive accuracy on held-out grid cells) across two or more crime types (e.g., property vs. violent).
- If interested in the temporal dimension: aggregate monthly crime counts and model change over time using a spatiotemporal panel structure.

This connects directly to the RTM literature (Wheeler & Steenbeek above) and the PAP framework.

---

### Project B: Alcohol-Related Crash Clustering in Virginia

Preliminary exploratory work on the VDOT crash dataset reveals spatial clustering in alcohol-involved crashes that has not been formally characterized. This is an open, tractable project with a well-documented public dataset.

**Entry points for a student project:**

- Download the crash records, filter to alcohol-involved incidents, and geocode to census tracts or block groups.
- Compute LISA statistics to identify stable hotspot clusters and cold spots.
- Merge with ACS covariates (income, percent commercial land use if available, urbanicity) and test whether cluster membership predicts alcohol-crash rates in a spatial regression.
- Consider whether the hourly population data from Ning et al. (Section 1.5) could serve as a more appropriate exposure denominator than residential population.

This project is more self-contained and may be more feasible for a single-semester effort.

---

## Suggested Sequencing

| Week | Focus |
|------|-------|
| 1–2 | Part I: R basics + pick one spatial data science book; complete tidycensus tutorial |
| 3–4 | Part I: Census data download, mapping, and hotspot analysis for chosen city |
| 5 | Part I: Bivariate maps; read Ning et al. on dynamic population |
| 6–7 | Part II: Andresen (crime concentrations) + Wheeler & Steenbeek (RTM + ML) |
| 8 | Part II: Sagepub methods paper; explore CPAL and Dallas data resources |
| 9 | Part II: MRP case studies (introductory chapters) |
| 10–end | Part III: Write the paper|

---

*Last updated: April 2026*
