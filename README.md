# Bayesian Analysis of Premier League Managerial Impact

This project investigates how team performance changes during the early
stages of a football manager's tenure, in other words, the 'New Manager Bounce'.

The analysis uses Premier League team-match data from the 2014/2015 through
2023/2024 seasons and applies hierarchical Bayesian count models to estimate
changes in attacking and defensive performance.

## Research Question

How does a managerial change affect a team's goals scored, goals conceded,
and expected points during the manager's first ten league matches?

## Planned Modeling Approach

The primary analysis will use hierarchical Bayesian Poisson and negative
binomial models.

The models will account for:

- manager-tenure period;
- home advantage;
- team attacking or defensive strength;
- opponent strength;
- season-level variation;
- uncertainty in all estimated effects.

## Manager-Tenure Periods

The first ten matches are divided into:

- Matches 1–3
- Matches 4–6
- Matches 7–10
- Established manager period: match 11 onward

## Current Progress

### Day 1 — Data preparation and validation

- Restructured the repository
- Added a reproducible R environment with `renv`
- Converted date columns to proper R date objects
- Validated required columns and data types
- Checked missing values and duplicate observations
- Validated manager-tenure dates
- Confirmed manager-window definitions
- Identified incomplete match pairs
- Created model-ready result and tenure features
- Generated validation summary tables

## Data Quality Notes

The dataset uses one team perspective per row.

Most matches therefore appear twice:

- once from the home team's perspective;
- once from the away team's perspective.

Some matches contain only one team-perspective record. These observations
are retained in the primary team-level dataset and flagged so that later
sensitivity analyses can be restricted to complete match pairs.

## Repository Structure

```text
data/raw_data/      Original input data
data/processed_data/ Model-ready datasets
outputs/       Generated figures, tables, and model objects
scripts/        Original exploratory scripts
