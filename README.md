# R Depth-Distribution Histograms — Mariana Trench Profiles

R scripts building the depth-distribution histogram of each of the 25 Mariana Trench cross-section profiles, with overlaid normal-distribution and kernel-density curves and mean/median lines. This is the development/basics version of the 25-histogram figure.

## Related publication

Lemenkova, P. Statistical Analysis of the Mariana Trench Geomorphology Using R
Programming Language. Geodesy and Cartography 2019, 45(2), 57-84.

- DOI: https://doi.org/10.3846/gac.2019.3785
- figshare: https://doi.org/10.6084/m9.figshare.9762860
- HAL: https://hal.science/hal-02277500
- Zenodo: https://zenodo.org/record/3385005
- ISSN: 2029-6991 (Scopus)

These scripts produced Figure 4 (histograms of the 25 profiles).

## Scripts

- Script-01.r ... Script-25 (one per profile): read MDepths.csv, remove NA, and build a ggplot2 histogram (geom_histogram) overlaid with a fitted normal curve (stat_function dnorm), a kernel-density curve (stat_density) and dashed mean/median lines, with a RdGy fill and percent y-axis.

## Methods

- Histogram density estimation with parametric (normal) and non-parametric (kernel) overlays and central-tendency lines.

## Data

- MDepths.csv: depths along 25 Mariana Trench cross-section profiles.

## Requirements

- R (>= 3.5); packages: ggplot2, scales, RColorBrewer

## Author and citation

Polina Lemenkova — ORCID https://orcid.org/0000-0002-5759-1089

Cite: Lemenkova, P. Statistical Analysis of the Mariana Trench Geomorphology Using R Programming Language. Geodesy and Cartography 2019, 45(2), 57-84. https://doi.org/10.3846/gac.2019.3785

## License

MIT — see LICENSE (Copyright Polina Lemenkova).
