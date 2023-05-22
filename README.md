
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Age reporting for the oldest old in the Brazilian COVID–19 vaccination database: what can we learn from it?

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![License: CC BY
4.0](https://img.shields.io/badge/License-CC_BY_4.0-blue.svg)](https://creativecommons.org/licenses/by/4.0/)

<!-- badges: end -->

This repository provides the reproducible `R` code and the data for the
paper *Age reporting for the oldest old in the Brazilian COVID–19
vaccination database: what can we learn from it?*

![](output/figure-3.png)*Proportion of individuals aged 90 and older
(90+/80+) by the proportion of centenarians (100+/80+) and life
expectancy at age 50 (e(50)*). Men and women. Brazil and its regions
(vaccination records) and Sweden.

------------------------------------------------------------------------

## Citation

Turra, Cássio M., Fernando Fernandes, Júlia Almeida Calazans, and
Marília R. Nepomuceno. 2023. “Age Reporting in the Brazilian COVID–19
Vaccination Database: What Can We Learn from It?” *Demographic Research*
48(28): 829–848. <https://doi.org/10.4054/DemRes.2023.48.28>.

------------------------------------------------------------------------

## Abstract

**Background**: Age misreporting affects population estimates at older
ages. In Brazil, every citizen must be registered and show an identity
document to vaccinate against COVID–19. This requirement to present
proof of age provides a unique opportunity for measuring the oldest-old
population using novel administrative data.

**Objectives**: To offer critically assessed estimates of the Brazilian
population aged 80 and older based on data from the vaccination
registration system (VRS). To uncover discrepancies between the number
of vaccinated oldest-old people and the projections used to estimate
target populations for COVID–19 vaccination.

**Methods**: We calculate data quality indicators based on data from the
VRS – namely, 100+/80+ and 90+/80+ population proportions, sex ratios,
and the Myers blended index – and compare them to those based on data on
target populations from Brazilian censuses and demographic projections,
and from Sweden – a country with high-quality data. We also estimate
vaccination coverage ratios using population projections adjusted to
excess deaths as the denominators.

**Results**: Requiring documentation reduces age heaping, age
exaggeration, and sex ratios marginally. However, it cannot solve the
problem of the misreporting of birth dates due to the absence of
long–standing birth registration systems in Brazil, particularly in the
northern and central regions. In addition, we find a mismatch between
the projected populations and numbers of vaccinated people across
regions.

**Conclusions**: Despite improvements in data quality in Brazil, we are
still not confident about the accuracy of age reporting among the oldest
old in the less advantaged Brazilian regions. The postponement of the
2020 census reduced the ability of authorities to define the target
populations for vaccinations against COVID–19 and other diseases.

**Contributions**: This is the first study to compare population
estimates for the oldest old in administrative data and census data in
Brazil. Age misreporting resulted in discrepancies that may have
compromised the efficacy of the COVID–19 vaccination campaign.

------------------------------------------------------------------------

## Reproducibility

We use population data for Brazil from the Brazilian Ministry of
Health’s (MoH) open microdata registers for the national COVID-19
vaccination campaign (Brasil 2021); population projections from the
Brazilian Institute of Geography and Statistics (IBGE 2018); population
projections from the United Nations (United Nations 2019); and Brazilian
censuses from 1980 to 2010 (IPUMS) (Center 2020). We use population data
for Sweden from the Human Mortality Database (HMD) (*Human Mortality
Database* 2021). We also use estimates for the distributions of excess
deaths in Brazil, including the proportions by age groups, sex, and
region of residence, and the absolute numbers by epidemiological week
and region of residence from the Conselho Nacional de Secretários de
Saúde (CONASS (Conselho Nacional de Secretários de Saúde) 2021).

The IBGE, UN, IPUMS, HMD and CONASS data files are in the `data-raw`
sub-directory.

The data file with the Brazilian MoH vaccination records has
`387,750,333` observations and around `190 GB`. A compressed `40 GB`
version is available at
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7360803.svg)](https://doi.org/10.5281/zenodo.7360803)
and should be downloaded and copied into the `data-raw` sub-directory.

The `R` code script files are in the `r-scripts` sub-directory.

The script to read, clean, prepare, tabulate and analyze data is
`1-covid-19-datasus-vaccine.R` and will write to the `data-treated`
sub-directory.

The script to reproduce the figures is
`2-covid-19-datasus-vaccine-plots.R` and will write to the `output`
sub-directory.

### Data References

<div id="refs" class="references csl-bib-body hanging-indent"
style="font-size: 90%; padding-left: 5%">

<div id="ref-Brasil2021a" class="csl-entry">

Brasil. 2021. “Campanha Nacional de Vacinação contra Covid-19 -
Registros de Vacinação COVID-19 - Open Data.” *openDataSUS*.
<https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/>.

</div>

<div id="ref-MinnesotaPopulationCenter2020" class="csl-entry">

Center, Minnesota Population. 2020. “Integrated Public Use Microdata
Series, International: Version 7.3.” <https://international.ipums.org>;
Minneapolis, MN: IPUMS. <https://doi.org/10.18128/D020.V7.3>.

</div>

<div id="ref-CONASS2021" class="csl-entry">

CONASS (Conselho Nacional de Secretários de Saúde). 2021. “Painel de
Análise Do Excesso de Mortalidade Por Causas Naturais No Brasil.”
<https://www.conass.org.br/indicadores-de-obitos-por-causas-naturais/>.

</div>

<div id="ref-HMD2021" class="csl-entry">

*Human Mortality Database*. 2021. University of California, Berkeley
(USA) and Max Planck Institute for Demographic Research (Germany).

</div>

<div id="ref-IBGE2018" class="csl-entry">

IBGE. 2018. *Projeções Da População: Brasil e Unidades Da Federação -
Revisão 2018*. Second. Séries Relatórios Metodológicos, volume 40. Rio
de Janeiro: IBGE, Coordenação de População e Indicadores Sociais.

</div>

<div id="ref-UnitedNations2019c" class="csl-entry">

United Nations. 2019. *World Population Prospects 2019: Online Edition*.
New York: United Nations, Department of Economic and Social Affairs,
Population Division.

</div>

</div>

------------------------------------------------------------------------

## Questions

For any problems or questions, please open an
[issue](https://github.com/demographyandme/covid-19-datasus-vacina/issues)
or start a
[discussion](https://github.com/demographyandme/covid-19-datasus-vacina/discussions/).

------------------------------------------------------------------------

## Authors

**Cássio M Turra**, Demography Department, Cedeplar, Universidade
Federal de Minas Gerais, Brazil
<img src="icons/twitter-brands.svg" width="15"/>
[@CassioMTurra](https://twitter.com/CassioMTurra)

**Fernando Fernandes**, Demography Department, Cedeplar, Universidade
Federal de Minas Gerais, Brazil
<img src="icons/twitter-brands.svg" width="15"/>
[@demographyandme](https://twitter.com/demographyandme)

**Júlia A. Calazans**, Demography Department, Cedeplar, Universidade
Federal de Minas Gerais, Brazil
<img src="icons/twitter-brands.svg" width="15"/>
[@\_JuliaCalazans\_](https://twitter.com/_JuliaCalazans_)

**Marília R. Nepomuceno**, Max–Planck–Institut für Demografische
Forschung, Rostock, Germany
<img src="icons/twitter-brands.svg" width="15"/>
[@MariliaNepo](https://twitter.com/MariliaNepo)

------------------------------------------------------------------------

## License

The data used and presented is licensed under [CC BY
4.0](http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1). The
code to read, format, analyze and display that data is licensed under
[The MIT license](http://opensource.org/licenses/mit-license.php).
