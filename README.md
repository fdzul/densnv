
<!-- README.md is generated from README.Rmd. Please edit that file -->

# densin

<!-- badges: start -->
<!-- badges: end -->

<img align="center" src="man/figures/logo.png" alt="logo" width="140">

El objetivo central del paquete densnv es realizar el análisis
exploratorio de las bases de datos de las Enfermedades Transmitidas por
Vector (ETV) e Intoxicación por Veneno de Artrópodos (IVA) del Sistema
Nacional de Vigilancia Epidemiológica
([SINAVE](https://www.sinave.gob.mx)) de la Secretaría de Salud de
México a través de graficos, tablas y mapas con el objetivo de incidir
de manera directa en la toma de desiciones racionales y oportunas en los
programas de ETVS

El paquete esta dividido en cuatro grupo de funciones identificadas con
los prefijos rd, gr, mps, & tbl.

- **rd** es una contración de la palabra read y carga las bases de datos
  en R.

- **gr** genera un gráfico.

- **mps** genera un mapa

- **tbl** genera una tabla.

## Instalación

La version en desarrollo del paquete densnv esta alojada en
[GitHub](https://github.com/) y se puede instalar en R con varios
paquetes: [devtools](https://devtools.r-lib.org),
[remotes](https://remotes.r-lib.org),
[renv](https://rstudio.github.io/renv/articles/renv.html) y
[pak](https://pak.r-lib.org)

``` r
# install.packages("devtools")
pak::pkg_install("fdzul/densnv")
```
