FROM islasgeci/base:1.0.0
COPY . /workdir

RUN Rscript -e "install.packages('rjson', repos='http://cran.rstudio.com')"
RUN R -e "remotes::install_github('IslasGECI/testtools', build_vignettes=FALSE, upgrade = 'always')"

RUN R -e "devtools::document()" && \
    R CMD build . && \
    R CMD check seabird.tracking_0.1.0.tar.gz && \
    R CMD INSTALL seabird.tracking_0.1.0.tar.gz
