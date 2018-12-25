doc:
	R -e "devtools::document()"
build:
	R -e "devtools::build()"
	R -e "pkgdown::build_site()"
install:
	R -e "devtools::install_github('software-analytics/Rnalytica')"
install_local:
	R CMD INSTALL .
check:
	R -e "devtools::check()"