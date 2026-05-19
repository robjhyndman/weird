build:
  Rscript --no-environ -e "devtools::build(args = c('--compact-vignettes=both'))"

check:
  Rscript --no-environ -e "try(devtools::check('.'), silent=FALSE)"

clean:
  rm -rf man/*.Rd
  rm -rf NAMESPACE
  rm -f .Rhistory
  rm -f *.RData
  rm -f *.Rproj
  rm -rf .Rproj.user

coverage:
  Rscript --no-environ -e "devtools::test_coverage('.')"

docs:
  Rscript --no-environ -e "devtools::document()"

install:
  R CMD INSTALL .

pkgdown:
  Rscript --no-environ -e "pkgdown::build_site('.')"

release:
  Rscript --no-environ -e "devtools::submit_cran(args = c('--compact-vignettes=both'))"

revdep:
  Rscript --no-environ -e "revdepcheck::revdep_check(num_workers=3)"

test:
  Rscript --no-environ -e "devtools::test('.')"
