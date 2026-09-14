VIGNETTE = vignettes/coding-alone
README = README

all: help

doc: ## Update package documentation with `roxygen2`
	Rscript -e "roxygen2::roxygenise()"; \

init: ## Initialize pkgdown site
	echo "pkgdown::init_site()" | R --no-save -q

pkgdown: ## Build entire pkgdown site
	echo "pkgdown::build_site()" | R --no-save -q

pkgdowncheck: ## Check 'pkgdown' site structure
	echo "pkgdown::check_pkgdown()" | R --no-save -q

vignette: $(VIGNETTE).Rmd.orig ## Precompile the review-dividend vignette from live source (requires repo-data-out/)
	cd $(dir $(VIGNETTE)) && Rscript -e "knitr::knit ('$(notdir $(VIGNETTE)).Rmd.orig', output = '$(notdir $(VIGNETTE)).Rmd')"

vignetteOld: $(VIGNETTE).Rmd.orig ## OLD: Precompile the review-dividend vignette from live source (requires repo-data-out/)
	cd vignettes && Rscript -e "devtools::load_all ('..', quiet = TRUE); knitr::knit ('$(VIGNETTE).Rmd.orig', output = '$(VIGNETTE).Rmd')"

knitr: $(README).Rmd ## Render README as markdown
	echo "rmarkdown::render('$(README).Rmd',output_file='$(README).md')" | R --no-save -q

check: ## Run `rcmdcheck`
	Rscript -e 'rcmdcheck::rcmdcheck()'

test: ## Run test suite
	Rscript -e 'testthat::test_local()'

urls: ## Apply 'urlchecker::url_update()' to update all URLs
	Rscript -e 'urlchecker::url_update()'

clean: ## Clean all junk files, including all pkgdown docs
	rm -rf *.html *.png README_cache

help: ## Show this help
	@printf "Usage:\033[36m make [target]\033[0m\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
