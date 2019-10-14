render:
	Rscript --quiet -e "rmarkdown::render('index.Rmd')"

moon:
	Rscript --quiet -e "xaringan::inf_mr('index.Rmd')"

serve:
	Rscript --quiet -e "servr::httw(dir = '.', pattern = 'html|css|js', port = 9090, browser = FALSE, daemon = FALSE)"
	
clean:
	rm -rf index.html index_files
