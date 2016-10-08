project_name = sharing

ts := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")

tables = 
tables += ./tables/ownership.tex 
./tables/ownership.tex: ../analysis/ownership.R
	cd ../analysis && ./ownership.R
tables += ./tables/ownership_attr.tex 
./tables/ownership_attr.tex: ../analysis/ownership_attr.R
	cd ../analysis && ./ownership_attr.R

tables += ./tables/own_v_rent.tex 
./tables/own_v_rent.tex: ../analysis/own_v_rent.R
	cd ../analysis && ./own_v_rent.R

plots = 
plots += ./plots/reasons.pdf 
./plots/reasons.pdf: ../analysis/reasons.R
	cd ../analysis && ./reasons.R

plots += ./plots/granularity_versus_predictability.pdf 
./plots/granularity_versus_predictability.pdf: ../analysis/granularity_versus_predictability.R
	cd ../analysis && ./granularity_versus_predictability.R

plots += ./plots/scatter_rent_v_own.pdf 
./plots/scatter_rent_v_own.pdf: ../analysis/scatter_rent_v_own.R
	cd ../analysis && ./scatter_rent_v_own.R

plots += ./plots/ownership_fractions.pdf 
./plots/ownership_fractions.pdf: ../analysis/ownership_fractions.R
	cd ../analysis && ./ownership_fractions.R

plots += ./plots/rental_fractions.pdf 
./plots/rental_fractions.pdf: ../analysis/rental_fractions.R
	cd ../analysis && ./rental_fractions.R

plots += ./plots/predictability.pdf 
./plots/predictability.pdf: ../analysis/predictability.R
	cd ../analysis && ./predictability.R

plots += ./plots/granularity.pdf
./plots/granularity.pdf: ../analysis/granularity.R
	cd ../analysis && ./granularity.R

tables = ./tables/ownership.tex ./tables/ownership_attr.tex ./tables/own_v_rent.tex 
plots = ./plots/reasons.pdf ./plots/granularity_versus_predictability.pdf ./plots/scatter_rent_v_own.pdf ./plots/ownership_fractions.pdf ./plots/rental_fractions.pdf ./plots/predictability.pdf ./plots/granularity.pdf

$(project_name).pdf : $(project_name).tex $(project_name).bib $(tables) $(plots)
	pdflatex $(project_name)
	pdflatex $(project_name)
	bibtex $(project_name) 
	pdflatex $(project_name)
	pdflatex $(project_name)
	gs -q -dNOPAUSE -dBATCH -dPDFSETTINGS=/prepress -sDEVICE=pdfwrite -sOutputFile=tmp.pdf $(project_name).pdf
	mv tmp.pdf $(project_name).pdf 

clean: 
	rm *.blg 
	rm *.bbl 
	rm *.aux 
	rm *.out 
	rm *.log 
	rm *.pdf 
	rm *.ttt 
	rm *.fff
	rm *.lot
	rm *.log
	rm *.lof

website: $(project_name).pdf
	printf 'cd papers\nput $<\n' \ | sftp -oPort=2222 johnjhorton@john-joseph-horton.com

backup:
	tar --exclude='../backups' -zcvf ../backups/$(project_name)_$(ts).tar.gz ../

snapshot:
	@echo Timestamp is $(ts)
	cp $(project_name).pdf ../snapshots/$(project_name)_$(ts).pdf


