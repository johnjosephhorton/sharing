project_name = sharing

ts := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")

# Get the data 
data_url_file := ../data_url.txt
data_url := $(shell cat ${data_url_file})

encrypted_data = ../code/shaRing/inst/extdata/sharing_survey_results_pilot.csv.gpg
data = ../code/shaRing/inst/extdata/sharing_survey_results_pilot.csv

$(encrypted_data): ../data_url.txt
	cd ../code/shaRing/inst/extdata && curl -o sharing_survey_results_pilot.csv.gpg $(data_url)  	

$(data): $(encrypted_data)
	cd ../code/shaRing/inst/extdata && gpg --passphrase-file ../../../../data_passphrase.txt --batch sharing_survey_results_pilot.csv.gpg

tables = ./tables/ownership.tex ./tables/ownership_attr.tex ./tables/own_v_rent.tex 
plots = ./plots/reasons.pdf ./plots/granularity_versus_predictability.pdf ./plots/scatter_rent_v_own.pdf ./plots/ownership_fractions.pdf ./plots/rental_fractions.pdf ./plots/predictability.pdf ./plots/granularity.pdf

$(tables) $(plots): $(data)
	cd ../code/R && ./sharing.R

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


