# These are the data files that the R package depends upon.
# If they are missing, then we need to run clean_data.R in the WageCutsR/data-raw folder.
# data = ../code/WageCutsR/data/df.rda ../code/WageCutsR/data/df_no_1.rda ../code/WageCutsR/data/df_no_3.rda

# # These are the tables used in the writeup 
# tables =./tables/uptake_simple.tex ./tables/uptake_framing.tex ./tables/uptake_unframed.tex ./tables/error.tex ./tables/cor.tex ./tables/games.tex

# # These are the figures used in the writup 
# figures = ./plots/uptake.pdf

project_name = sharing

tables = ./tables/ownership.tex ./tables/ownership_attr.tex ./tables/own_v_rent.tex 

plots = ./plots/reasons.pdf ./plots/granularity_versus_predictability.pdf ./plots/scatter_rent_v_own.pdf ./plots/ownership_fractions.pdf ./plots/rental_fractions.pdf ./plots/predictability.pdf ./plots/granularity.pdf


# $(data):
# 	cd ../code/WageCutsR/data-raw; ./clean_data.R

# $(tables): $(data)
# 	cd ../code/R; ./$(project_name).R

# $(figures): $(data)
# 	cd ../code/R; ./$(project_name).R

$(tables) $(plots):
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
	aws s3 cp employer_search_$(ts).tar.gz s3://john_horton_backups/

snapshot:
	@echo Timestamp is $(ts)
	cp $(project_name).pdf ../snapshots/$(project_name)_$(ts).pdf


