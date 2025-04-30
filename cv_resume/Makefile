MAKEFLAGS += --silent

LATEX_INTERPRETER = pdflatex -synctex=1 -interaction=nonstopmode 
TEX_FILE = src/cv-resume-ewr.tex

.PHONY: cv
cv:
	echo "Compiling PDF output..."
	bash src/bold.sh
	$(LATEX_INTERPRETER) $(TEX_FILE)

.PHONY: clean
clean:
	find . -type f -name "*.pdf" -delete
	find . -type f -name "*.a*" -delete
	find . -type f -name "cv*.b*" -delete
	find . -type f -name "*.log" -delete
	find . -type f -name "*.out" -delete
	find . -type f -name "*.run*" -delete
	find . -type f -name "*.gz" -delete
