all: report.pdf

clean:
	-rm -f Describe2.hp Describe2.svg title.txt key.txt figure.tex

Describe2.hp: Describe2.hp.gz
	gunzip < Describe2.hp.gz > Describe2.hp

Describe2.svg title.txt key.txt: Describe2.hp
	hp2pretty --title=title.txt --key=key.txt --pattern --bands=5 Describe2.hp 

figure.tex: figure.sh title.txt key.txt Describe2.svg
	bash figure.sh title.txt key.txt > figure.tex

report.pdf: report.tex figure.tex $(wildcard *.svg)
	pdflatex -synctex=1 -interaction=nonstopmode --shell-escape report.tex
