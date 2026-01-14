.PHONY: all build format edit demo clean

src?=0
dst?=5
graph?=graph1.txt
sports_set?=set1.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	dune build src/fsport.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) outfile
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile

demosport: build
	@echo "\n   📜  INPUT DATA (content of infile)  📜\n"
	@cat sports/${sports_set}
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./fsport.exe sports/${sports_set} outfile-sport
	@echo "\n   🥁  RESULT (content of outfile)  🥁\n"
	@cat outfile-sport

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean
