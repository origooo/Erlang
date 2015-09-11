#Makefile
EBINS = ./ebin 
COUCHVIEWS = ebin/couch_views
ERLANG_LOG = ebin/log
SYMBOLS = ebin/symbols
DOC = ./doc
INCLUDE = ./include
SRC = ./src
APP = stocksadapa

all: dirs compile edocs
install: dirs move compile edocs
uninstall: restore compile clean

dirs:
	@echo Creating directories
	@mkdir -p $(EBINS)
	@mkdir -p $(SRC)
	@mkdir -p $(COUCHVIEWS)
	@mkdir -p $(ERLANG_LOG)
	@mkdir -p $(INCLUDE)
	@mkdir -p $(DOC)
edocs:
	@echo Generating edocs from src. 
	@erl -noinput -eval 'edoc:application($(APP), ".", [{source_path, ["./src"]}])' -s erlang halt
compile:
	@echo Compiling src files.
	@erl -noinput -eval 'make:all([load])' -s erlang halt
move:
	@echo Moving to release mode
	-@mv ./*.erl  ./$(SRC)
	-@mv ./*.hrl ./$(INCLUDE)
	-@mv ./couch_views ./$(EBINS)
	-@mv ./log ./$(EBINS)
	-@mv ./*.edoc ./$(DOC)
	-@mv ./symbols ./$(SYMBOLS)
	-@mv ./*.beam ./$(EBINS)
	-@mv ./*.app ./$(EBINS)
restore:
	@echo Restoring to development mode
	-@mv ./src/*.erl .
	-@mv ./ebin/couch_views .
	-@mv ./ebin/log .
	-@mv ./ebin/symbols .
	-@mv ./ebin/*.app .
	-@mv ./doc/*edoc .
	-@mv ./include/*.hrl .
clean:
	@rm -rf $(EBINS)
	@rm -rf doc
	@rm -rf erl_crash.dump

