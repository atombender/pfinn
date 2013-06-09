default: all

all: WebUI Scraper

STYLESHEETS=assets/stylesheets/screen.css

SRC_STYLESHEETS=$(wildcard assets/stylesheets/screen/*.sass)

OUTPUT_STYLESHEETS=$(patsubst %.sass,%.css,$(SRC_STYLESHEETS)) $(STYLESHEETS)

SOURCES=$(wildcard *.hs)

OBJECTS=$(patsubst %.hs,%.o,$(SOURCES))

GHC_FLAGS=-Werror
#GHC_FLAGS=-Werror -Wall

WebUI: $(SOURCES) $(STYLESHEETS)
	ghc $(GHC_FLAGS) WebUI.hs

Scraper: $(SOURCES)
	ghc $(GHC_FLAGS) Scraper.hs

$(STYLESHEETS): $(SRC_STYLESHEETS)
	sass assets/stylesheets/screen.sass $@

clean:
	rm -f $(OBJECTS) *.ld_* WebUI Scraper $(OUTPUT_STYLESHEETS)

%.o : %.hs
	ghc $<

%.css : %.sass
	sass $< $@

install-dependencies:
	cabal install $(CABAL_OPTS) --reinstall hxt
	cabal install $(CABAL_OPTS) --reinstall encoding
	cabal install $(CABAL_OPTS) --reinstall HTTP
	cabal install $(CABAL_OPTS) --reinstall network
	cabal install $(CABAL_OPTS) --reinstall timerep
	cabal install $(CABAL_OPTS) --reinstall HDBostgresql
	cabal install $(CABAL_OPTS) --reinstall utf8-string
	cabal install $(CABAL_OPTS) --reinstall convertible
	cabal install $(CABAL_OPTS) --reinstall happstack-lite
	cabal install $(CABAL_OPTS) --reinstall regex-posix
	cabal install $(CABAL_OPTS) --reinstall text-icu
	cabal install $(CABAL_OPTS) --reinstall time
	cabal install $(CABAL_OPTS) --reinstall transformers 
