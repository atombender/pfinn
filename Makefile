default: all

all: WebUI Scraper

STYLESHEETS=assets/stylesheets/screen.css

SRC_STYLESHEETS=$(wildcard assets/stylesheets/screen/*.sass)

SOURCES=$(wildcard *.hs)

OBJECTS=$(patsubst %.hs,%.o,$(SOURCES))

WebUI: $(SOURCES) $(STYLESHEETS)
	ghc WebUI.hs

Scraper: $(SOURCES)
	ghc Scraper.hs

$(STYLESHEETS): $(SRC_STYLESHEETS)
	sass assets/stylesheets/screen.sass $@

clean:
	rm -f *.o WebUI Scraper

%.o : %.hs
	ghc $<

%.css : %.sass
	sass $< $@
