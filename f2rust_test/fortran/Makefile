# Use gfortran to generate the expected output file for each test case

SOURCES = $(wildcard *.f90)
EXPECTED = $(SOURCES:.f90=.exp)
OBJECTS = $(patsubst %.f90,build/%,$(notdir $(SOURCES)))

all: $(EXPECTED)

clean:
	rm -f $(OBJECTS) $(EXPECTED)

.SECONDARY: $(OBJECTS)

%.exp: build/%
	$< > $@

build/%: %.f90 Makefile
	@mkdir -p build
	@gfortran -std=gnu -Wall -Wno-integer-division -Wno-zerotrip $< -o $@
