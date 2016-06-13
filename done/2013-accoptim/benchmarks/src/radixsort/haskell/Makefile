
GHC	 	= ghc
NVCC		= nvcc

SRCDIR   	= .
BUILDDIR 	= dist
HSMAIN   	= Main.hs
TARGET   	= $(BUILDDIR)/bin/radixsort
CUBITS   	=

# PACKAGES	= accelerate-0.12.2.0 accelerate-cuda-0.12.1.2 accelerate-io-0.12.0.0
GHCFLAGS	= --make -Wall -threaded -rtsopts -fllvm -optlo-O3 -O2 \
		  $(PACKAGES:%=-package %) -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR)

LDFLAGS		=
CUFLAGS		= -c -O2

OBJS     	= $(CUBITS:%.cu=$(BUILDDIR)/%.o)
OS_ARCH	 	= $(shell uname -m)

ifeq ($(OS_ARCH),i386)
    CUFLAGS	+= -m32
else
    CUFLAGS	+= -m64
endif


all: dirs $(OBJS)
	$(GHC) $(GHCFLAGS) $(LDFLAGS) $(HSMAIN) $(OBJS) -o $(TARGET)

$(BUILDDIR)/%.o: %.cu
	@mkdir -p $(dir $@)
	$(NVCC) $(CUFLAGS) $< -o $@

dirs:
	mkdir -p $(BUILDDIR)/$(SRCDIR)
	mkdir -p $(dir $(TARGET))

clean:
	$(RM) $(TARGET)
	$(RM) -r $(BUILDDIR)

