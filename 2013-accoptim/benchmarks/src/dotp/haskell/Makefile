
GHC	 	= ghc
NVCC		= nvcc

SRCDIR   	= .
BUILDDIR 	= dist
HSMAIN   	= Main.hs
TARGET   	= $(BUILDDIR)/bin/dotp
CUBITS   	=

GHCFLAGS	= --make -Wall -threaded -rtsopts -fllvm -optlo-O3 -Odph -fno-liberate-case \
		  -funfolding-use-threshold100 -funfolding-keeness-factor100 \
		  -odir $(BUILDDIR) -hidir $(BUILDDIR) -i$(SRCDIR)

LDFLAGS		= -L/usr/local/cuda/lib -lcudart
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

