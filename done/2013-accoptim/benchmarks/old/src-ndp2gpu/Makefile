
CC		= nvcc
CUDA_SDK	= $(HOME)/Developer/NVIDIA_GPU_Computing_SDK/C/common/inc/
CUDA_ARCH	= sm_13

CUFILES		= $(wildcard *.cu)
CFLAGS		= -arch=$(CUDA_ARCH) --compiler-options -fPIC --compiler-options -Wall -DUNIX -DCUDA -DNDEBUG

# we should be in ROOT/benchmarks
ROOT		= ..
CFLAGS		+= -I$(CUDA_SDK) -I$(ROOT)/include -I$(ROOT)/vcode


all: $(CUFILES:%.cu=%.so)

%.so : %.cu
	$(CC) -shared --linker-options -soname,$@ -o $@ $(CFLAGS) $<


