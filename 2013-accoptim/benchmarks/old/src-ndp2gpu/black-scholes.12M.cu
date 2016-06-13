#include "config.h"
#include "vcode.h"
#include <cvl.h>
#include "y.tab.h"
#include <cutil_inline.h>
#include "defins.cuh"

MAXALIGN *ComputeMemory = NULL;

extern "C" void init (MAXALIGN *mem) {
  ComputeMemory = mem;
}

__global__ void fused0Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (100.0);
  }
}

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1.0);
  }
}

__global__ void fused2Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (times(pSrc0[address], (divide((z_to_d(pSrc1[address])), (z_to_d(pSrc2[address]))))));
  }
}

__global__ void fused3Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (divide(pSrc0[address], (z_to_d(pSrc1[address]))));
  }
}

__global__ void fused4Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    float *pSrc8 = (float*)(&data[s8]);
    
    pDst[address] = (divide((plus((log((divide(pSrc0[address], pSrc1[address])))), (times((plus(pSrc2[address], (divide((times(pSrc3[address], pSrc4[address])), (2.0))))), pSrc6[address])))), (times(pSrc7[address], (sqrt(pSrc8[address]))))));
  }
}

__global__ void fused5Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int s10, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    float *pSrc8 = (float*)(&data[s8]);
    float *pSrc9 = (float*)(&data[s9]);
    float *pSrc10 = (float*)(&data[s10]);
    
    pDst[address] = (minus((divide((plus((log((divide(pSrc0[address], pSrc1[address])))), (times((plus(pSrc2[address], (divide((times(pSrc3[address], pSrc4[address])), (2.0))))), pSrc6[address])))), (times(pSrc7[address], (sqrt(pSrc8[address])))))), (times(pSrc9[address], (sqrt(pSrc10[address]))))));
  }
}

__global__ void fused6Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    
    pDst[address] = (minus((times(pSrc0[address], pSrc1[address])), (times((times(pSrc2[address], (exp((times(pSrc3[address], pSrc4[address])))))), pSrc5[address]))));
  }
}

__global__ void fused7Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    
    pDst[address] = (minus((times((times(pSrc0[address], (exp((times(pSrc1[address], pSrc2[address])))))), pSrc3[address])), (times(pSrc4[address], pSrc5[address]))));
  }
}

__global__ void fused8Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (minus((0.0), pSrc1[address]));
  }
}

__global__ void fused9Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (times(pSrc0[address], (atan(pSrc1[address]))));
  }
}

__global__ void fused10Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (selection((gt(pSrc0[address], (0.0))), pSrc2[address], pSrc3[address]));
  }
}

__global__ void fused11Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    int *pSrc4 = (int*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    
    pDst[address] = (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc3[address], (0.0))), pSrc5[address], pSrc6[address]))))))));
  }
}

__global__ void fused12Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (3);
  }
}

__global__ void fused13Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (4);
  }
}

__global__ void fused14Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (5);
  }
}

__global__ void fused15Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int s10, int s11, int s12, int s13, int s14, int s15, int s16, int s17, int s18, int s19, int s20, int s21, int s22, int s23, int s24, int s25, int s26, int s27, int s28, int s29, int s30, int s31, int s32, int s33, int s34, int s35, int s36, int s37, int s38, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    int *pSrc6 = (int*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    float *pSrc8 = (float*)(&data[s8]);
    int *pSrc9 = (int*)(&data[s9]);
    int *pSrc10 = (int*)(&data[s10]);
    int *pSrc11 = (int*)(&data[s11]);
    int *pSrc12 = (int*)(&data[s12]);
    int *pSrc13 = (int*)(&data[s13]);
    float *pSrc14 = (float*)(&data[s14]);
    int *pSrc15 = (int*)(&data[s15]);
    float *pSrc16 = (float*)(&data[s16]);
    float *pSrc17 = (float*)(&data[s17]);
    float *pSrc18 = (float*)(&data[s18]);
    int *pSrc19 = (int*)(&data[s19]);
    int *pSrc20 = (int*)(&data[s20]);
    int *pSrc21 = (int*)(&data[s21]);
    float *pSrc22 = (float*)(&data[s22]);
    int *pSrc23 = (int*)(&data[s23]);
    float *pSrc24 = (float*)(&data[s24]);
    float *pSrc25 = (float*)(&data[s25]);
    int *pSrc26 = (int*)(&data[s26]);
    int *pSrc27 = (int*)(&data[s27]);
    int *pSrc28 = (int*)(&data[s28]);
    float *pSrc29 = (float*)(&data[s29]);
    int *pSrc30 = (int*)(&data[s30]);
    float *pSrc31 = (float*)(&data[s31]);
    float *pSrc32 = (float*)(&data[s32]);
    int *pSrc33 = (int*)(&data[s33]);
    float *pSrc34 = (float*)(&data[s34]);
    float *pSrc35 = (float*)(&data[s35]);
    float *pSrc36 = (float*)(&data[s36]);
    int *pSrc37 = (int*)(&data[s37]);
    float *pSrc38 = (float*)(&data[s38]);
    
    pDst[address] = (minus((1.0), (times((times((divide((1.0), (sqrt((times((2.0), pSrc3[address])))))), (exp((divide((times(pSrc4[address], (selection((gt(pSrc5[address], (0.0))), pSrc7[address], pSrc8[address])))), (2.0))))))), (plus((plus((plus((plus((times((0.31938153), (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc14[address], (0.0))), pSrc16[address], pSrc17[address])))))))))), (times((times(pSrc18[address], (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc22[address], (0.0))), pSrc24[address], pSrc25[address])))))))))), (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc29[address], (0.0))), pSrc31[address], pSrc32[address])))))))))))), (times((1.781477937), pSrc34[address])))), (times(pSrc35[address], pSrc36[address])))), (times((1.330274429), pSrc38[address]))))))));
  }
}

__global__ void fused16Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (lt(pSrc0[address], (0.0)));
  }
}

__global__ void fused17Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((lt(pSrc0[address], (0.0)))));
  }
}

__global__ void fused18Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int s10, int s11, int s12, int s13, int s14, int s15, int s16, int s17, int s18, int s19, int s20, int s21, int s22, int s23, int s24, int s25, int s26, int s27, int s28, int s29, int s30, int s31, int s32, int s33, int s34, int s35, int s36, int s37, int s38, int s39, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    int *pSrc7 = (int*)(&data[s7]);
    float *pSrc8 = (float*)(&data[s8]);
    float *pSrc9 = (float*)(&data[s9]);
    int *pSrc10 = (int*)(&data[s10]);
    int *pSrc11 = (int*)(&data[s11]);
    int *pSrc12 = (int*)(&data[s12]);
    int *pSrc13 = (int*)(&data[s13]);
    int *pSrc14 = (int*)(&data[s14]);
    float *pSrc15 = (float*)(&data[s15]);
    int *pSrc16 = (int*)(&data[s16]);
    float *pSrc17 = (float*)(&data[s17]);
    float *pSrc18 = (float*)(&data[s18]);
    float *pSrc19 = (float*)(&data[s19]);
    int *pSrc20 = (int*)(&data[s20]);
    int *pSrc21 = (int*)(&data[s21]);
    int *pSrc22 = (int*)(&data[s22]);
    float *pSrc23 = (float*)(&data[s23]);
    int *pSrc24 = (int*)(&data[s24]);
    float *pSrc25 = (float*)(&data[s25]);
    float *pSrc26 = (float*)(&data[s26]);
    int *pSrc27 = (int*)(&data[s27]);
    int *pSrc28 = (int*)(&data[s28]);
    int *pSrc29 = (int*)(&data[s29]);
    float *pSrc30 = (float*)(&data[s30]);
    int *pSrc31 = (int*)(&data[s31]);
    float *pSrc32 = (float*)(&data[s32]);
    float *pSrc33 = (float*)(&data[s33]);
    int *pSrc34 = (int*)(&data[s34]);
    float *pSrc35 = (float*)(&data[s35]);
    float *pSrc36 = (float*)(&data[s36]);
    float *pSrc37 = (float*)(&data[s37]);
    int *pSrc38 = (int*)(&data[s38]);
    float *pSrc39 = (float*)(&data[s39]);
    
    pDst[address] = (minus((1.0), (minus((1.0), (times((times((divide((1.0), (sqrt((times((2.0), pSrc4[address])))))), (exp((divide((times(pSrc5[address], (selection((gt(pSrc6[address], (0.0))), pSrc8[address], pSrc9[address])))), (2.0))))))), (plus((plus((plus((plus((times((0.31938153), (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc15[address], (0.0))), pSrc17[address], pSrc18[address])))))))))), (times((times(pSrc19[address], (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc23[address], (0.0))), pSrc25[address], pSrc26[address])))))))))), (divide((1.0), (plus((1.0), (times((0.2316419), (selection((gt(pSrc30[address], (0.0))), pSrc32[address], pSrc33[address])))))))))))), (times((1.781477937), pSrc35[address])))), (times(pSrc36[address], pSrc37[address])))), (times((1.330274429), pSrc39[address]))))))))));
  }
}

__global__ void fused19Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (eq((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused20Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (minus((1.0), pSrc1[address]));
  }
}

__global__ void fused21Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (times((exp((z_to_d(pSrc0[address])))), (log(pSrc1[address]))));
  }
}

void fused0(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

void fused1(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused1Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused1 execution failed\n");
}

void fused2(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused2Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused2 execution failed\n");
}

void fused3(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused3Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused3 execution failed\n");
}

void fused4(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused4Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, len, scratch);
  cutilCheckMsg("fused4 execution failed\n");
}

void fused5(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, vec_p s10, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused5Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, len, scratch);
  cutilCheckMsg("fused5 execution failed\n");
}

void fused6(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused6Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused6 execution failed\n");
}

void fused7(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused7Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused7 execution failed\n");
}

void fused8(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused8Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused8 execution failed\n");
}

void fused9(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused9Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused9 execution failed\n");
}

void fused10(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused10Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused10 execution failed\n");
}

void fused11(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused11Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, len, scratch);
  cutilCheckMsg("fused11 execution failed\n");
}

void fused12(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused12Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused12 execution failed\n");
}

void fused13(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused13Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused13 execution failed\n");
}

void fused14(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused14Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused14 execution failed\n");
}

void fused15(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, vec_p s10, vec_p s11, vec_p s12, vec_p s13, vec_p s14, vec_p s15, vec_p s16, vec_p s17, vec_p s18, vec_p s19, vec_p s20, vec_p s21, vec_p s22, vec_p s23, vec_p s24, vec_p s25, vec_p s26, vec_p s27, vec_p s28, vec_p s29, vec_p s30, vec_p s31, vec_p s32, vec_p s33, vec_p s34, vec_p s35, vec_p s36, vec_p s37, vec_p s38, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused15Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31, s32, s33, s34, s35, s36, s37, s38, len, scratch);
  cutilCheckMsg("fused15 execution failed\n");
}

void fused16(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused16Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused16 execution failed\n");
}

void fused17(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused17Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused17 execution failed\n");
}

void fused18(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, vec_p s10, vec_p s11, vec_p s12, vec_p s13, vec_p s14, vec_p s15, vec_p s16, vec_p s17, vec_p s18, vec_p s19, vec_p s20, vec_p s21, vec_p s22, vec_p s23, vec_p s24, vec_p s25, vec_p s26, vec_p s27, vec_p s28, vec_p s29, vec_p s30, vec_p s31, vec_p s32, vec_p s33, vec_p s34, vec_p s35, vec_p s36, vec_p s37, vec_p s38, vec_p s39, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused18Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24, s25, s26, s27, s28, s29, s30, s31, s32, s33, s34, s35, s36, s37, s38, s39, len, scratch);
  cutilCheckMsg("fused18 execution failed\n");
}

void fused19(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused19Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused19 execution failed\n");
}

void fused20(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused20Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused20 execution failed\n");
}

void fused21(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused21Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused21 execution failed\n");
}

make_no_scratch(fused0)
make_no_scratch(fused1)
make_no_scratch(fused2)
make_no_scratch(fused3)
make_no_scratch(fused4)
make_no_scratch(fused5)
make_no_scratch(fused6)
make_no_scratch(fused7)
make_no_scratch(fused8)
make_no_scratch(fused9)
make_no_scratch(fused10)
make_no_scratch(fused11)
make_no_scratch(fused12)
make_no_scratch(fused13)
make_no_scratch(fused14)
make_no_scratch(fused15)
make_no_scratch(fused16)
make_no_scratch(fused17)
make_no_scratch(fused18)
make_no_scratch(fused19)
make_no_scratch(fused20)
make_no_scratch(fused21)
make_inplace(fused0, INPLACE_NONE)
make_inplace(fused1, INPLACE_NONE)
make_inplace(fused2, INPLACE_1)
make_inplace(fused3, INPLACE_1)
make_inplace(fused4, INPLACE_1)
make_inplace(fused5, INPLACE_1)
make_inplace(fused6, INPLACE_1)
make_inplace(fused7, INPLACE_1)
make_inplace(fused8, INPLACE_2)
make_inplace(fused9, INPLACE_1)
make_inplace(fused10, INPLACE_1)
make_inplace(fused11, INPLACE_4)
make_inplace(fused12, INPLACE_NONE)
make_inplace(fused13, INPLACE_NONE)
make_inplace(fused14, INPLACE_NONE)
make_inplace(fused15, INPLACE_4)
make_inplace(fused16, INPLACE_1)
make_inplace(fused17, INPLACE_1)
make_inplace(fused18, INPLACE_5)
make_inplace(fused19, INPLACE_1)
make_inplace(fused20, INPLACE_2)
make_inplace(fused21, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused1", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused2", 3, 1,
  {Float,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused3", 2, 1,
  {Float,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused4", 9, 1,
  {Float,Float,Float,Float,Float,Segdes,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise9},
  {FUSED, "fused5", 11, 1,
  {Float,Float,Float,Float,Float,Segdes,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise11},
  {FUSED, "fused6", 6, 1,
  {Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused7", 6, 1,
  {Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused8", 2, 1,
  {Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise2},
  {FUSED, "fused9", 2, 1,
  {Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused10", 4, 1,
  {Float,Segdes,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused11", 7, 1,
  {Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise7},
  {FUSED, "fused12", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused13", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused14", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused15", 39, 1,
  {Segdes,Segdes,Segdes,Float,Float,Float,Segdes,Float,Float,Segdes,Segdes,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Float,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Segdes,Float,Float,Float,Segdes,Float,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise39},
  {FUSED, "fused16", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused17", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused18", 40, 1,
  {Segdes,Segdes,Segdes,Segdes,Float,Float,Float,Segdes,Float,Float,Segdes,Segdes,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Float,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Segdes,Segdes,Segdes,Float,Segdes,Float,Float,Segdes,Float,Float,Float,Segdes,Float},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise40},
  {FUSED, "fused19", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused20", 2, 1,
  {Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise2},
  {FUSED, "fused21", 2, 1,
  {Int,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  };

cvl_triple_t cvl_funs[] = {
  { { (void (*)())fused0, (int (*)())fused0_scratch, (unsigned (*)())fused0_inplace },},
  { { (void (*)())fused1, (int (*)())fused1_scratch, (unsigned (*)())fused1_inplace },},
  { { (void (*)())fused2, (int (*)())fused2_scratch, (unsigned (*)())fused2_inplace },},
  { { (void (*)())fused3, (int (*)())fused3_scratch, (unsigned (*)())fused3_inplace },},
  { { (void (*)())fused4, (int (*)())fused4_scratch, (unsigned (*)())fused4_inplace },},
  { { (void (*)())fused5, (int (*)())fused5_scratch, (unsigned (*)())fused5_inplace },},
  { { (void (*)())fused6, (int (*)())fused6_scratch, (unsigned (*)())fused6_inplace },},
  { { (void (*)())fused7, (int (*)())fused7_scratch, (unsigned (*)())fused7_inplace },},
  { { (void (*)())fused8, (int (*)())fused8_scratch, (unsigned (*)())fused8_inplace },},
  { { (void (*)())fused9, (int (*)())fused9_scratch, (unsigned (*)())fused9_inplace },},
  { { (void (*)())fused10, (int (*)())fused10_scratch, (unsigned (*)())fused10_inplace },},
  { { (void (*)())fused11, (int (*)())fused11_scratch, (unsigned (*)())fused11_inplace },},
  { { (void (*)())fused12, (int (*)())fused12_scratch, (unsigned (*)())fused12_inplace },},
  { { (void (*)())fused13, (int (*)())fused13_scratch, (unsigned (*)())fused13_inplace },},
  { { (void (*)())fused14, (int (*)())fused14_scratch, (unsigned (*)())fused14_inplace },},
  { { (void (*)())fused15, (int (*)())fused15_scratch, (unsigned (*)())fused15_inplace },},
  { { (void (*)())fused16, (int (*)())fused16_scratch, (unsigned (*)())fused16_inplace },},
  { { (void (*)())fused17, (int (*)())fused17_scratch, (unsigned (*)())fused17_inplace },},
  { { (void (*)())fused18, (int (*)())fused18_scratch, (unsigned (*)())fused18_inplace },},
  { { (void (*)())fused19, (int (*)())fused19_scratch, (unsigned (*)())fused19_inplace },},
  { { (void (*)())fused20, (int (*)())fused20_scratch, (unsigned (*)())fused20_inplace },},
  { { (void (*)())fused21, (int (*)())fused21_scratch, (unsigned (*)())fused21_inplace },},
  };
/*
fused OP0#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 100.0) $0)
fused OP1#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 1.0) $0)
fused OP2#1 ($0 : FLOAT, $1 : INT, $2 : INT) = (* FLOAT @ $0 (/ FLOAT @ (I_TO_F @ $1) (I_TO_F @ $2)))
fused OP3#1 ($0 : FLOAT, $1 : INT) = (/ FLOAT @ $0 (I_TO_F @ $1))
fused OP4#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : SEGDES, $6 : FLOAT, $7 : FLOAT, $8 : FLOAT) = (/ FLOAT
  @
  (+ FLOAT @ (LOG @ (/ FLOAT @ $0 $1))
    (* FLOAT @ (+ FLOAT @ $2 (/ FLOAT @ (* FLOAT @ $3 $4) (DIST FLOAT @ (CONST FLOAT 2.0) $5))) $6))
  (* FLOAT @ $7 (SQRT @ $8)))
fused OP5#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : SEGDES, $6 : FLOAT, $7 : FLOAT, $8 : FLOAT, $9 : FLOAT, $10 : FLOAT) = (- FLOAT
  @
  (/ FLOAT @
    (+ FLOAT @ (LOG @ (/ FLOAT @ $0 $1))
      (* FLOAT @ (+ FLOAT @ $2 (/ FLOAT @ (* FLOAT @ $3 $4) (DIST FLOAT @ (CONST FLOAT 2.0) $5))) $6))
    (* FLOAT @ $7 (SQRT @ $8))) (* FLOAT @ $9 (SQRT @ $10)))
fused OP6#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT) = (- FLOAT @ (* FLOAT @ $0 $1)
  (* FLOAT @ (* FLOAT @ $2 (EXP @ (* FLOAT @ $3 $4))) $5))
fused OP7#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT) = (- FLOAT @
  (* FLOAT @ (* FLOAT @ $0 (EXP @ (* FLOAT @ $1 $2))) $3) (* FLOAT @ $4 $5))
fused OP8#1 ($0 : SEGDES, $1 : FLOAT) = (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.0) $0) $1)
fused OP9#1 ($0 : FLOAT, $1 : FLOAT) = (* FLOAT @ $0 (ATAN @ $1))
fused OP10#1 ($0 : FLOAT, $1 : SEGDES, $2 : FLOAT, $3 : FLOAT) = (SELECT FLOAT @
  (> FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)) $2 $3)
fused OP11#1 ($0 : SEGDES, $1 : SEGDES, $2 : SEGDES, $3 : FLOAT, $4 : SEGDES, $5 : FLOAT, $6 : FLOAT) = (/ FLOAT @
  (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $1)
    (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $2)
      (SELECT FLOAT @ (> FLOAT @ $3 (DIST FLOAT @ (CONST FLOAT 0.0) $4)) $5 $6))))
fused OP12#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 3) $0)
fused OP13#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 4) $0)
fused OP14#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 5) $0)
fused OP15#1 ($0 : SEGDES, $1 : SEGDES, $2 : SEGDES, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT, $6 : SEGDES, $7 : FLOAT, $8 : FLOAT, $9 : SEGDES, $10 : SEGDES, $11 : SEGDES, $12 : SEGDES, $13 : SEGDES, $14 : FLOAT, $15 : SEGDES, $16 : FLOAT, $17 : FLOAT, $18 : FLOAT, $19 : SEGDES, $20 : SEGDES, $21 : SEGDES, $22 : FLOAT, $23 : SEGDES, $24 : FLOAT, $25 : FLOAT, $26 : SEGDES, $27 : SEGDES, $28 : SEGDES, $29 : FLOAT, $30 : SEGDES, $31 : FLOAT, $32 : FLOAT, $33 : SEGDES, $34 : FLOAT, $35 : FLOAT, $36 : FLOAT, $37 : SEGDES, $38 : FLOAT) = (- FLOAT
  @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (* FLOAT @
    (* FLOAT @
      (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $1) (SQRT @ (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 2.0) $2) $3)))
      (EXP @
        (/ FLOAT @ (* FLOAT @ $4 (SELECT FLOAT @ (> FLOAT @ $5 (DIST FLOAT @ (CONST FLOAT 0.0) $6)) $7 $8))
          (DIST FLOAT @ (CONST FLOAT 2.0) $9))))
    (+ FLOAT @
      (+ FLOAT @
        (+ FLOAT @
          (+ FLOAT @
            (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.31938153) $10)
              (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $11)
                (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $12)
                  (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $13)
                    (SELECT FLOAT @ (> FLOAT @ $14 (DIST FLOAT @ (CONST FLOAT 0.0) $15)) $16 $17)))))
            (* FLOAT @
              (* FLOAT @ $18
                (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $19)
                  (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $20)
                    (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $21)
                      (SELECT FLOAT @ (> FLOAT @ $22 (DIST FLOAT @ (CONST FLOAT 0.0) $23)) $24 $25)))))
              (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $26)
                (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $27)
                  (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $28)
                    (SELECT FLOAT @ (> FLOAT @ $29 (DIST FLOAT @ (CONST FLOAT 0.0) $30)) $31 $32))))))
          (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.781477937) $33) $34)) (* FLOAT @ $35 $36))
      (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.330274429) $37) $38))))
fused OP16#1 ($0 : FLOAT, $1 : SEGDES) = (< FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1))
fused OP17#1 ($0 : FLOAT, $1 : SEGDES) = (B_TO_I @ (< FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)))
fused OP18#1 ($0 : SEGDES, $1 : SEGDES, $2 : SEGDES, $3 : SEGDES, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT, $7 : SEGDES, $8 : FLOAT, $9 : FLOAT, $10 : SEGDES, $11 : SEGDES, $12 : SEGDES, $13 : SEGDES, $14 : SEGDES, $15 : FLOAT, $16 : SEGDES, $17 : FLOAT, $18 : FLOAT, $19 : FLOAT, $20 : SEGDES, $21 : SEGDES, $22 : SEGDES, $23 : FLOAT, $24 : SEGDES, $25 : FLOAT, $26 : FLOAT, $27 : SEGDES, $28 : SEGDES, $29 : SEGDES, $30 : FLOAT, $31 : SEGDES, $32 : FLOAT, $33 : FLOAT, $34 : SEGDES, $35 : FLOAT, $36 : FLOAT, $37 : FLOAT, $38 : SEGDES, $39 : FLOAT) = (- FLOAT
  @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $1)
    (* FLOAT @
      (* FLOAT @
        (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $2) (SQRT @ (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 2.0) $3) $4)))
        (EXP @
          (/ FLOAT @ (* FLOAT @ $5 (SELECT FLOAT @ (> FLOAT @ $6 (DIST FLOAT @ (CONST FLOAT 0.0) $7)) $8 $9))
            (DIST FLOAT @ (CONST FLOAT 2.0) $10))))
      (+ FLOAT @
        (+ FLOAT @
          (+ FLOAT @
            (+ FLOAT @
              (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.31938153) $11)
                (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $12)
                  (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $13)
                    (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $14)
                      (SELECT FLOAT @ (> FLOAT @ $15 (DIST FLOAT @ (CONST FLOAT 0.0) $16)) $17 $18)))))
              (* FLOAT @
                (* FLOAT @ $19
                  (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $20)
                    (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $21)
                      (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $22)
                        (SELECT FLOAT @ (> FLOAT @ $23 (DIST FLOAT @ (CONST FLOAT 0.0) $24)) $25 $26)))))
                (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $27)
                  (+ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $28)
                    (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $29)
                      (SELECT FLOAT @ (> FLOAT @ $30 (DIST FLOAT @ (CONST FLOAT 0.0) $31)) $32 $33))))))
            (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.781477937) $34) $35)) (* FLOAT @ $36 $37))
        (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.330274429) $38) $39)))))
fused OP19#1 ($0 : INT, $1 : INT, $2 : INT) = (= INT @ (- INT @ $0 $1) $2)
fused OP20#1 ($0 : SEGDES, $1 : FLOAT) = (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $0) $1)
fused OP21#1 ($0 : INT, $1 : FLOAT) = (* FLOAT @ (EXP @ (I_TO_F @ $0)) (LOG @ $1))
*/
