
#include <stdio.h>
#include <cuda_runtime_api.h>

#include "sdk-kernel.cu"

#define cutilSafeCallNoSync(err)     __cudaSafeCallNoSync(err, __FILE__, __LINE__)

inline void __cudaSafeCallNoSync( cudaError err, const char *file, const int line )
{
    if( cudaSuccess != err) {
        fprintf(stderr, "%s(%i) : cudaSafeCallNoSync() Runtime API error : %s.\n",
                file, line, cudaGetErrorString( err) );
        exit(-1);
    }
}


extern "C"
void blackscholes
(
    float               *h_CallResult,
    float               *h_PutResult,
    float               *d_StockPrice,
    float               *d_OptionStrike,
    float               *d_OptionYears,
    const float         riskfree,
    const float         volatility,
    const int           opt_n
)
{
    const float opt_sz = opt_n * sizeof(float);
    float * d_CallResult;
    float * d_PutResult;

    /*
     * Allocate device memory
     */
    cutilSafeCallNoSync( cudaMalloc((void **)&d_CallResult,   opt_sz) );
    cutilSafeCallNoSync( cudaMalloc((void **)&d_PutResult,    opt_sz) );

    /*
     * Execute
     */
    BlackScholesGPU<<<480, 128>>>(
        d_CallResult,
        d_PutResult,
        d_StockPrice,
        d_OptionStrike,
        d_OptionYears,
        riskfree,
        volatility,
        opt_n
    );
    cutilSafeCallNoSync( cudaThreadSynchronize() );

    /*
     * Copy result back to host
     */
//    cutilSafeCallNoSync( cudaMemcpy(h_CallResult, d_CallResult, opt_sz, cudaMemcpyDeviceToHost) );
//    cutilSafeCallNoSync( cudaMemcpy(h_PutResult,  d_PutResult,  opt_sz, cudaMemcpyDeviceToHost) );

    /*
     * Release device memory
     */
    cutilSafeCallNoSync( cudaFree(d_PutResult)    );
    cutilSafeCallNoSync( cudaFree(d_CallResult)   );
}

