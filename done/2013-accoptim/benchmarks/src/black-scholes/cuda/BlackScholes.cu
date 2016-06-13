/*
 * Copyright 1993-2012 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

/*
 * This sample evaluates fair call and put prices for a
 * given set of European options by Black-Scholes formula.
 * See supplied whitepaper for more explanations.
 */


#include <helper_functions.h>   // helper functions for string parsing
#include <helper_cuda.h>        // helper functions CUDA error checking and initialization

////////////////////////////////////////////////////////////////////////////////
// Process an array of optN options on CPU
////////////////////////////////////////////////////////////////////////////////
extern "C" void BlackScholesCPU(
    float *h_CallResult,
    float *h_PutResult,
    float *h_StockPrice,
    float *h_OptionStrike,
    float *h_OptionYears,
    float Riskfree,
    float Volatility,
    int optN
);

////////////////////////////////////////////////////////////////////////////////
// Process an array of OptN options on GPU
////////////////////////////////////////////////////////////////////////////////
#include "BlackScholes_kernel.cuh"

////////////////////////////////////////////////////////////////////////////////
// Helper function, returning uniformly distributed
// random float in [low, high] range
////////////////////////////////////////////////////////////////////////////////
float RandFloat(float low, float high)
{
    float t = (float)rand() / (float)RAND_MAX;
    return (1.0f - t) * low + t * high;
}

////////////////////////////////////////////////////////////////////////////////
// Data configuration
////////////////////////////////////////////////////////////////////////////////
// const int OPT_N = 4000000;
// const int  NUM_ITERATIONS = 512;

#define OPT_N           4000000
#define NUM_ITERATIONS  512


// const int          OPT_SZ = OPT_N * sizeof(float);
const float      RISKFREE = 0.02f;
const float    VOLATILITY = 0.30f;

////////////////////////////////////////////////////////////////////////////////
// Main program
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
    // Start logs
    printf("[%s] - Starting...\n", argv[0]);

    //'h_' prefix - CPU (host) memory space
    float
    //Results calculated by CPU for reference
    *h_CallResultCPU,
    *h_PutResultCPU,
    //CPU copy of GPU results
    *h_CallResultGPU,
    *h_PutResultGPU,
    //CPU instance of input data
    *h_StockPrice,
    *h_OptionStrike,
    *h_OptionYears;

    //'d_' prefix - GPU (device) memory space
    float
    //Results calculated by GPU
    *d_CallResult,
    *d_PutResult,
    //GPU instance of input data
    *d_StockPrice,
    *d_OptionStrike,
    *d_OptionYears;

//    double delta, ref, sum_delta, sum_ref, max_delta, L1norm;
    double gpuTime;

    StopWatchInterface *hTimer = NULL;
    int i;

    findCudaDevice(argc, (const char **)argv);

    // determine how many options to process
    int opt_n = OPT_N;
    if (checkCmdLineFlag(argc, (const char**) argv, "options"))
    {
        opt_n   = getCmdLineArgumentInt(argc, (const char**)argv, "options");
        if (opt_n < 1) {
            printf("Error: \"number of options\" specified %d is invalid\n", opt_n);
            exit(EXIT_FAILURE);
        }
    }
    const int opt_sz = opt_n * sizeof(float);

    // how many benchmarking iterations
    int num_iterations = NUM_ITERATIONS;
    if (checkCmdLineFlag(argc, (const char**) argv, "samples"))
    {
        num_iterations = getCmdLineArgumentInt(argc, (const char**)argv, "samples");
        if (opt_n < 1) {
            printf("Error: \"number of benchmark samples\" specified %d is invalid\n", num_iterations);
            exit(EXIT_FAILURE);
        }
    }

    sdkCreateTimer(&hTimer);

    printf("Initializing data...\n");
    printf("...allocating CPU memory for options.\n");
    h_CallResultCPU = (float *)malloc(opt_sz);
    h_PutResultCPU  = (float *)malloc(opt_sz);
    h_CallResultGPU = (float *)malloc(opt_sz);
    h_PutResultGPU  = (float *)malloc(opt_sz);
    h_StockPrice    = (float *)malloc(opt_sz);
    h_OptionStrike  = (float *)malloc(opt_sz);
    h_OptionYears   = (float *)malloc(opt_sz);

    printf("...allocating GPU memory for options.\n");
//    checkCudaErrors(cudaMalloc((void **)&d_CallResult,   opt_sz));
//    checkCudaErrors(cudaMalloc((void **)&d_PutResult,    opt_sz));
    checkCudaErrors(cudaMalloc((void **)&d_StockPrice,   opt_sz));
    checkCudaErrors(cudaMalloc((void **)&d_OptionStrike, opt_sz));
    checkCudaErrors(cudaMalloc((void **)&d_OptionYears,  opt_sz));

    printf("...generating input data in CPU mem.\n");
    srand(5347);

    //Generate options set
    for (i = 0; i < opt_n; i++)
    {
        h_CallResultCPU[i] = 0.0f;
        h_PutResultCPU[i]  = -1.0f;
        h_StockPrice[i]    = RandFloat(5.0f, 30.0f);
        h_OptionStrike[i]  = RandFloat(1.0f, 100.0f);
        h_OptionYears[i]   = RandFloat(0.25f, 10.0f);
    }

    printf("...copying input data to GPU mem.\n");
    //Copy options data to GPU memory for further processing
    checkCudaErrors(cudaMemcpy(d_StockPrice,  h_StockPrice,   opt_sz, cudaMemcpyHostToDevice));
    checkCudaErrors(cudaMemcpy(d_OptionStrike, h_OptionStrike,  opt_sz, cudaMemcpyHostToDevice));
    checkCudaErrors(cudaMemcpy(d_OptionYears,  h_OptionYears,   opt_sz, cudaMemcpyHostToDevice));
    printf("Data init done.\n\n");


    printf("Executing Black-Scholes GPU kernel (%i iterations)...\n", num_iterations);
    checkCudaErrors(cudaDeviceSynchronize());
    sdkResetTimer(&hTimer);
    sdkStartTimer(&hTimer);

    for (i = 0; i < num_iterations; i++)
    {
        checkCudaErrors(cudaMalloc((void **)&d_CallResult, opt_sz));    // TLM
        checkCudaErrors(cudaMalloc((void **)&d_PutResult,  opt_sz));    // TLM

        BlackScholesGPU<<<480, 128>>>(
            d_CallResult,
            d_PutResult,
            d_StockPrice,
            d_OptionStrike,
            d_OptionYears,
            RISKFREE,
            VOLATILITY,
            opt_n
        );
        getLastCudaError("BlackScholesGPU() execution failed\n");

        checkCudaErrors(cudaFree(d_PutResult));         // TLM
        checkCudaErrors(cudaFree(d_CallResult));        // TLM
    }

    checkCudaErrors(cudaDeviceSynchronize());
    sdkStopTimer(&hTimer);
    gpuTime = sdkGetTimerValue(&hTimer) / num_iterations;

    //Both call and put is calculated
    printf("Options count             : %i\n",      opt_n);
    printf("BlackScholesGPU() time    : %f msec\n", gpuTime);
    printf("Effective memory bandwidth: %f GB/s\n", ((double)(5 * opt_n * sizeof(float)) * 1E-9) / (gpuTime * 1E-3));
    printf("Gigaoptions per second    : %f\n\n",    ((double)(2 * opt_n) * 1E-9) / (gpuTime * 1E-3));

    printf("BlackScholes, Throughput = %.4f GOptions/s, Time = %.5f s, Size = %u options, NumDevsUsed = %u, Workgroup = %u\n",
           (((double)(2.0 * opt_n) * 1.0E-9) / (gpuTime * 1.0E-3)), gpuTime*1e-3, (2 * opt_n), 1, 128);

#if 0
    printf("\nReading back GPU results...\n");
    //Read back GPU results to compare them to CPU results
    checkCudaErrors(cudaMemcpy(h_CallResultGPU, d_CallResult, opt_sz, cudaMemcpyDeviceToHost));
    checkCudaErrors(cudaMemcpy(h_PutResultGPU,  d_PutResult,  opt_sz, cudaMemcpyDeviceToHost));


    printf("Checking the results...\n");
    printf("...running CPU calculations.\n\n");
    //Calculate options values on CPU
    BlackScholesCPU(
        h_CallResultCPU,
        h_PutResultCPU,
        h_StockPrice,
        h_OptionStrike,
        h_OptionYears,
        RISKFREE,
        VOLATILITY,
        opt_n
    );

    printf("Comparing the results...\n");
    //Calculate max absolute difference and L1 distance
    //between CPU and GPU results
    sum_delta = 0;
    sum_ref   = 0;
    max_delta = 0;

    for (i = 0; i < opt_n; i++)
    {
        ref   = h_CallResultCPU[i];
        delta = fabs(h_CallResultCPU[i] - h_CallResultGPU[i]);

        if (delta > max_delta)
        {
            max_delta = delta;
        }

        sum_delta += delta;
        sum_ref   += fabs(ref);
    }

    L1norm = sum_delta / sum_ref;
    printf("L1 norm: %E\n", L1norm);
    printf("Max absolute error: %E\n\n", max_delta);
#endif

    printf("Shutting down...\n");
    printf("...releasing GPU memory.\n");
    checkCudaErrors(cudaFree(d_OptionYears));
    checkCudaErrors(cudaFree(d_OptionStrike));
    checkCudaErrors(cudaFree(d_StockPrice));
//    checkCudaErrors(cudaFree(d_PutResult));
//    checkCudaErrors(cudaFree(d_CallResult));

    printf("...releasing CPU memory.\n");
    free(h_OptionYears);
    free(h_OptionStrike);
    free(h_StockPrice);
    free(h_PutResultGPU);
    free(h_CallResultGPU);
    free(h_PutResultCPU);
    free(h_CallResultCPU);
    sdkDeleteTimer(&hTimer);

    cudaDeviceReset();
    printf("Shutdown done.\n");

#if 0
    printf("\n[BlackScholes] - Test Summary\n");

    if (L1norm > 1e-6)
    {
        printf("Test failed!\n");
        exit(EXIT_FAILURE);
    }

    printf("Test passed\n");
    exit(EXIT_SUCCESS);
#endif
}
