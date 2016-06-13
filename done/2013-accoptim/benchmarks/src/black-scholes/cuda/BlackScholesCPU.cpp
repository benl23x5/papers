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

#define OPT_N           4000000
#define NUM_ITERATIONS  512


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
    //CPU instance of input data
    *h_StockPrice,
    *h_OptionStrike,
    *h_OptionYears;

    double cpuTime;

    StopWatchInterface *hTimer = NULL;
    int i;

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
    h_StockPrice    = (float *)malloc(opt_sz);
    h_OptionStrike  = (float *)malloc(opt_sz);
    h_OptionYears   = (float *)malloc(opt_sz);

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
    printf("Data init done.\n\n");

    printf("Executing Black-Scholes CPU function (%i iterations)...\n", num_iterations);

    sdkResetTimer(&hTimer);
    sdkStartTimer(&hTimer);

    for (i = 0; i < num_iterations; i++)
    {
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
    }

    sdkStopTimer(&hTimer);
    cpuTime = sdkGetTimerValue(&hTimer) / num_iterations;

    //Both call and put is calculated
    printf("Options count             : %i\n",      opt_n);
    printf("BlackScholesCPU() time    : %f msec\n", cpuTime);
    printf("Effective memory bandwidth: %f GB/s\n", ((double)(5 * opt_n * sizeof(float)) * 1E-9) / (cpuTime * 1E-3));
    printf("Gigaoptions per second    : %f\n\n",    ((double)(2 * opt_n) * 1E-9) / (cpuTime * 1E-3));

    printf("BlackScholes, Throughput = %.4f GOptions/s, Time = %.5f s, Size = %u options\n",
           (((double)(2.0 * opt_n) * 1.0E-9) / (cpuTime * 1.0E-3)), cpuTime*1e-3, (2 * opt_n));

    printf("...releasing CPU memory.\n");
    free(h_OptionYears);
    free(h_OptionStrike);
    free(h_StockPrice);
    free(h_PutResultCPU);
    free(h_CallResultCPU);
    sdkDeleteTimer(&hTimer);

    printf("Shutdown done.\n");

    return 0;
}
