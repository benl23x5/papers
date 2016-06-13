#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cublas.h>

/*
 * Host implementation of a simple version of scalar dot-product
 */
static float dotp(int n, const float *x, int incx, const float *y, int incy)
{
    int i, lx, ly;
    float dot = 0;
    lx = incx >= 0 ? 0 : (1 + (1 - n) * incx);
    ly = incy >= 0 ? 0 : (1 + (1 - n) * incy);
    for (i = 0; i < n; ++i) {
        dot += x[lx + i * incx] * y[ly + i * incy];
    }
    return dot;
}


/*
 * CUBLAS version
 */
float dotp_cublas(int n, float* d_x, int incx, float* d_y, int incy)
{
    cublasStatus status;
    float dot;

    /*
     * Clear last error
     */
    cublasGetError();

    /*
     * Performs operation using CUBLAS
     */
    dot    = cublasSdot(n, d_x, incx, d_y, incy);
    status = cublasGetError();
    if (status != CUBLAS_STATUS_SUCCESS) {
        fprintf(stderr, "!!!! kernel execution error.\n");
        exit(EXIT_FAILURE);
    }

    return dot;
}


/*
 * Main ------------------------------------------------------------------------
 */
int main(int argc, char **argv)
{
    const int n = argc < 2 ? 1024 : atoi(argv[1]);
    float *h_x  = NULL;
    float *h_y  = NULL;
    float *d_x  = NULL;
    float *d_y  = NULL;
    int i;

    /*
     * Initialize CUBLAS
     */
    printf("Running dot-product test with %lf million elements...\n", n / 1000000.0);

    if (CUBLAS_STATUS_SUCCESS != cublasInit()) {
        fprintf(stderr, "!!!! CUBLAS initialization error\n");
        exit(EXIT_FAILURE);
    }

    /*
     * Allocate host memory for the vectors. Fill with test data.
     */
    h_x = (float *) malloc(n * sizeof(float));
    h_y = (float *) malloc(n * sizeof(float));
    if (h_x == NULL || h_y == NULL) {
        fprintf(stderr, "!!!! host memory allocation error\n");
        exit(EXIT_FAILURE);
    }

    for (i = 0; i < n; i++) {
        h_x[i] = rand() / (float) RAND_MAX;
        h_y[i] = rand() / (float) RAND_MAX;
    }

    /*
     * Allocate device memory for the vectors, initialise
     */
    if (CUBLAS_STATUS_SUCCESS != cublasAlloc(n, sizeof(float), (void**) &d_x)
        ||
        CUBLAS_STATUS_SUCCESS != cublasAlloc(n, sizeof(float), (void**) &d_y)
    ) {
        fprintf(stderr, "!!!! device memory allocation error\n");
        exit(EXIT_FAILURE);
    }

    if (CUBLAS_STATUS_SUCCESS != cublasSetVector(n, sizeof(float), h_x, 1, d_x, 1)
        ||
        CUBLAS_STATUS_SUCCESS != cublasSetVector(n, sizeof(float), h_y, 1, d_y, 1)
    ) {
        fprintf(stderr, "!!!! device access error (write data)\n");
        exit(EXIT_FAILURE);
    }

    /*
     * Performs operation using plain C code, compare against the CUBLAS result
     */
    float error_norm;
    float ref_norm;
    float diff;

    const float ref = dotp(n, h_x, 1, h_y, 1);
    const float dot = dotp_cublas(n, d_x, 1, d_y, 1);

    error_norm  = 0;
    ref_norm    = 0;
    diff        = ref - dot;
    error_norm  = diff * diff;
    ref_norm    = ref * ref;
    error_norm  = (float) sqrt((double)error_norm);
    ref_norm    = (float) sqrt((double)ref_norm);
    if (fabs(ref_norm) < 1e-7) {
        fprintf(stderr, "!!!! reference norm is 0\n");
        exit(EXIT_FAILURE);
    }
    int ok      = error_norm / ref_norm < 1e-5f;
    if (ok) {
        printf("PASSED\n");
    } else {
        printf("FAILED: %f vs. %f, error_norm: %f\n", ref, dot, error_norm);
    }

    /*
     * Run many times and estimate performance
     */
    for (i = 0; i < 100; ++i) {
        dotp_cublas(n, d_x, 1, d_y, 1);
    }
    cudaDeviceSynchronize();

    /*
     * Cleanup & shutdown
     */
    free(h_x);
    free(h_y);
    if (CUBLAS_STATUS_SUCCESS != cublasFree(d_x)
        ||
        CUBLAS_STATUS_SUCCESS != cublasFree(d_y)
    ) {
        fprintf(stderr, "!!!! memory free error\n");
        exit(EXIT_FAILURE);
    }

    if (CUBLAS_STATUS_SUCCESS != cublasShutdown()) {
        fprintf(stderr, "!!!! shutdown error\n");
        exit(EXIT_FAILURE);
    }

    cudaDeviceReset();

    return 0;
}

