#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP bsrto_c_read_rdi(SEXP con);

static const R_CallMethodDef CallEntries[] = {
    {"bsrto_c_read_rdi", (DL_FUNC) &bsrto_c_read_rdi, 1},
    {NULL, NULL, 0}
};

void R_init_bsrto(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
