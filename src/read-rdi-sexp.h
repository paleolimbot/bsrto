
#ifndef READ_RDI_SEXP_H
#define READ_RDI_SEXP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "read-rdi-common.h"

SEXP rdi_fixed_leader_data_list(rdi_fixed_leader_data_t* fixed);
SEXP rdi_variable_leader_data_list(rdi_variable_leader_data_t* variable);

#endif
