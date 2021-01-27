
#ifndef READ_RDI_SEXP_H
#define READ_RDI_SEXP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "read-rdi-common.h"

SEXP rdi_header_list(rdi_header_t* header, uint16_t* data_offset);
SEXP rdi_fixed_leader_data_list(rdi_fixed_leader_data_t* fixed);
SEXP rdi_variable_leader_data_list(rdi_variable_leader_data_t* variable);
SEXP rdi_bottom_track_list(rdi_bottom_track_t* bottom_track);
SEXP rdi_unknown_list(uint16_t magic_number);

#endif
