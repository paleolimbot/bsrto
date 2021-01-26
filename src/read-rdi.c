
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#include "read-rdi-common.h"
#include "read-rdi-sexp.h"

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup() but is also a
// nice abstraction around the file types
typedef struct {
    FILE* handle;
} read_rdi_data_t;

// these functions read FILE->struct and check the magic numbers at the start of each
void rdi_read_uint16_n(uint16_t* buf, size_t n, read_rdi_data_t* data) {
    size_t size_read = fread(buf, sizeof(uint16_t), n, data->handle);
    if (size_read != n) {
        Rf_error("Read %d 16-bit integers but expected %d", size_read, n);
    }
}

void rdi_read_header(rdi_header_t* header, read_rdi_data_t* data) {
    size_t size_read = fread(header, sizeof(rdi_header_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete header");
    }

    if (header->magic_number != 0x7f7f) {
        Rf_error(
            "Expected 0x7f7f at start of file but found %#04x",
            header->magic_number
        );
    }
}

void rdi_read_fixed_leader_data(rdi_fixed_leader_data_t* fixed, read_rdi_data_t* data) {
    size_t size_read = fread(fixed, sizeof(rdi_fixed_leader_data_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete fixed leader data");
    }

    if (fixed->magic_number != 0x0000 && fixed->magic_number != 0x0001) {
        Rf_error(
            "Expected 0x0000 or 0x0100 at start of fixed leader data but found %#04x",
             fixed->magic_number
        );
    }
}

void rdi_read_variable_leader_data(rdi_variable_leader_data_t* variable, read_rdi_data_t* data) {
    size_t size_read = fread(variable, sizeof(rdi_fixed_leader_data_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete variable leader data");
    }

    if (variable->magic_number != 0x0008) {
        // Rf_error(
        //     "Expected 0x8000 at start of variable leader data but found %#04x",
        //      variable->magic_number
        // );
    }
}

SEXP bsrto_c_read_rdi_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;

    // read C structs and check information that affects
    // the ability for valid results to be calculated

    // header is always at the start of a file
    rdi_header_t header;
    rdi_read_header(&header, data);
    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, data);

    // fixed leader data is located at the data_offset[0]
    if (fseek(data->handle, data_offset[0], SEEK_SET) != 0) {
        Rf_error("Can't seek to data_offset position %d", data_offset[0]);
    }
    rdi_fixed_leader_data_t fixed;
    rdi_read_fixed_leader_data(&fixed, data);

    // variable leader data is located at the data_offset[1]
    if (fseek(data->handle, data_offset[1], SEEK_SET) != 0) {
        Rf_error("Can't seek to data_offset position %d", data_offset[1]);
    }
    rdi_variable_leader_data_t variable;
    rdi_read_variable_leader_data(&variable, data);

    // generate R objects from C structs
    const char* meta_names[] = {"header", "fixed_leader_data", "variable_leader_data", ""};
    SEXP r_meta = PROTECT(Rf_mkNamed(VECSXP, meta_names));
    SET_VECTOR_ELT(r_meta, 0, rdi_header_list(&header, data_offset));
    SET_VECTOR_ELT(r_meta, 1, rdi_fixed_leader_data_list(&fixed));
    SET_VECTOR_ELT(r_meta, 2, rdi_variable_leader_data_list(&variable));

    UNPROTECT(1);
    return r_meta;
}

void bsrto_c_read_rdi_cleanup(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    free(data);
}

SEXP bsrto_c_read_rdi(SEXP filename) {
    const char* filename_chr = CHAR(STRING_ELT(filename, 0));

    FILE* handle = fopen(filename_chr, "rb");
    if (handle == NULL) {
        Rf_error("Failed to open file '%s'", filename_chr);
    }

    read_rdi_data_t* data = (read_rdi_data_t*) malloc(sizeof(read_rdi_data_t));
    if (data == NULL) {
        Rf_error("Failed to allocate file handle data");
    }

    data->handle = handle;
    return R_ExecWithCleanup(&bsrto_c_read_rdi_impl, data, &bsrto_c_read_rdi_cleanup, data);
}
