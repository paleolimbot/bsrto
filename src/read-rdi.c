
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#include "read-rdi-common.h"
#include "read-rdi-sexp.h"

// get a human-readable label from the rdi_item_type enum
const char* rdi_item_type_label(uint16_t magic_number) {
    switch(magic_number) {
    case RDI_TYPE_FIXED_LEADER: return "fixed_leader";
    case RDI_TYPE_VARIABLE_LEADER: return "variable_leader";
    case RDI_TYPE_VELOCITY: return "velocity";
    case RDI_TYPE_CORRELATION: return "correlation";
    case RDI_TYPE_ECHO_INTENSITY: return "echo_intensity";
    case RDI_TYPE_PCT_GOOD: return "pct_good";
    case RDI_TYPE_BOTTOM_TRACK: return "bottom_track";
    case RDI_TYPE_SENTINEL_VERTICAL_BEAM_VELOCITY: return "sentinel_vertical_beam_velocity";
    case RDI_TYPE_SENTINEL_VERTICAL_BEAM_CORRECTION: return "sentinel_vertical_beam_correction";
    case RDI_TYPE_SENTINEL_VERTICAL_BEAM_AMPLITUDE: return "sentinel_vertical_beam_amplitude";
    case RDI_TYPE_SENTINEL_VERTICAL_BEAM_PCT_GOOD: return "sentinel_vertical_beam_pct_good";
    case RDI_TYPE_VMDASS: return "vmdass";
    case RDI_TYPE_BINARY_FIXED_ATTITUDE: return "binary_fixed_attitude";
    case RDI_TYPE_SENTINEL_TRANSFORMATION_MATRIX: return "sentinel_transformation_matrix";
    default: return "unknown";
    }
}

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup() but is also a
// nice abstraction around the file types
typedef struct {
    FILE* handle;
    int read_offsets;
} read_rdi_data_t;

void rdi_seek_absolute(read_rdi_data_t* data, size_t offset) {
    if (fseek(data->handle, offset, SEEK_SET) != 0) {
        Rf_error("Seek to file offset %d failed", offset);
    }
}

void rdi_read_uint16_n(uint16_t* buf, size_t n, read_rdi_data_t* data) {
    size_t size_read = fread(buf, sizeof(uint16_t), n, data->handle);
    if (size_read != n) {
        Rf_error("Read %d 16-bit integers but expected %d", size_read, n);
    }
}

uint16_t rdi_read_uint16(read_rdi_data_t* data) {
    uint16_t result;
    size_t size_read = fread(&result, sizeof(uint16_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Read %d 16-bit integers but expected %d", size_read, 1);
    }

    return result;
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

    if (variable->magic_number != 0x0080) {
        Rf_error(
            "Expected 0x8000 at start of variable leader data but found %#04x",
             variable->magic_number
        );
    }
}

SEXP rdi_find_profile_offsets(read_rdi_data_t* data, uint16_t ensemble_size, 
                              size_t start_offset, size_t end_offset) {
    // a maximum number of profiles we might expect given the ensemble size estimate
    R_xlen_t max_profiles_estimated = (end_offset - start_offset) / ensemble_size + 10;
    SEXP collector = PROTECT(Rf_allocVector(INTSXP, max_profiles_estimated));
    int* collector_int = INTEGER(collector);

    R_xlen_t n_profiles_found = 0;
    size_t offset = start_offset;
    size_t items_read = 0;
    uint16_t buf16 = 0x0000;
    while (offset < end_offset) {
        items_read = fread(&buf16, sizeof(uint16_t), 1, data->handle);
        if (items_read != 1) {
            break;
        }
        offset += items_read * 2;

        if (buf16 == 0x7f7f) {
            if (n_profiles_found >= max_profiles_estimated) {
                Rf_error(
                    "Attempt to add profile offset with index '%d' to vector of length '%d",
                    n_profiles_found, max_profiles_estimated
                );
            }
            collector_int[n_profiles_found] = offset;
            n_profiles_found++;
        }
    }

    // reallocate result to the number of profiles actually found
    SEXP result = PROTECT(Rf_allocVector(INTSXP, n_profiles_found));
    memcpy(INTEGER(result), INTEGER(collector), sizeof(int) * n_profiles_found);
    UNPROTECT(2);
    return result;
}

SEXP bsrto_c_read_rdi_meta_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;

    // read header
    rdi_header_t header;
    rdi_read_header(&header, data);
    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, data);

    // get machine- and human-readable ids for the data types in this file
    // leave room for the header as the first item
    uint16_t data_type[header.n_data_types];
    const char* data_type_name[header.n_data_types + 3];
    data_type_name[0] = "header";
    data_type_name[1] = "profile_offsets";
    data_type_name[header.n_data_types + 2] = "";
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        rdi_seek_absolute(data, data_offset[i]);
        data_type[i] = rdi_read_uint16(data);
        data_type_name[i + 2] = rdi_item_type_label(data_type[i]);
    }

    // allocate container using the human-readable names
    SEXP container = PROTECT(Rf_mkNamed(VECSXP, data_type_name));
    SET_VECTOR_ELT(container, 0, rdi_header_list(&header, data_offset));
    SET_VECTOR_ELT(container, 1, Rf_allocVector(VECSXP, header.n_data_types));

    // for each data type, read the type and convert it to a SEXP
    SEXP item;
    SEXP item_offsets;
    rdi_fixed_leader_data_t fixed;
    rdi_variable_leader_data_t variable;
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        rdi_seek_absolute(data, data_offset[i]);
        switch(data_type[i]) {
        case RDI_TYPE_FIXED_LEADER:
            rdi_read_fixed_leader_data(&fixed, data);
            item = PROTECT(rdi_fixed_leader_data_list(&fixed));
            break;
        case RDI_TYPE_VARIABLE_LEADER:
            rdi_read_variable_leader_data(&variable, data);
            item = PROTECT(rdi_variable_leader_data_list(&variable));
            break;
        default:
            item = PROTECT(rdi_unknown_list(data_type[i]));
            break;
        }

        if (data->read_offsets) {
            size_t this_offset = ftell(data->handle);
            size_t next_data;
            if (i < (header.n_data_types - 1)) {
                next_data = data_offset[i + 1];
            } else {
                fseek(data->handle, 0, SEEK_END);
                next_data = ftell(data->handle);
                rdi_seek_absolute(data, this_offset);
            }
            item_offsets = PROTECT(rdi_find_profile_offsets(data, header.bytes_per_ensemble, this_offset, next_data));
            SET_VECTOR_ELT(VECTOR_ELT(container, 1), i, item_offsets);
        }

        SET_VECTOR_ELT(container, i + 2, item);
        UNPROTECT(1);
    }
    
    UNPROTECT(1);
    return container;
}

void bsrto_c_read_rdi_cleanup(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    free(data);
}

SEXP bsrto_c_read_rdi_meta(SEXP filename, SEXP read_offsets) {
    const char* filename_chr = CHAR(STRING_ELT(filename, 0));
    int read_offsets_int = LOGICAL(read_offsets)[0];

    FILE* handle = fopen(filename_chr, "rb");
    if (handle == NULL) {
        Rf_error("Failed to open file '%s'", filename_chr);
    }

    read_rdi_data_t* data = (read_rdi_data_t*) malloc(sizeof(read_rdi_data_t));
    if (data == NULL) {
        Rf_error("Failed to allocate file handle data");
    }

    data->handle = handle;
    data->read_offsets = read_offsets_int;
    return R_ExecWithCleanup(&bsrto_c_read_rdi_meta_impl, data, &bsrto_c_read_rdi_cleanup, data);
}
