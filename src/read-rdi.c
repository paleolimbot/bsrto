#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup() but is also a
// nice abstraction around the file types
typedef struct {
    FILE* handle;
} read_rdi_data_t;


// these structs are used as the source of "truth" for the other 
// data structures (e.g., data-frame code is auto-generated from
// the struct definition)

// For readability and for auto-generating the code to translate
// the C structs into R objects
typedef uint16_t uint16_scaled_by_100_t;

typedef struct {
    uint16_t magic_number; // 0x7f7f
    uint16_t bytes_per_ensemble;
    uint8_t padding;
    uint8_t n_data_types;
    // uint16_t data_offset[n_data_types];
} rdi_header_t;

typedef struct {
    uint16_t magic_number; // 0x0000 or 0x0100
    uint8_t firmware_version_major;
    uint8_t firmware_version_minor;
    uint16_t system_config;
    uint8_t real_sim_flag;
    uint8_t lag_length;
    uint8_t n_beams;
    uint8_t n_cells;
    uint16_t pings_per_ensemble;
    uint16_scaled_by_100_t cell_size;
    uint16_scaled_by_100_t blank_after_transmit;
    uint8_t profiling_mode;
    uint8_t low_corr_thresh;
    uint8_t n_code_reps;
    uint8_t pct_gd_min;
    uint16_t error_velocity_maximum;
    uint8_t tpp_minutes;
    uint8_t tpp_seconds;
    uint8_t tpp_hundredths;
    uint8_t coord_transform;
    uint16_scaled_by_100_t heading_alignment;
    uint16_scaled_by_100_t heading_bias;
    uint8_t sensor_source;
    uint8_t sensors_available;
    uint16_scaled_by_100_t bin1_distance;
    uint16_scaled_by_100_t transmit_pulse_length;
    uint16_t wp_ref_layer_average;
    uint8_t false_target_threshold;
    uint8_t padding;
    uint16_t transmit_lag_distance;
    uint8_t cpu_board_serial_number[8];
    uint16_t system_bandwidth;
    uint8_t system_power;
    uint8_t padding2;
    uint32_t serial_number;
    uint8_t beam_angle;
    // trailing padding, possibly variable length
    // Sentinel IV config bytes possibly here
} rdi_fixed_leader_data_t;

typedef struct {
    uint16_t magic_number; // 0x8000
    uint16_t ensemble_number;
    uint8_t real_time_clock[7];
    uint8_t ensemble_number_msb;
    uint16_t bit_result;
    uint16_t sound_speed;
    uint16_t tranducer_depth;
    // there is more here that is not included in oce::read.adp.rdi()
} rdi_variable_leader_data_t;

// these functions read and check the magic numbers at the start of each

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

    if (fixed->magic_number != 0x0000 && fixed->magic_number != 0x0100) {
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

    if (variable->magic_number != 0x8000) {
        Rf_error(
            "Expected 0x8000 at start of variable leader data but found %#04x",
             variable->magic_number
        );
    }
}

// these functions are auto-generated based on the above struct objects
SEXP rdi_fixed_leader_data_list(rdi_fixed_leader_data_t* fixed);
SEXP rdi_variable_leader_data_list(rdi_variable_leader_data_t* variable);

#include "rdi-items.inc"

SEXP rdi_header_list(read_rdi_data_t* data, rdi_header_t* header, uint16_t* data_offset) {
    const char* header_names[] = {"bytes_per_ensemble", "n_data_types", "data_offset", ""};
    SEXP r_header = PROTECT(Rf_mkNamed(VECSXP, header_names));
    SET_VECTOR_ELT(r_header, 0, Rf_ScalarInteger(header->bytes_per_ensemble));
    SET_VECTOR_ELT(r_header, 1, Rf_ScalarInteger(header->n_data_types));

    // wrap in list() so that this can be a data.frame
    SEXP r_data_offset_list = PROTECT(Rf_allocVector(VECSXP, 1));
    SEXP r_data_offset = PROTECT(Rf_allocVector(INTSXP, header->n_data_types));
    for (uint16_t i = 0; i < header->n_data_types; i++) {
        INTEGER(r_data_offset)[i] = data_offset[i];
    }

    SET_VECTOR_ELT(r_data_offset_list, 0, r_data_offset);
    SET_VECTOR_ELT(r_header, 2, r_data_offset_list);
    UNPROTECT(2);

    UNPROTECT(1);
    return r_header;
}

SEXP bsrto_c_read_rdi_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;

    // container for meta information
    const char* meta_names[] = {"header", "fixed_leader_data", "variable_leader_data", ""};
    SEXP r_meta = PROTECT(Rf_mkNamed(VECSXP, meta_names));

    // header is always at the start of a file
    rdi_header_t header;
    rdi_read_header(&header, data);
    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, data);
    SEXP r_header = PROTECT(rdi_header_list(data, &header, data_offset));
    SET_VECTOR_ELT(r_meta, 0, r_header);
    UNPROTECT(1);

    // fixed leader data is located at the data_offset[0]
    if (fseek(data->handle, data_offset[0], SEEK_SET) != 0) {
        Rf_error("Can't seek to data_offset position %d", data_offset[0]);
    }

    rdi_fixed_leader_data_t fixed;
    rdi_read_fixed_leader_data(&fixed, data);

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
