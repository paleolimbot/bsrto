#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t magic_number;
    uint16_t bytes_per_ensemble;
    uint8_t unused;
    uint8_t n_data_types;
    // uint16_t data_offset[n_data_types];
} rdi_header_t;

typedef struct {
    uint16_t magic_number;
    uint8_t firmware_version_major;
    uint8_t firmware_version_minor;
    uint8_t system_config1;
    uint8_t system_config2;
    uint8_t real_sim_flag;
    uint8_t lag_length;
    uint8_t n_beams;
    uint8_t n_cells;
    uint16_t pings_per_ensemble;
    uint16_t cell_size100;
    uint16_t blank_after_transmit100;
    uint8_t profiling_mode;
    uint8_t low_corr_thresh;
    uint8_t n_code_reps;
    uint8_t pct_gd_min;
    uint16_t error_velocity_maximum;
    uint8_t tpp_minutes;
    uint8_t tpp_seconds;
    uint8_t tpp_hundredths;
    uint8_t coord_transform;
    uint16_t heading_alignment100;
    uint16_t heading_bias100;
    uint8_t sensor_source; // 31
    uint8_t sensors_available;
    uint16_t bin1_distance100;
    uint16_t transmit_pulse_length100;
    uint16_t wp_ref_layer_average;
    uint8_t false_target_threshold;
    uint8_t unused; // padding
    uint16_t transmit_lag_distance; //41
    uint8_t cpu_board_searial_number[8];
    uint16_t system_bandwidth; //51
    uint8_t system_power;
    uint8_t unused2; // padding
    uint32_t serial_number;
    uint8_t beam_angle;
    // probably some padding here, but the location of the next
    // element is specified in the header by data_offset[1]
} rdi_fixed_leader_data_t;



void rdi_read_uint16_n(uint16_t* buf, size_t n, FILE* f) {
    size_t size_read = fread(buf, sizeof(uint16_t), n, f);
    if (size_read != n) {
        Rf_error("Read %d 16-bit integers but expected %d", size_read, n);
    }
}

rdi_header_t rdi_read_header(FILE* f) {
    rdi_header_t header;
    size_t size_read = fread(&header, sizeof(rdi_header_t), 1, f);
    if (size_read != 1) {
        Rf_error("Failed to read header");
    }
    return header;
}

typedef struct {
    FILE* handle;
} read_rdi_data_t;

SEXP bsrto_c_read_rdi_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;


    rdi_header_t header = rdi_read_header(data->handle);
    if (header.magic_number != 0x7f7f) {
        Rf_error("Expected magic number 0x7f7f but found %#04x", header.magic_number);
    }

    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, data->handle);

    const char* header_names[] = {"bytes_per_ensemble", "n_data_types", "data_offset", ""};
    SEXP r_header = PROTECT(Rf_mkNamed(VECSXP, header_names));
    SET_VECTOR_ELT(r_header, 0, Rf_ScalarInteger(header.bytes_per_ensemble));
    SET_VECTOR_ELT(r_header, 1, Rf_ScalarInteger(header.n_data_types));

    SEXP r_data_offset = PROTECT(Rf_allocVector(INTSXP, header.n_data_types));
    for (uint16_t i = 0; i < header.n_data_types; i++) {
        INTEGER(r_data_offset)[i] = data_offset[i];
    }
    SET_VECTOR_ELT(r_header, 2, r_data_offset);
    UNPROTECT(1);

    UNPROTECT(1);
    return r_header;
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
