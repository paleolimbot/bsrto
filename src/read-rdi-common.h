
#ifndef READ_RDI_COMMON_H
#define READ_RDI_COMMON_H

#include <stdint.h>

// these structs are used as the source of "truth" for the other 
// data structures (e.g., data-frame creation code is auto-generated from
// the struct definition)

// For readability and for auto-generating the code to translate
// the C structs into R objects
typedef uint16_t uint16_scaled_by_100_t;

typedef struct {
    uint16_t magic_number; // 0x7f7f
    uint16_t bytes_per_ensemble;
    uint8_t padding;
    uint8_t n_data_types;
    // read separately:
    // uint16_t data_offset[n_data_types];
} rdi_header_t;

// Writing these as little-endian uint16_t instead of
// byte one...byte two because each struct has to start with
// a uint16_t to ensure that the members are arranged without
// any padding. These are best documented on the oce reference
// page for read.adp.rdi(). These are the values pointed to
// by the `data_offset` immediately following the rdi_header_t
// that each have a recipe for reading.
enum rdi_item_type {
    RDI_TYPE_FIXED_LEADER = 0x0000,
    RDI_TYPE_VARIABLE_LEADER = 0x0080,
    RDI_TYPE_VELOCITY = 0x0100,
    RDI_TYPE_CORRELATION = 0x0200,
    RDI_TYPE_ECHO_INTENSITY = 0x0300,
    RDI_TYPE_PCT_GOOD = 0x0400,
    RDI_TYPE_BOTTOM_TRACK = 0x0600,
    RDI_TYPE_SENTINEL_VERTICAL_BEAM_VELOCITY = 0x0a00,
    RDI_TYPE_SENTINEL_VERTICAL_BEAM_CORRECTION = 0x0b00,
    RDI_TYPE_SENTINEL_VERTICAL_BEAM_AMPLITUDE = 0x0c00,
    RDI_TYPE_SENTINEL_VERTICAL_BEAM_PCT_GOOD = 0x0d00,
    RDI_TYPE_VMDASS = 0x2000,
    RDI_TYPE_BINARY_FIXED_ATTITUDE = 0x3000,
    RDI_TYPE_SENTINEL_TRANSFORMATION_MATRIX = 0x3200
};

// These structs are laid out such that there is no padding between
// members. This makes it easy to read all at once using
// fread(&str, 1, 1, file_handle). To make this work they have to
// start with a uint16_t and contain no members wider than 2 bytes.
typedef struct {
    uint16_t magic_number; // 0x00 then 0x00 or 0x01 then 0x00
    uint8_t firmware_version[2];
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
    // this is actually an int32_t; however, declaring it as such causes
    // padding between members that causes the integer to be read
    // at the wrong location
    uint8_t serial_number[4];
    uint8_t beam_angle;
    // trailing padding, possibly variable length
    // Sentinel IV config bytes possibly here
} rdi_fixed_leader_data_t;

typedef struct {
    uint16_t magic_number; // 0x80 then 0x00
    uint16_t ensemble_number;
    uint8_t real_time_clock[7];
    uint8_t ensemble_number_msb;
    uint16_t bit_result;
    uint16_t sound_speed;
    uint16_t tranducer_depth;
    // there is more here that is not included in oce::read.adp.rdi()
} rdi_variable_leader_data_t;

#endif
