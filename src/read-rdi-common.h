
#ifndef READ_RDI_COMMON_H
#define READ_RDI_COMMON_H

#include <stdint.h>

// these structs are used as the source of "truth" for the other
// data structures (e.g., data-frame creation code is auto-generated from
// the struct definition)

// For readability and for auto-generating the code to translate
// the C structs into R objects
typedef uint16_t uint16_scaled_by_100_t;
typedef int16_t int16_scaled_by_100_t;
typedef uint8_t uint8_scaled_by_10_t;

#define RDI_ENSEMBLE_HEADER_UINT16 0x7f7f

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
    // see rdi_fixed_leader_data_t
    RDI_TYPE_FIXED_LEADER = 0x0000,
    // rdi_variable_leader_data_t
    RDI_TYPE_VARIABLE_LEADER = 0x0080,
    // int16_t[n_beams * n_cells]
    // -32768 as NA, divide by 1000 to get velocity in m/s
    RDI_TYPE_VELOCITY = 0x0100,
    // uint8_t[n_beams * n_cells]
    RDI_TYPE_CORRELATION = 0x0200,
    // uint8_t[n_beams * n_cells]
    RDI_TYPE_ECHO_INTENSITY = 0x0300,
    // uint8_t[n_beams * n_cells]
    RDI_TYPE_PCT_GOOD = 0x0400,
    // rdi_bottom_track_t
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
// Note that the fread() method suggested above will only work on
// little-endian systems (this can be checked easily at the R level).
typedef struct {
    uint16_t magic_number; // 0x00 then 0x00 or 0x01 then 0x00
    uint8_t firmware_version[2];
    uint8_t system_config[2];
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
} rdi_fixed_leader_data_t;

typedef struct {
    uint16_t magic_number;
    uint16_t ensemble_number;
    uint8_t real_time_clock[7];
    uint8_t ensemble_number_msb;
    uint16_t bit_result;
    uint16_t sound_speed;
    uint16_t transducer_depth;
    uint16_scaled_by_100_t heading;
    int16_scaled_by_100_t pitch;
    int16_scaled_by_100_t roll;
    int16_t salinity;
    int16_scaled_by_100_t temperature;
    uint8_t unknown[3];
    uint8_t heading_std;
    uint8_scaled_by_10_t pitch_std;
    uint8_scaled_by_10_t roll_std;
    uint8_t transmit_current;
    uint8_t transmit_voltage;
    uint8_t ambient_temperature;
    uint8_t pressure_plus;
    uint8_t pressure_minus;
    uint8_t attitude_temp;
    uint8_t attitude;
    uint8_t contamination_sensor;
    uint8_t unknown2[6];
    uint8_t pressure[4]; // int32_t scaled by 1000
    uint8_t pressure_std[4]; // int32_t (probably also scaled by 1000?)
} rdi_variable_leader_data_t;

// this struct may depend on the number of beams? (4 here)
typedef struct {
    uint16_t magic_number;
    uint8_t unknown[14];
    uint16_t range_lsb[4]; // 16-23
    int16_t bottom_track_velocity[4]; // 24: scaled by 1000
    uint8_t bc[4];
    uint8_t ba[4];
    uint8_t bg[4]; // 40-43
    uint8_t unknown2[33];
    uint8_t range_msb[4]; // 77
} rdi_bottom_track_t;

#endif
