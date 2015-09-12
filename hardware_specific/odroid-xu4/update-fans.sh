#!/usr/bin/env bash

#
# Run as root as a systemd service at startup
#

# Set the triggering temp levels to 40/75/85
# Supposedly continuous operation at over 85C is not recommended
echo "40 75 85" > /sys/devices/odroid_fan.13/temp_levels
# Set the fan speeds to:
# 1 at below 40C (Off)
# 28% above 40C and below 75C
# 75% above 75C and below 85C
# 100% above 85C
echo "1 28 75 100" > /sys/devices/odroid_fan.13/fan_speeds

# Display the current fan speed and temperature
#
# Sensor 2 seems to be the processor, or atleast the one that triggers the fan speeds
#
# Sensor 4 is always lower than the other sensors and doesn't change as fast under CPU load. Possibly memory controller?
#cat /sys/devices/odroid_fan.13/pwm_duty && sudo cat /sys/devices/10060000.tmu/temp
