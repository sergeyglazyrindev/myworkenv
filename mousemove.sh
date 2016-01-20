#!/usr/bin/env bash

sleep_period=60s

while true; do
        xset -dpms; xset s off
        xset +dpms; xset s on
    sleep ${sleep_period}
done
