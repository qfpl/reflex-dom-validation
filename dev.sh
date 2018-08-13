#!/usr/bin/env bash
ghcid --command="ghci -ghci-script=dev.ghci" -W --test="myTest" --reload=./assets
