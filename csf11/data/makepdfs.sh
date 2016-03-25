#!/bin/bash

for f in *.eps ; do
  ps2pdf -dEPSCrop "$f"
done;
