#!/bin/bash
touch ../log/job_$1.txt
for ARRAY in {1..200}
do
  seff $1_$ARRAY >> ../log/job_$1.txt
done
