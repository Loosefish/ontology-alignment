#!/bin/bash
echo "Running ..."
TRIVIAL=946

time ./match mouse.owl human.owl +RTS -N | sort > out
TP=$(comm -12 out matches | wc -l)
FP=$(comm -23 out matches | wc -l)
FN=$(comm -13 out matches | wc -l)

PRECISION=$(bc -l <<< "$TP / ($TP + $FP)")
RECALL=$(bc -l <<< "$TP / ($TP + $FN)")
FSCORE=$(bc -l <<< "(2 * $PRECISION * $RECALL) / ($PRECISION + $RECALL)")

echo "True positives: "$TP
echo "False positives: "$FP
echo "False negatives: "$FN
echo "Precision: "$PRECISION
echo "Recall: "$RECALL
echo "F-Score: "$FSCORE

rm out
