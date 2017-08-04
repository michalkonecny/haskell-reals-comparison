* Regenerate results.csv (and run any benchmarks for which there are no logs):

rm results.csv
bash runBench.sh results.csv

* Generate svg execution time chart:

aern2-bench-chart CSVName Method BenchN Log ExecTime Log logistic.csv charts

* Generate svg max memory usage chart:

aern2-bench-chart CSVName Method BenchN Log MaxMem Log logistic.csv charts

* Convert all svgs to pngs suitable for web:

cd charts
for f in *.svg; do rsvg-convert -z 2.0 $f -o ${f/.svg/.png}; done

* Convert all svgs to good quality pdfs:

cd charts
for f in *.svg; do inkscape -z -f $f --export-pdf=${f/.svg/.pdf}; done
