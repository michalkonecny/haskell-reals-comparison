* Regenerate logistic-fine.csv (and run any benchmarks for which there are no logs):

rm logistic-fine.csv
bash runBench.sh logistic-fine.csv

* Generate svg execution time chart:

aern2-bench-chart CSVName Method BenchN Log ExecTime Log logistic-fine.csv charts

* Generate svg max memory usage chart:

aern2-bench-chart CSVName Method BenchN Log MaxMem Log logistic-fine.csv charts

* Convert all svgs to pngs suitable for web:

cd charts
for f in *.svg; do rsvg-convert -z 2.0 $f -o ${f/.svg/.png}; done

* Convert all svgs to good quality pdfs:

cd charts
for f in *.svg; do inkscape -z -f $f --export-pdf=${f/.svg/.pdf}; done
