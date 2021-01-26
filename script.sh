exe='benchmarks/launch.exe'
json_file='output.json'
output='result.html'

dune build

./_build/default/$exe > $json_file


bechamel-html < $json_file > $output
