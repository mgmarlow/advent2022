require "benchmark"

task(:benchmark) do
  Benchmark.bm do |x|
    Dir["./src/*.rb"].each do |filename|
      x.report(filename) { ruby(filename) }
    end
  end
end
