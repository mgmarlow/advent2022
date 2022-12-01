require "benchmark"

task(:benchmark) do
  Benchmark.bm do |x|
    Dir["./src/*.lisp"].each do |filename|
      x.report(filename + ": ") { puts `sbcl --script #{filename}` }
    end
  end
end
