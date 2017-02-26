task default: %w[test]

task :clean do
  rm_f "a.out"
  rm_rf "specbin/"
end

task :test do
  sh "cargo test"
  ruby "specs.rb"
end
