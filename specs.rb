#! /usr/bin/env ruby

require 'test/unit'

# Expected output
#
# Represents a simple check which ensures that the given output is
# present when the program is run.
class ExpectedOutput

  # Initialise Expected Output
  #
  # Create a new instance of the check, passing in the output to be
  # expected.
  def initialize(line, output)
    @line = line
    @output = output
  end

  # To String
  #
  # Nice string represntation
  def to_s()
    "Expect<#{output}>"
  end

  attr_reader :output, :line
end

# Method Name
#
# Gets the test method name for a given spec file. This name is based
# on the file name, with invalid characters replaced with underscores.
def methodname(specfile)
  ("test_"+specfile.gsub('/', '_').split('.')[0]).intern
end

# Get Checks
#
# Parses a single line of the input file to extract a check, if one
# exits. This could be a simple output check (#=>) or a more complex
# compile time error check.
def get_checks(file)
  linum = 0
  IO.readlines(file).map do |line|
    linum += 1
    case line
    when /#[ ]?=> (.+)/
      ExpectedOutput.new(linum, $1)
    end
  end.reject { |check| check == nil }
end

# Ullage Specs
#
# This test class searches for each spec file in the `specs/`
# directory and creates a unit test function for them. Each test
# function will compile and run the spec and then examine it's
# output.
class UllageSpec < Test::Unit::TestCase
  Dir.chdir File.expand_path('../', __FILE__)
  Dir.glob("spec/**.ulg").each do |natfile|
    checks = get_checks(natfile)
    define_method methodname(natfile) do
      bin_dir = "specbin"
      name = File.basename(natfile, '.*')
      Dir.mkdir bin_dir unless Dir.exists? bin_dir
      bin = "./#{bin_dir}/#{name}"
      lines = `cargo run -- -o #{bin} #{natfile} && #{bin}`.lines.map{|l|  l.chomp! }.to_a
      assert 0 == $?, "Expected successful exit"
      checks.each do |check|
        line = lines.shift
        assert check.output == line, "#{natfile}:#{check.line}: Expected '#{check.output}' but found '#{line || "nothing"}'"
      end
      assert lines.empty?, "Unexpected output: #{lines}"
    end
  end
end
