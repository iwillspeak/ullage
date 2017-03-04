#! /usr/bin/env ruby

require 'test/unit'
require 'open3'

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

  # Expects Error
  #
  # This check doesn't expect the compilation or execution to fail.
  def expects_err?
    false
  end

  # To String
  #
  # Nice string represntation
  def to_s()
    "Expect<#{output}>"
  end

  attr_reader :output, :line
end

# Expected Error
#
# Represents a check for a compilation error.
class ExpectedError

  # Initialise the Error
  #
  # Create a new instance of the check, passing in a string which is
  # expected in the error message.
  def initialize(line, err)
    @line = line
    @error = err
  end

  # Expects Erorr
  #
  # This check expects a non-zero exit status
  def expects_err?
    true
  end

  # Nice String Representation
  def to_s()
    "Error<at=#{line},#{error}>"
  end

  attr_reader :error, :line
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
    when /#[ ]?!> (.+)/
      ExpectedError.new(linum, $1)
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

      # Run the test
      stdout, stderr, status = Open3.capture3("cargo run -- -o #{bin} #{natfile} && #{bin}")

      expecting_error = checks.any? {|c| c.expects_err? }

      if expecting_error
        assert status != 0, "Expected failure:#{status}"

        checks.each do |check|
          assert stderr.include?(check.error), "#{natfile}:#{check.line}: Expected '#{check.error}' in stderr:\n#{stderr}"
        end

        return
      end

      # Make sure the whole thing went well
      assert 0 == status, "Expected successful exit:#{status}\n#{stderr}"


      lines = stdout.lines.map{|l|  l.chomp! }.to_a
      checks.each do |check|
        line = lines.shift
        assert check.output == line, "#{natfile}:#{check.line}: Expected '#{check.output}' but found '#{line || "nothing"}'"
      end
      assert lines.empty?, "Unexpected output: #{lines}"
    end
  end
end
