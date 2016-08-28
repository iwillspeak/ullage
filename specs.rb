#! /usr/bin/env ruby

require 'test/unit'

def methodname(specfile)
  ("test_"+specfile.gsub('/', '_').split('.')[0]).intern
end

class UllageSpec < Test::Unit::TestCase
  Dir.chdir File.expand_path('../', __FILE__)
  Dir.glob("spec/**.ulg").each do |natfile|
    define_method methodname(natfile) do
      output = `cargo run < #{natfile}`
      assert 0 == $?, "Expected successful exit"
    end
  end
end
