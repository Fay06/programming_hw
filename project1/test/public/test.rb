require "minitest/autorun"
require_relative "../../src/wordnet.rb"

$VALID_SYNSETS = "inputs/public_synsets_valid"
$INVALID_SYNSETS = "inputs/public_synsets_invalid"
$VALID_HYPERNYMS = "inputs/public_hypernyms_valid"
$VALID_HYPERNYMS_2 = "inputs/hypernyms_valid_2"
$INVALID_HYPERNYMS = "inputs/public_hypernyms_invalid"
$VALID_SYNSETS_2 = "inputs/public_synsets_valid_2"
$INVALID_SYNSETS_3 = "inputs/public_synsets_invalid_3"

class PublicTests < MiniTest::Test

    def test_release_commandline_multiload
        parser = CommandParser.new
        #hypernum file invalid
        assert_equal({:recognized_command => :load, :result => false},
                     parser.parse("load #{$VALID_SYNSETS} #{$INVALID_HYPERNYMS}"))
        #synset file invalid
        assert_equal({:recognized_command => :load, :result => false},
                     parser.parse("load #{$INVALID_SYNSETS} #{$VALID_HYPERNYMS}"))
        #both file invalid
        assert_equal({:recognized_command => :load, :result => false},
                     parser.parse("load #{$INVALID_SYNSETS} #{$INVALID_HYPERNYMS}"))
        # synset id in hypernum not in current object or synset file
        assert_equal({:recognized_command => :load, :result => false},
                     parser.parse("load #{$VALID_SYNSETS_2} #{$VALID_HYPERNYMS}"))
        #sucess load
        assert_equal({:recognized_command => :load, :result => true},
                     parser.parse("load #{$VALID_SYNSETS} #{$VALID_HYPERNYMS}"))
        #load same file again
        assert_equal({:recognized_command => :load, :result => false},
                     parser.parse("load #{$VALID_SYNSETS} #{$VALID_HYPERNYMS}"))
        #check not exist id in hypernum destination
        assert_equal({:recognized_command => :load, :result => false},
                parser.parse("load #{$VALID_SYNSETS_2} #{$VALID_HYPERNYMS_2}"))
        assert_equal({:recognized_command => :load, :result => :error},
                     parser.parse("load invalid argument invalid file name"))
    end

=begin
    def test_release_commandline_errors
        parser = CommandParser.new
        assert_equal({:recognized_command=>:load, :result=>:error},
                    parser.parse("load #{$VALID_SYNSETS} "))
        assert_equal({:recognized_command=>:load, :result=>:error},
                    parser.parse("load #{$VALID_SYNSETS} #{$VALID_HYPERNYMS} abc"))
    end
=end
end
