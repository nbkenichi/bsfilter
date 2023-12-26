# -*- coding: euc-jp -*-
# -*-Ruby-*- $Id: test.rb,v 1.13 2013/11/03 08:26:42 nabeken Exp $

load '../bsfilter/bsfilter'
require 'test/unit'
require 'fileutils'

$default_options = ["--homedir", ".", "-v", "-d", "--show-process"]

class DummyFH
  def initialize
    @buf = Array::new
  end
  attr_accessor :buf
  def sync=(*args)
  end
  def print(*arg)
    @buf.push(*arg.flatten.dup)
    @buf.map{|str| str.force_encoding('ASCII-8BIT')}
    @buf = @buf.join.split(/(\r\n|\r|\n)/).each_slice(2).to_a.map{|s| s.join}
  end
  def printf(format, *args)
    @buf.push(sprintf(format, *args))
  end
end

class Bsfilter
  attr_accessor :options
  def use_dummyfh
    options["message-fh"] = DummyFH::new
    options["pipe-fh"] = DummyFH::new
  end

  def grep_message(pattern)
    if RUBY_VERSION < "1.9"
      options["message-fh"].buf.grep(pattern)
    else
      options["message-fh"].buf.map{|str| str.force_encoding('EUC-JP')}.grep(pattern)
    end
  end

  def count_message(pattern)
    grep_message(pattern).length
  end

  def grep_pipe(pattern)
    if RUBY_VERSION < "1.9"
      options["pipe-fh"].buf.grep(pattern)
    else
      options["pipe-fh"].buf.map{|str| str.force_encoding('EUC-JP')}.grep(pattern)
    end
  end

  def count_pipe(pattern)
    grep_pipe(pattern).length
  end
end


def safe_require(file)
  begin
    require file
    return true
  rescue LoadError
    return false
  end
end

def unlink_all
  unlink_prob_sdbm({:force => true})
  unlink_token_sdbm({:force => true})

  unlink_prob_gdbm({:force => true})
  unlink_token_gdbm({:force => true})

  unlink_prob_bdb1({:force => true})
  unlink_token_bdb1({:force => true})

  unlink_prob_bdb({:force => true})
  unlink_token_bdb({:force => true})

  unlink_prob_qdbm({:force => true})
  unlink_token_qdbm({:force => true})
end

def unlink_prob_sdbm(options = {})
    FileUtils.rm(["C.prob.sdbm.dir",
                  "C.prob.sdbm.pag",
                  "C.prob.sdbm.lock",
                  "ja.prob.sdbm.dir",
                  "ja.prob.sdbm.pag",
                  "ja.prob.sdbm.lock"], options)
end
def unlink_token_sdbm(options = {})
    FileUtils.rm(["C.clean.sdbm.dir",
                  "C.clean.sdbm.pag",
                  "C.clean.sdbm.lock",
                  "C.spam.sdbm.dir",
                  "C.spam.sdbm.pag",
                  "C.spam.sdbm.lock",
                  "ja.clean.sdbm.dir",
                  "ja.clean.sdbm.pag",
                  "ja.clean.sdbm.lock",
                  "ja.spam.sdbm.dir",
                  "ja.spam.sdbm.pag",
                  "ja.spam.sdbm.lock"], options)
end


def unlink_prob_gdbm(options = {})
    FileUtils.rm(["C.prob.gdbm",
                  "C.prob.gdbm.lock",
                  "ja.prob.gdbm",
                  "ja.prob.gdbm.lock"], options)
end
def unlink_token_gdbm(options = {})
    FileUtils.rm(["C.clean.gdbm",
                  "C.clean.gdbm.lock",
                  "ja.clean.gdbm",
                  "ja.clean.gdbm.lock",
                  "C.spam.gdbm",
                  "C.spam.gdbm.lock",
                  "ja.spam.gdbm",
                  "ja.spam.gdbm.lock"], options)
end

def unlink_prob_bdb1(options = {})
    FileUtils.rm(["C.prob.bdb1",
                  "C.prob.bdb1.lock",
                  "ja.prob.bdb1",
                  "ja.prob.bdb1.lock"], options)
end


def unlink_token_bdb1(options = {})
    FileUtils.rm(["C.clean.bdb1",
                  "C.clean.bdb1.lock",
                  "ja.clean.bdb1",
                  "ja.clean.bdb1.lock",
                  "C.spam.bdb1",
                  "C.spam.bdb1.lock",
                  "ja.spam.bdb1",
                  "ja.spam.bdb1.lock"], options)
end

def unlink_prob_bdb(options = {})
    FileUtils.rm(["C.prob.bdb",
                  "C.prob.bdb.lock",
                  "ja.prob.bdb",
                  "ja.prob.bdb.lock"], options)
end

def unlink_token_bdb(options = {})
    FileUtils.rm(["C.clean.bdb",
                  "C.clean.bdb.lock",
                  "ja.clean.bdb",
                  "ja.clean.bdb.lock",
                  "C.spam.bdb",
                  "C.spam.bdb.lock",
                  "ja.spam.bdb",
                  "ja.spam.bdb.lock"], options)
end

def unlink_prob_qdbm(options = {})
    FileUtils.rm(["C.prob.qdbm",
                  "C.prob.qdbm.lock",
                  "ja.prob.qdbm",
                  "ja.prob.qdbm.lock"], options)
end

def unlink_token_qdbm(options = {})
    FileUtils.rm(["C.clean.qdbm",
                  "C.clean.qdbm.lock",
                  "ja.clean.qdbm",
                  "ja.clean.qdbm.lock",
                  "C.spam.qdbm",
                  "C.spam.qdbm.lock",
                  "ja.spam.qdbm",
                  "ja.spam.qdbm.lock"], options)
end

class TestMultipleInstances < Test::Unit::TestCase
  def test_by_mbox
    @files = ["testcases/mbox"]

    @bsfilter0 = Bsfilter::new
    @bsfilter0.setup($default_options + ["--mbox"])
    @bsfilter0.use_dummyfh

    @bsfilter1 = Bsfilter::new
    @bsfilter1.setup($default_options)
    @bsfilter1.use_dummyfh

    @bsfilter2 = Bsfilter::new
    @bsfilter2.setup($default_options + ["--mbox"])
    @bsfilter2.use_dummyfh

    @bsfilter3 = Bsfilter::new
    @bsfilter3.setup($default_options)
    @bsfilter3.use_dummyfh

    @bsfilter0.run(@files)
    @bsfilter1.run(@files)
    @bsfilter2.run(@files)
    @bsfilter3.run(@files)

    assert_equal(3, @bsfilter0.count_message(/^file/), "@bsfilter0")
    assert_equal(1, @bsfilter1.count_message(/^file/), "@bsfilter1")
    assert_equal(3, @bsfilter2.count_message(/^file/), "@bsfilter2")
    assert_equal(1, @bsfilter3.count_message(/^file/), "@bsfilter3")
  end

  def test_by_jtokenizer
    return if (! safe_require('MeCab'))
    return if (! safe_require('chasen.o'))

    @files = ["testcases/iso_2022_jp_plain"]

    @bsfilter0 = Bsfilter::new
    @bsfilter0.setup($default_options + ["--jtokenizer", "bigram"])
    @bsfilter0.use_dummyfh

    @bsfilter1 = Bsfilter::new
    @bsfilter1.setup($default_options + ["--jtokenizer", "mecab"])
    @bsfilter1.use_dummyfh

    @bsfilter2 = Bsfilter::new
    @bsfilter2.setup($default_options + ["--jtokenizer", "bigram"])
    @bsfilter2.use_dummyfh

    @bsfilter3 = Bsfilter::new
    @bsfilter3.setup($default_options + ["--jtokenizer", "chasen"])
    @bsfilter3.use_dummyfh

    @bsfilter0.run(@files)
    @bsfilter1.run(@files)
    @bsfilter2.run(@files)
    @bsfilter3.run(@files)

    assert_equal(1, @bsfilter0.count_message(/tokenizer ja body 朝顔/),  "@bsfilter0 2letters")
    assert_equal(0, @bsfilter0.count_message(/tokenizer ja body 向日葵/), "@bsfilter0 3letters")

    assert_equal(1, @bsfilter1.count_message(/tokenizer ja body 朝顔/), "@bsfilter1 2letters")
    assert_equal(1, @bsfilter1.count_message(/tokenizer ja body 向日葵/), "@bsfilter1 3letters")

    assert_equal(1, @bsfilter2.count_message(/tokenizer ja body 朝顔/), "@bsfilter2 2letters")
    assert_equal(0, @bsfilter2.count_message(/tokenizer ja body 向日葵/), "@bsfilter2 3letters")

    assert_equal(1, @bsfilter3.count_message(/tokenizer ja body 朝顔/), "@bsfilter3 2letters")
    assert_equal(1, @bsfilter3.count_message(/tokenizer ja body 向日葵/), "@bsfilter3 3letters")
  end

  def teardown
    unlink_all
  end
end

class TestGetLang < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
  end

  def test_euc
    @files = ["testcases/euc_plain_iso_2022_jp"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja euc/))
  end

  def test_sjis
    @files = ["testcases/sjis_plain_iso_2022_jp"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja sjis/))
  end

  def test_sjis_base64_iso_2022_jp
    @files = ["testcases/sjis_base64_iso_2022_jp"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja sjis/))
  end

  def test_sjis_base64_iso_2202_jp
    @files = ["testcases/sjis_base64_iso_2202_jp"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja sjis/))
  end

  def test_iso_2022_jp_plain
    @files = ["testcases/iso_2022_jp_plain"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja jis/))
  end

  def test_utf8_base64
    @files = ["testcases/utf8_base64"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja utf8/))
  end

  def test_utf8_plain
    @files = ["testcases/utf8_plain"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja utf8/))
  end

  def test_gb18030_base64_gb2312
    @files = ["testcases/gb18030_base64_gb2312"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/lang ja gb18030/))
  end

  def teardown
    unlink_prob_sdbm
  end
end


class TestJtokenizer < Test::Unit::TestCase
  def setup
    @files = ["testcases/iso_2022_jp_plain"]
    @bsfilter = Bsfilter::new
  end

  def test_bigram
    @bsfilter.setup($default_options + ["--jtokenizer", "bigram"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "2 letters")
    assert_equal(0, @bsfilter.count_message(/tokenizer ja body 向日葵/), "3 letters")
  end

  def test_mecab
    assert_nothing_raised('Warning: ignore this test if MeCab is NOT installed') do
      @bsfilter.setup($default_options + ["--jtokenizer", "mecab"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "2 letters")
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 向日葵/), "3 letters")
  end

  def test_chasen
    assert_nothing_raised('Warning: ignore this test if chasen is NOT installed') do
      @bsfilter.setup($default_options + ["--jtokenizer", "chasen"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "2 letters")
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 向日葵/), "3 letters")
  end

  def teardown
    unlink_all
  end
end


class TestBase64 < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
  end

  def test_delimiter_bug
    @files = ["testcases/mime_delimiter_bug"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "japanese")
  end

  def test_base64
    @files = ["testcases/sjis_base64_iso_2022_jp"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "japanese")
  end
end

class TestPlainTextParser < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
  end

  def test_folding
    @files = ["testcases/folding"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer ja body 朝顔/), "japanese")
    assert_equal(0, @bsfilter.count_message(/headtail/), "english")
  end

  def test_iso_8895_1
    @files = ["testcases/iso_8895_1_plain"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/tokenizer subject replIca/), "replIca")
    assert_equal(1, @bsfilter.count_message(/tokenizer.*elegant/), "elegant")
  end

  def teardown
    unlink_prob_sdbm
  end
end

class TestDBM < Test::Unit::TestCase
  def setup
    unlink_all
    @files = ["testcases/iso_2022_jp_plain", "testcases/ascii_plain"]
    @bsfilter = Bsfilter::new
  end

  def test_default_dbm
    @bsfilter.setup($default_options + ["-c"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.sdbm.dir"), "ja.clean.sdbm.dir")
    assert(File::readable?("C.clean.sdbm.dir"), "C.clean.sdbm.dir")

    @bsfilter.setup($default_options + ["-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.sdbm.dir"), "ja.prob.sdbm.dir")
    assert(File::readable?("C.prob.sdbm.dir"), "C.prob.sdbm.dir")

    unlink_token_sdbm
    unlink_prob_sdbm
  end

  def test_sdbm
    @bsfilter.setup($default_options + ["--db", "sdbm", "-c"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.sdbm.dir"), "ja.clean.sdbm.dir")
    assert(File::readable?("C.clean.sdbm.dir"), "C.clean.sdbm.dir")

    @bsfilter.setup($default_options + ["--db", "sdbm", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.sdbm.dir"), "ja.prob.sdbm.dir")
    assert(File::readable?("C.prob.sdbm.dir"), "C.prob.sdbm.dir")

    unlink_token_sdbm
    unlink_prob_sdbm
  end

  def test_gdbm
    assert_nothing_raised('Warning: ignore this test if GDBM is NOT installed') do
      @bsfilter.setup($default_options + ["--db", "gdbm", "-c"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.gdbm"), "ja.clean.gdbm")
    assert(File::readable?("C.clean.gdbm"), "C.clean.gdbm")

    @bsfilter.setup($default_options + ["--db", "gdbm", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.gdbm"), "ja.prob.gdbm")
    assert(File::readable?("C.prob.gdbm"), "C.prob.gdbm")

    unlink_token_gdbm
    unlink_prob_gdbm
  end

  def test_bdb1
    assert_nothing_raised('Warning: ignore this test if BDB1 is NOT installed') do
      @bsfilter.setup($default_options + ["--db", "bdb1", "-c"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.bdb1"), "ja.clean.bdb1")
    assert(File::readable?("C.clean.bdb1"), "C.clean.bdb1")

    @bsfilter.setup($default_options + ["--db", "bdb1", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.bdb1"), "ja.prob.bdb1")
    assert(File::readable?("C.prob.bdb1"), "C.prob.bdb1")

    unlink_token_bdb1
    unlink_prob_bdb1
  end

  def test_bdb
    assert_nothing_raised('Warning: ignore this test if BDB is NOT installed') do
      @bsfilter.setup($default_options + ["--db", "bdb", "-c"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.bdb"), "ja.clean.bdb")
    assert(File::readable?("C.clean.bdb"), "C.clean.bdb")

    @bsfilter.setup($default_options + ["--db", "bdb", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.bdb"), "ja.prob.bdb")
    assert(File::readable?("C.prob.bdb"), "C.prob.bdb")

    unlink_token_bdb
    unlink_prob_bdb
  end

  def test_qdbm
    assert_nothing_raised('Warning: ignore this test if QDBM is NOT installed') do
      @bsfilter.setup($default_options + ["--db", "qdbm", "-c"])
    end
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert(File::readable?("ja.clean.qdbm"), "ja.clean.qdbm")
    assert(File::readable?("C.clean.qdbm"), "C.clean.qdbm")

    @bsfilter.setup($default_options + ["--db", "qdbm", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
    assert(File::readable?("ja.prob.qdbm"), "ja.prob.qdbm")
    assert(File::readable?("C.prob.qdbm"), "C.prob.qdbm")

    unlink_token_qdbm
    unlink_prob_qdbm
  end
end

class TestMbox < Test::Unit::TestCase
  def setup
    @files = ["testcases/mbox"]
    @bsfilter = Bsfilter::new
  end

  def test_with_mbox
    @bsfilter.setup($default_options + ["--mbox"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(3, @bsfilter.count_message(/^file/))
  end

  def test_without_mbox
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/file/))
  end

  def teardown
    unlink_prob_sdbm
  end
end

class TestHeaderParser < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new
  end

  def test_header_parser
    @files = ["testcases/header"]
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(1, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3") # drop 2nd hop
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh") # drop ID
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject") # refer subject
    assert_equal(0, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date") # ignore date
  end

  def test_ignore_header
    @files = ["testcases/header"]
    @bsfilter.setup($default_options + ["--ignore-header"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3")
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh")
    assert_equal(0, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject")
    assert_equal(0, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date")
  end

  def test_refer_header_null
    @files = ["testcases/header"]
    @bsfilter.setup($default_options + ["--refer-header", ""])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3")
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh")
    assert_equal(0, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject")
    assert_equal(0, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date")
  end

  def test_refer_header_subject
    @files = ["testcases/header"]
    @bsfilter.setup($default_options + ["--refer-header", "subject"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3")
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh")
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject")
    assert_equal(0, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date")
  end

  def test_refer_header_date
    @files = ["testcases/header"]
    @bsfilter.setup($default_options + ["--refer-header", "date"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3")
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh")
    assert_equal(0, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject")
    assert_equal(9, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date") # date header has 9 tokens
  end

  def test_refer_header_subject_date
    @files = ["testcases/header"]
    @bsfilter.setup($default_options + ["--refer-header", "subject,date"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host1/), "^tokenizer received host1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host2/), "^tokenizer received host2")
    assert_equal(0, @bsfilter.count_message(/^tokenizer received host3/), "^tokenizer received host3")
    assert_equal(0, @bsfilter.count_message(/abcdefgh/), "abcdefgh")
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject/), "^tokenizer subject")
    assert_equal(9, @bsfilter.count_message(/^tokenizer date/), "^tokenizer date")
  end

  def test_mime_b_iso_2022_jp
    @files = ["testcases/mime_b_iso_2022_jp"]
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject 花見/), "^tokenizer subject")
  end

  def test_mime_b_iso_2202_jp
    @files = ["testcases/mime_b_iso_2202_jp"]
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject 花見/), "^tokenizer subject")
  end

  def test_mime_b_shift_jis
    @files = ["testcases/mime_b_shift_jis"]
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject 花見/), "^tokenizer subject")
  end

  def test_mime_b_shift_jis_bad
    @files = ["testcases/mime_b_shift_jis_bad"]
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^tokenizer subject 花見/), "^tokenizer subject")
  end

  def teardown
    unlink_prob_sdbm
  end
end

class TestInsertHeader < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new    
    @bsfilter.setup($default_options + ["--pipe", "--insert-revision"])
    @bsfilter.use_dummyfh
  end
  
  def test_normal
    @files = ["testcases/ascii_plain"]
    @bsfilter.run(@files)
    assert_equal(16, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_match(/^X-Spam-Revision:/, @bsfilter.options["pipe-fh"].buf[8])
  end

  def test_no_body
    @files = ["testcases/no_body"]
    @bsfilter.run(@files)
    assert_equal(9, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_match(/^X-Spam-Revision:/, @bsfilter.options["pipe-fh"].buf[8])
  end

  def test_no_boundary
    @files = ["testcases/no_boundary"]
    @bsfilter.run(@files)
    assert_equal(15, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_match(/^X-Spam-Revision:/, @bsfilter.options["pipe-fh"].buf[8])
  end

  def teardown
    unlink_prob_sdbm
  end
end

class TestMarkSpamSubject < Test::Unit::TestCase
  def setup
    unlink_all
    @files = ["testcases/multi_subject", "testcases/no_body", "testcases/no_boundary"]
    @bsfilter = Bsfilter::new    
    @bsfilter.setup($default_options + ["-s", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)

    @bsfilter.setup($default_options + ["--pipe", "--mark-spam-subject", "--insert-revision"])
    @bsfilter.use_dummyfh
  end

  def test_multi_subject
    @files = ["testcases/multi_subject"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_pipe(/\ASubject: \[SPAM\] subject1/), "1st subject")
    assert_equal(1, @bsfilter.count_pipe(/\ASubject: \[SPAM\] subject2/), "2nd subject")
  end

  def test_no_body
    @files = ["testcases/no_body"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_pipe(/\ASubject: \[SPAM\]/), "no body, no subject")
  end

  def test_no_boundary
    @files = ["testcases/no_boundary"]
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_pipe(/\ASubject: \[SPAM\]/), "no boundary, no subject")
  end

  def teardown
    unlink_token_sdbm
    unlink_prob_sdbm
  end
end


class TestEOL < Test::Unit::TestCase
  def setup
    unlink_all
    @files = ["testcases/lf", "testcases/crlf", "testcases/cr"]
    @bsfilter = Bsfilter::new    
    @bsfilter.setup($default_options + ["-s", "-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)

    @bsfilter.setup($default_options + ["--pipe", "--mark-spam-subject", "--insert-revision"])
    @bsfilter.use_dummyfh
  end

  def test_lf
    @files = ["testcases/lf"]
    @bsfilter.run(@files)

    assert_equal(11, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def test_cr
    @files = ["testcases/cr"]
    @bsfilter.run(@files)

    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(11, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def test_crlf
    @files = ["testcases/crlf"]
    @bsfilter.run(@files)

    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(11, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def teardown
    unlink_token_sdbm
    unlink_prob_sdbm
  end
end

class TestEOLMBox < Test::Unit::TestCase
  def setup
    @bsfilter = Bsfilter::new    
    @bsfilter.setup($default_options + ["--mbox", "--pipe", "--insert-revision"])
    @bsfilter.use_dummyfh
  end

  def test_lf
    @files = ["testcases/mbox_lf"]
    @bsfilter.run(@files)

    assert_equal(23, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def test_cr
    @files = ["testcases/mbox_cr"]
    @bsfilter.run(@files)

    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(23, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def test_crlf
    @files = ["testcases/mbox_crlf"]
    @bsfilter.run(@files)

    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\n\z/), '\n')
    assert_equal(0, @bsfilter.count_pipe(/\A[^\r\n]*\r\z/), '\r')
    assert_equal(23, @bsfilter.count_pipe(/\A[^\r\n]*\r\n\z/), '\r\n')
  end

  def teardown
    unlink_prob_sdbm
  end
end


class TestHtmlParser < Test::Unit::TestCase
  def setup
    @files = ["testcases/html"]
    @bsfilter = Bsfilter::new
    @bsfilter.setup($default_options + ["-c"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)

    @bsfilter.setup($default_options + ["-u"])
    @bsfilter.use_dummyfh
    @bsfilter.run([])
  end

  def test_default
    @bsfilter.setup($default_options)
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(1, @bsfilter.count_message(/^word.*plain_text_part/), "^word.*plain_text_part")
    assert_equal(1, @bsfilter.count_message(/^tokenizer.*ABCDEF/), "ABCDEF")
    assert_equal(1, @bsfilter.count_message(/^tokenizer.*after_atag/), "after_atag")

    assert_equal(0, @bsfilter.count_message(/^tokenizer.*after_html/), "after_html")
    assert_equal(0, @bsfilter.count_message(/^tokenizer.*after_body/), "after_body")
    assert_equal(0, @bsfilter.count_message(/^tokenizer.*fontsize0/), "fontsize0")
    assert_equal(0, @bsfilter.count_message(/^tokenizer.*fontsize1/), "fontsize1")
    assert_equal(0, @bsfilter.count_message(/^tokenizer.*displaynone/), "displaynone")
    assert_equal(1, @bsfilter.count_message(/^tokenizer url 192\.168\.0\.1/), "192.168.0.1")
    assert_equal(1, @bsfilter.count_message(/^tokenizer url www/), "www")
  end

  def test_ignore_plain_text_part
    @bsfilter.setup($default_options + ["--ignore-plain-text-part"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^word.*plain_text_part/))
  end

  def test_ignore_after_last_atag
    @bsfilter.setup($default_options + ["--ignore-after-last-atag"])
    @bsfilter.use_dummyfh
    @bsfilter.run(@files)
    assert_equal(0, @bsfilter.count_message(/^tokenizer.*after_atag/))
  end

  def teardown
    unlink_all
  end
end

class TestTokenizerOptionCombination < Test::Unit::TestCase
  def setup
    @files = Dir.glob("testcases/[^C]*")
    @option_elements = ["--disable-utf-8", "--ignore-header", "--ignore-body", "--disable-degeneration",
                        "--mark-in-token @", "--ignore-plain-text-part", "--ignore-after-last-atag"]
  end

  def test_all
    i = 0
    imax = 2 ** @option_elements.length

    while (i < imax)
      j = 0
      option_array = Array::new
      while (j < @option_elements.length)
        if ((i >> j) % 2 == 1)
          option_array.concat(@option_elements[j].split)
        end
        j += 1
      end
      bsfilter = Bsfilter::new
      bsfilter.setup($default_options + option_array + ["-q"])
      bsfilter.run(@files)
      i += 1
    end
  end

  def teardown
    unlink_prob_sdbm
  end
end
