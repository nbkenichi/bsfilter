#! /usr/bin/env ruby
## -*-Ruby-*-
## Copyright (C) 2003-2024 NABEYA Kenichi
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

require 'English'
require 'getoptlong'
require 'nkf'

class Bsfilter
  def initialize
    @threads = []
    @token_dbs = nil
    @options = {}
    @db_hash = {}
    @jtokenizer = nil
  end
  attr_accessor :token_dbs

  Revision = 'GIT_HASH'.freeze
  Release = 'GIT_TAG'.freeze
  Languages = %w[C ja].freeze
  Default_Language = 'C'.freeze

  ##  Options = Hash::new           # used like a global variable
  ##  DB = Hash::new

  Default_header_prefix = 'Spam'.freeze
  Default_spam_subject_prefix = '[SPAM] '.freeze
  Default_refer_header =
    %w[Ufrom From To Cc Subject Reply-to Return-path Received
       Content-Transfer-Encoding Content-Type charset Content-Disposition].join(',')

  Default_jtokenizer = 'bigram'.freeze
  Default_mark_in_token = "|!*'".freeze
  Default_homedir = '.bsfilter'.freeze
  Default_conf_file = 'bsfilter.conf'.freeze
  Default_pid_file = 'bsfilter.pid'.freeze

  Default_method = 'rf'.freeze # Robinson Fisher
  Default_db = 'sdbm'.freeze
  Default_max_mail = 10_000
  Default_min_mail = 8000
  Default_max_line = 500

  Default_pop_proxy_if = '0.0.0.0'.freeze
  Default_pop_port = '110'.freeze
  Default_pop_proxy_port = '10110'.freeze
  Default_pop_max_size = 50_000

  Default_imap_port = '143'.freeze
  Default_imap_auth = 'auto'.freeze
  Default_imap_auth_preference = %w[cram-md5 login loginc].freeze

  Default_icon_number = 32_512

  Clean_ext = '.clean'.freeze
  Spam_ext = '.spam'.freeze
  Prob_ext = '.prob'.freeze
  Lock_ext = '.lock'.freeze

  NDBM_ext = '.ndbm'.freeze
  SDBM_ext = '.sdbm'.freeze
  GDBM_ext = '.gdbm'.freeze
  BDB1_ext = '.bdb1'.freeze
  BDB_ext = '.bdb'.freeze
  QDBM_ext = '.qdbm'.freeze

  EXIT_NORMAL = 0
  CODE_NORMAL = true
  CODE_SPAM = true
  CODE_CLEAN = false

  LOG_CODESET = 'UTF-8'.freeze	# codeset for verbose and debug message. nil => no conversion

  ALL_TAGS = %w[html head title meta body div spam
                h1 h2 h3 h4 h5 h6
                em strong font basefont big small
                b i s u tt sub sub
                rb rp rt ruby
                blink marquee
                dfn cite abbr acronym
                blockquote q
                br pre ins del center style hr
                ul ol li dl dt dd
                table caption thead tbody tfoot
                colgroup col tr td th
                a link base img address
                form input select option textarea label
                fieldset legend optgroup
                frameset frame nofrmaes iframe].join('|')

  SPACE_TAGS = 'br|p|td|tr|table|ul|ol|dl|li|dt|dd'.freeze

  RE_ALL_TAGS = Regexp.compile('\A<(' + ALL_TAGS + ')\b', Regexp::IGNORECASE)
  RE_SPACE_TAGS = Regexp.compile('\A<(' + SPACE_TAGS + ')\b', Regexp::IGNORECASE)

  SOCKET_TIMEOUT = 30 # for single socket operation

  module Bsutil
    def insert_header!(buf, header, content)
      buf[0] =~ /([\r\n]*)\z/
      eol = ::Regexp.last_match(1)

      (0...buf.length).each do |i|
        if (i.zero? && # unix from line
            (buf[i] =~ /\A>?from\s+(\S+)/))
          next
        elsif (buf[i] =~ /\A(.*?:)/)
          h = ::Regexp.last_match(1)
          if (h == header)
            buf[i] = "#{header} #{content}#{eol}"
            return
          end
        elsif (buf[i] =~ /\A\s+\S/) # folded header
          next
        elsif (buf[i] =~ /\A[\r\n]*\z/) # separator between header and body
          buf[i, 0] = "#{header} #{content}#{eol}"
          return
        else # not header. may be body without separator
          buf[i, 0] = "#{header} #{content}#{eol}"
          return
        end
      end
      buf.push("#{header} #{content}#{eol}")
    end

    def append_header!(buf, header, prefix)
      buf[0] =~ /([\r\n]*)\z/
      eol = ::Regexp.last_match(1)
      append_done = false
      (0...buf.length).each do |i|
        if (buf[i] =~ /\A(.*?:)(\s*)(.*?)([\r\n]*)\z/)
          h = ::Regexp.last_match(1)
          org_content = ::Regexp.last_match(3)
          if (h.downcase == header.downcase)
            buf[i] = "#{header} #{prefix}#{org_content}#{eol}"
            append_done = true
          end
        elsif (!append_done &&
               (((buf[i] =~ /\A\S/) && (buf[i] !~ /\A\S+:/)) || # found body without separator
                (buf[i] =~ /\A[\r\n]*\z/))) # separator between header and body
          buf[i, 0] = "#{header} #{prefix}#{eol}"
          append_done = true
          break
        end
      end
      buf.push("#{header} #{prefix}#{eol}") if (! append_done)
    end

    def x_spam_flag
      return format('X-%s-Flag:', @options['header-prefix'])
    end

    def x_spam_probability
      return format('X-%s-Probability:', @options['header-prefix'])
    end

    def x_spam_revision
      return format('X-%s-Revision:', @options['header-prefix'])
    end

    def insert_headers!(buf, spam_flag, probability = nil)
      updated = false
      if (@options['insert-revision'])
        insert_header!(buf, x_spam_revision, "bsfilter release #{Release} revision #{Revision}")
        updated = true
      end
      if (@options['insert-flag'])
        updated = true
        if spam_flag
          insert_header!(buf, x_spam_flag, 'Yes')
        else
          insert_header!(buf, x_spam_flag, 'No')
        end
      end
      if (@options['insert-probability'] && probability)
        updated = true
        insert_header!(buf, x_spam_probability, format('%f', probability))
      end
      if (@options['mark-spam-subject'])
        updated = true
        append_header!(buf, 'Subject:', @options['spam-subject-prefix']) if spam_flag
      end
      return updated
    end
  end

  include Bsutil

  class DevNull
    def sync=(*args); end

    def print(*args); end

    def printf(*args); end
  end

  class DBHash < Hash
    def flatten(magic = '###', head = '', &block)
      each do |k, v|
        if v.instance_of?(DBHash)
          if (head == '')
            v.flatten(magic, k, &block)
          else
            v.flatten(magic, head + magic + k, &block)
          end
        elsif (head == '')
          yield k, v
        else
          yield head + magic + k, v
        end
      end
    end

    def add(hash)
      hash.each do |k, v|
        if (self[k])
          if (self[k].instance_of?(DBHash) &&
              v.instance_of?(DBHash))
            self[k].add(v)
          else
            self[k] += v
          end
        else
          self[k] = v # should do deep copy ?
        end
      end
    end

    def sub(hash)
      hash.each do |k, v|
        if (self[k])
          if (self[k].instance_of?(DBHash) &&
              v.instance_of?(DBHash))
            self[k].sub(v)
            delete(k) if self[k].empty?
          elsif (self[k] > v)
            self[k] -= v
          else
            delete(k)
          end
        end
      end
    end
  end

  def safe_require(file)
    require file
    return true
  rescue LoadError
    return false
  end

  def latin2ascii(str)
    str.force_encoding('ASCII-8BIT')
    newstr = str.tr("\x92\x93\x94".dup.force_encoding('ASCII-8BIT'), "'''")
    newstr.tr!("\xc0-\xc5\xc8-\xcb\xcc-\xcf\xd2-\xd6\xd9-\xdc".dup.force_encoding('ASCII-8BIT'), 'AAAAAAEEEEIIIIOOOOOUUUU')
    newstr.tr!("\xe0-\xe5\xe8-\xeb\xec-\xef\xf2-\xf6\xf9-\xfc".dup.force_encoding('ASCII-8BIT'), 'aaaaaaeeeeiiiiooooouuuu')
    return newstr
  end

  def u2eucjp(str)
    return NKF.nkf('-e -E -X -Z0', str.encode('EUC-JP', 'UTF-8', undef: :replace, invalid: :replace)).validate_encoding
  end

  def u2latin(str)
    return str.encode('US-ASCII', 'UTF-8', undef: :replace, invalid: :replace)
  end

  def gb180302eucjp(str)
    return str.encode('EUC-JP', 'BIG5', undef: :replace, invalid: :replace)
  end

  def open_ro(file)
    if (file == '-')
      fh = $stdin
      yield fh
    elsif file.instance_of?(Array)
      file.instance_eval <<EOM, __FILE__, __LINE__ + 1
      @eof = false
      def gets
        @n = 0 if (! @n)
        if (@n >= self.length)
          nil
        else
          @n = @n + 1
          self[@n - 1]
        end
      end
      def readlines
        @eof = true
        self
      end
      def eof?
        (@eof || empty?)
      end
EOM
      yield file
    else
      if (! FileTest.file?(file))
        raise format('%s is not file', file)
      end

      fh = File.open(file, 'rb')
      yield fh
      fh.close
    end
  end

  def open_wo(file, &block)
    if (file == '-')
      fh = $stdout
    else
      fh = open(file, 'wb')
    end
    if (block)
      yield fh
      if (file != '-')
        fh.close
      end
    else
      return fh
    end
  end

  class FLOAT
    def initialize(f = 0, power = 1)
      @mant = 0
      @exp = 0
      set_f(f, power)
    end
    attr_accessor :mant, :exp

    def to_f
      return @mant * Math.exp(@exp)
    end

    def ln
      return Math.log(@mant) + @exp
    end

    def *(a)
      n = FLOAT.new
      if a.instance_of?(FLOAT)
        n.mant = @mant * a.mant
        n.exp = @exp + a.exp
      else
        n.exp = @exp
        n.mant = @mant * a
      end
      return n
    end

    def set_f(a, power = 1)
      if a.positive?
        @mant = 1
        @exp = Math.log(a) * power
      elsif a.negative?
        @mant = -1
        @exp = Math.log(-a) * power
      else
        @mant = 0
        @exp = 0
      end
      self
    end
  end

  module TokenAccess
    def check_size(max_size, min_size)
      if ((@file_count <= max_size) || (max_size <= 0) || (min_size <= 0))
        return false
      end

      old_count = @file_count
      if (@options['verbose'])
        @options['message-fh'].printf("reduce token database %s from %d to %d\n", @filename, old_count, min_size)
      end

      key_cts.each do |(category, token)|
        if (category != '.internal')
          v = value(category, token) || 0
          sub_scalar(category, token, (v * (old_count - min_size).to_f / old_count.to_f).ceil)
          if (@options['debug'] && ! value(category, token))
            @options['message-fh'].printf("deleted %s %s\n", category, token.to_utf8)
          end
        end
      end
      @file_count = min_size
      @dirty = true
      return true
    end

    def value_with_degene(category, token)
      if value(category, token)
        return value(category, token)
      elsif (!@options['degeneration']) # no degeneration
        return nil
      else
        if (v = value(category, token[0..-2])) # cut last char
          return v
        end

        token = token.gsub(Regexp.compile("[#{@options['mark-in-token']}]"), '')
        if (v = value(category, token))
          return v
        end

        token = token.downcase
        if (v = value(category, token))
          return v
        end

        token = token.upcase
        if (v = value(category, token))
          return v
        end

        token = token.capitalize
        if (v = value(category, token))
          return v
        end

        return nil
      end
    end

    def set_scalar(category, token, val)
      @dirty = true
      @file_count += 1
      set(category, token, val)
    end

    def add_scalar(category, token, val)
      @dirty = true
      @file_count += 1
      if (v = value(category, token))
        set(category, token, v + val)
      else
        set(category, token, val)
      end
    end

    def show_new_token(db)
      db.each_ct do |category, token|
        if (!value(category, token) || value(category, token).zero?)
          @options['message-fh'].printf("new %s %s\n", category, token.to_utf8)
        end
      end
    end

    def values
      array = []
      each_ct do |c, t|
        array.push(value(c, t))
      end
      return array
    end

    def key_cts
      array = []
      each_ct do |c, t|
        array.push([c, t])
      end
      return array
    end

    def export(fh)
      each_ct do |category, token|
        fh.printf("%s %s %s %g\n", @language, category, token, value(category, token)) if value(category, token)
      end
    end
  end

  class TokenDB
    include TokenAccess

    def initialize(language = nil)
      @hash = DBHash.new
      @file_count = 0
      @language = language
      @message_id = '-'
      @probability = nil
      @spam_flag = nil
      @dirty = false
      @time = nil
      @filename = '-'
    end
    attr_accessor :hash, :file_count, :probability, :language, :spam_flag, :message_id, :time, :filename

    def size
      @hash.size
    end

    def each_ct
      @hash.each_key do |category|
        @hash[category].each_key do |token|
          yield(category, token)
        end
      end
    end

    def value(category, token)
      if (!@hash[category])
        return nil
      elsif (v = @hash[category][token])
        return v
      else
        return nil
      end
    end

    def set(category, token, v)
      @dirty = true
      @hash[category] = DBHash.new if (! @hash[category])
      @hash[category][token] = v
    end

    def print_keys_to_str(hash, separator, fh = $stdout)
      hash.keys.sort.each do |k|
        v = hash[k]
        v = v.to_i
        fh.print separator
        fh.print(([k] * v).join(separator))
      end
    end

    def clear
      @dirty = true
      @file_count = 0
      @hash = DBHash.new
    end

    def add_db(db)
      @dirty = true
      @file_count += db.file_count
      @language = db.language if (!@language && db.language)
      @hash.add(db.hash)
    end

    def add_hash(hash)
      @dirty = true
      @file_count += 1
      @hash.add(hash)
    end

    def sub_scalar(category, token, val)
      @file_count -= 1 if @file_count.positive?
      @hash.sub({ category => { token => val } })
    end

    def sub_hash(hash)
      @dirty = true
      @file_count -= 1 if @file_count.positive?
      @hash.sub(hash)
    end

    def sub_db(db)
      @dirty = true
      @file_count -= db.file_count
      @file_count = 1 if (@file_count < 1)
      @hash.sub(db.hash)
    end
  end

  class TokenDBM
    include TokenAccess
    MAGIC = '###'.freeze
    def initialize(options, language, _ext)
      @options = options
      @dbm = nil                  # SDBM not Hash
      @dirty = nil                # not used. for TokenAccess
      @lockfh = nil
      @file_count = nil
      @language = language
    end
    attr_accessor :file_count

    def size
      @dbm.size
    end

    def to_db
      token_db = TokenDB.new(@language)
      @dbm.each do |ct, v|
        (category, token) = ct.split(Regexp.new(MAGIC), 2)
        token_db.set(category, token, v)
        token_db.file_count = @file_count
      end
      return token_db
    end

    def clear
      @dbm.clear
      @file_count = 0
      set('.internal', 'file_count', 0)
    end

    def each_ct
      @dbm.each_key do |ct|
        (category, token) = ct.force_encoding('ASCII-8BIT').split(Regexp.new(MAGIC), 2)
        yield(category, token) if (category && token)
      end
    end

    def add_db(token_db)
      add_hash(token_db.hash)
      @file_count += + token_db.file_count
    end

    def add_hash(hash)
      @dirty = true
      hash.flatten(MAGIC) do |k, v|
        if (@dbm[k])
          @dbm[k] = (@dbm[k].to_f + v.to_f).to_s
        else
          @dbm[k] = v.to_s
        end
      end
    end

    def sub_db(token_db)
      sub_hash(token_db.hash)
      if (@file_count > token_db.file_count)
        @file_count -= token_db.file_count
      else
        @file_count = 0
      end
    end

    def sub_hash(hash)
      @dirty = true
      hash.flatten(MAGIC) do |k, v|
        if (@dbm[k])
          if (@dbm[k].to_f > v.to_f)
            @dbm[k] = (@dbm[k].to_f - v.to_f).to_s
          else
            @dbm.delete(k)
          end
        end
      end
    end

    def value(category, token)
      v = @dbm[category + MAGIC + token]
      return v.to_f if v

      return nil
    end

    def set(category, token, v)
      @dirty = true
      begin
        @dbm[category + MAGIC + token] = v.to_s
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect, category + MAGIC + token, v.to_s) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
    end

    def sub_scalar(category, token, v)
      @dirty = true
      if (@file_count.positive?)
        @file_count -= 1
      end
      oldv = value(category, token)
      if (oldv)
        if (oldv > v)
          set(category, token, oldv - v)
        else
          @dbm.delete(category + MAGIC + token)
        end
      end
    end

    def open(mode = 'r')
      @lockfh = File.open(@lockfile, 'w+')
      case mode
      when 'r'
        begin
          @lockfh.flock(File::LOCK_SH)
        rescue Errno::EINVAL ## Win9x doesn't support LOCK_SH
          @lockfh.flock(File::LOCK_EX)
        end
      when 'w', 'wr', 'rw'
        @lockfh.flock(File::LOCK_EX)
      else
        raise "internal error: unknown mode #{mode}"
      end

      @dbm = open_dbm(@filename, 0o600)

      if (v = value('.internal', 'file_count'))
        @file_count = v.to_i
      else
        @file_count = 0
        set('.internal', 'file_count', @file_count)
      end
      if (@options['verbose'])
        @options['message-fh'].printf("open %s %d tokens %d mails by %d.\n", @filename, @dbm.length, @file_count,
                                      Process.pid)
      end
      @dirty = false
    end

    def close
      dirty = @dirty
      set('.internal', 'file_count', @file_count) if dirty
      if (@options['verbose'])
        @options['message-fh'].printf("close %s %d tokens %d mails by %d.\n", @filename, @dbm.length, @file_count,
                                      Process.pid)
      end
      if (@options['debug'] && dirty)
        key_cts.sort.each do |(c, t)|
          @options['message-fh'].printf("%s %s %s %f\n", @filename, c, t.to_utf8, value(c, t))
        end
      end
      @dbm.close

      @lockfh.flock(File::LOCK_UN)
      @lockfh.close
      @dirty = false
    end
  end

  class TokenNDBM < TokenDBM
    def initialize(options, language, ext)
      @filename = options['homedir'] + language + ext + NDBM_ext
      @lockfile = options['homedir'] + language + ext + NDBM_ext + Lock_ext
      super
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename + '.db')
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, mode)
      DBM.open(filename, mode)
    end
  end

  class TokenSDBM < TokenDBM
    def initialize(options, language, ext)
      @filename = options['homedir'] + language + ext + SDBM_ext
      @lockfile = options['homedir'] + language + ext + SDBM_ext + Lock_ext
      super
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename + '.dir')
        File.unlink(@filename + '.pag')
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, mode)
      SDBM.open(filename, mode)
    end
  end

  class TokenGDBM < TokenDBM
    def initialize(options, language, ext)
      @options = options
      @filename = @options['homedir'] + language + ext + GDBM_ext
      @lockfile = @options['homedir'] + language + ext + GDBM_ext + Lock_ext
      super
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename)
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, mode)
      GDBM.open(filename, mode, GDBM::NOLOCK)
    end
  end

  class TokenBDB1 < TokenDBM
    def initialize(options, language, ext)
      @filename = options['homedir'] + language + ext + BDB1_ext
      @lockfile = options['homedir'] + language + ext + BDB1_ext + Lock_ext
      super
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename)
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, mode)
      BDB1::Hash.open(filename, BDB1::CREATE | BDB1::WRITE, mode)
    end
  end

  class TokenBDB < TokenDBM
    def initialize(options, language, ext)
      @filename = options['homedir'] + language + ext + BDB_ext
      @lockfile = options['homedir'] + language + ext + BDB_ext + Lock_ext
      super
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename)
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, mode)
      BDB::Hash.open(filename, nil, BDB::CREATE, mode)
    end
  end

  class TokenQDBM < TokenDBM
    def initialize(options, language, ext)
      @filename = options['homedir'] + language + ext + QDBM_ext
      @lockfile = options['homedir'] + language + ext + QDBM_ext + Lock_ext
      super
    end

    def value(category, token)
      v = @dbm[category + MAGIC + token]
    rescue DepotError_ENOITEM
      return nil
    else
      return v.to_f
    end

    def add_hash(hash)
      @dirty = true
      hash.flatten(MAGIC) do |k, v|
        if (@dbm[k])
          @dbm[k] = (@dbm[k].to_f + v.to_f).to_s
        else
          @dbm[k] = v.to_s
        end
      end
    end

    def clear
      @file_count = 0
      @dbm.close
      begin
        if (@options['verbose'])
          @options['message-fh'].printf("unlink %s by %d.\n", @filename, Process.pid)
        end
        File.unlink(@filename)
      rescue
        @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
        @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
      end
      @dbm = open_dbm(@filename, 0o600)
      if (@options['verbose'])
        @options['message-fh'].printf("reopen %s by %d.\n", @filename, Process.pid)
      end
    end

    def open_dbm(filename, _mode)
      Depot.open(filename, Depot::OWRITER | Depot::OCREAT)
    end
  end

  def get_lang_from_headers(headers)
    reg_char_ja = Regexp.compile('\?(iso-2022-jp|iso-2202-jp|x.sjis|shift.jis|euc.jp)\?', Regexp::IGNORECASE)
    reg_jis = Regexp.compile('\\x1b\\x24[\\x42\\x40]', nil) # escape sequence to jisx0208 new and old

    @options['refer-header'].each_key do |header_name|
      str = headers[header_name]
      if (str)
        case str
        when reg_char_ja
          @options['message-fh'].printf("lang ja header char_ja\n") if (@options['debug'])
          return ['ja', nil]
        when reg_jis
          @options['message-fh'].printf("lang ja header jis\n") if (@options['debug'])
          return %w[ja jis]
        end
      end
    end
    return [nil, nil]
  end

  def get_lang_from_buf(buf, html_flag)
    return get_lang(buf, html_flag)
  end

  def get_lang(buf, html_flag)
    reg_euc = Regexp.compile("[\xa1\xa2-\xa1\xbc\xa4\xa1-\xa4\xf3\xa5\xa1-\xa5\xf6]{4}".dup.force_encoding('EUC-JP'))
    reg_sjis = Regexp.compile("[\x81\x40-\x81\x5b\x82\x9f-\x82\xf1\x83\x40-\x83\x96]{2}".dup.force_encoding('SHIFT_JIS'))
    reg_utf8 = Regexp.compile("[\xe3\x80\x80-\xe3\x80\x82\xe3\x81\x81-\xe3\x82\x93\xe3\x82\xa1-\xe3\x83\xb6]{4}".dup.force_encoding('UTF-8'))
    reg_jis = Regexp.compile('\\x1b\\x24[\\x42\\x40]'.dup.force_encoding('ASCII-8BIT'))
    reg_gb18030_possible = Regexp.compile('[\x80-\x9f]'.dup.force_encoding('ASCII-8BIT'))
    gb18030_possible = false
    buf.each do |str|
      str = decode_character_reference2u(str) if html_flag
      gb18030_possible = true if (str.force_encoding('ASCII-8BIT') =~ reg_gb18030_possible)

      str_utf8 = str.encode('UTF-16BE', 'UTF-8', undef: :replace, invalid: :replace).encode('UTF-8', 'UTF-16BE',
                                                                                            undef: :replace, invalid: :replace)
      str_sjis = str.encode('UTF-16BE', 'SHIFT_JIS', undef: :replace, invalid: :replace).encode('SHIFT_JIS',
                                                                                                'UTF-16BE', undef: :replace, invalid: :replace)
      str_euc = str.encode('UTF-16BE', 'EUC-JP', undef: :replace, invalid: :replace).encode('EUC-JP', 'UTF-16BE',
                                                                                            undef: :replace, invalid: :replace)

      if (str_utf8 =~ reg_utf8)
        @options['message-fh'].printf("lang ja utf8\n") if (@options['debug'])
        return %w[ja utf8]
      elsif (str.force_encoding('ASCII-8BIT') =~ reg_jis)
        @options['message-fh'].printf("lang ja jis\n") if (@options['debug'])
        return %w[ja jis]
      elsif (str_sjis =~ reg_sjis)
        @options['message-fh'].printf("lang ja sjis\n") if (@options['debug'])
        return %w[ja sjis]
      elsif (str_euc =~ reg_euc)
        if gb18030_possible
          @options['message-fh'].printf("lang ja gb18030\n") if (@options['debug'])
          return %w[ja gb18030]
        else
          @options['message-fh'].printf("lang ja euc\n") if (@options['debug'])
          return %w[ja euc]
        end
      end
    end
    return [nil, nil]
  end

  def get_headers(buf, lang)
    headers = DBHash.new
    buf = buf.dup
    header_buf = []
    if ((buf[0] !~ /\A>?from\s+(\S+)/i) && # this isn't mail
        (buf[0] !~ /\A(\S+):/))
      if (@options['max-line'] <= 0)
        return [headers, buf, lang]
      else
        return [headers, buf[0..@options['max-line']], lang]
      end
    end

    num_of_dquote = 0
    ignore_dquote = false

    while (str = buf.shift)
      header_buf.push(str)
      str = str.chomp
      if (str =~ /\A(\S+?):\s*(.*)/)
        current = ::Regexp.last_match(1).downcase
        if (current == 'received')
          headers[current] = ::Regexp.last_match(2).sub(/[\r\n]*\z/, '')
        else
          headers[current] = (headers[current] || '') + ' ' + ::Regexp.last_match(2).sub(/[\r\n]*\z/, '')
        end
      elsif (str =~ /\A>?from\s+(\S+)/i)
        headers['ufrom'] = ::Regexp.last_match(1)
      elsif (str =~ /\A[\r\n]*\z/ && (ignore_dquote || num_of_dquote.even?)) # separator between header and body
        break
      elsif (str =~ /\A\S/ && (ignore_dquote || num_of_dquote.even?)) # found body without separator
        buf.push(str) # rewind
        break
      elsif !current
        break
      elsif (str =~ /\A\s*=\?/)
        headers[current] += str.sub(/[\r\n]*\z/, '').sub(/\A\s*/, '')
      else
        headers[current] += str.sub(/[\r\n]*\z/, '').sub(/\A\s*/, ' ')
      end
      ## start count on from, to and cc line
      ## contiune count while number of dquote is odd
      if ((current =~ /\A(from|to|cc)\z/) || num_of_dquote.odd?)
        num_of_dquote = num_of_dquote + str.scan(/"/).length - str.scan(/\\"/).length
      end

      if (buf.empty? && ! ignore_dquote) # retry?
        ignore_dquote = true
        buf.concat(header_buf)
        header_buf.clear
        headers.clear
      end
    end

    if ((headers['content-type'] =~ /\bboundary=\s*"(.*?)"/i) ||
        (headers['content-type'] =~ /\bboundary=\s*'(.*?)'/i) ||
        (headers['content-type'] =~ /\bboundary=([^\s;]+)/i))
      headers['boundary'] = ::Regexp.last_match(1)
    end
    headers['charset'] = ::Regexp.last_match(2) if (headers['content-type'] =~ /charset=(['"]*)([^\s\1;]+)\1/i)
    headers['content-type'] = ::Regexp.last_match(1) if (headers['content-type'] =~ /\A([^;]+)/)

    if (@options['max-line'] <= 0)
      return [headers, buf, lang]
    else
      return [headers, buf[0..@options['max-line']], lang]
    end
  end

  class Jtokenizer
    def initialize(method)
      case method
      when 'bigram'
        @method = proc { |s| bigram(s) }
      when 'block'
        @method = proc { |s| block(s) }
      when 'mecab'
        @method = proc { |s| mecab(s) }
        meishi_euc = "\xcc\xbe\xbb\xec".dup.force_encoding('ASCII-8BIT')
        meishi_sjis = meishi_euc.encode('SHIFT_JIS', 'EUC-JP').force_encoding('ASCII-8BIT')
        meishi_utf8 = meishi_euc.encode('UTF-8', 'EUC-JP').force_encoding('ASCII-8BIT')
        @m = MeCab::Tagger.new('-Ochasen')
        node = @m.parseToNode('this is a pen')
        if (defined?(MeCab::VERSION)) # defined after 0.90
          hinshi = node.next.feature.force_encoding('ASCII-8BIT').split(/,/)[0]
        else
          hinshi = node.next.getFeature.force_encoding('ASCII-8BIT').split(/,/)[0]
        end
        case hinshi
        when meishi_euc
          @m_dic_enc = Encoding::EUC_JP
        when meishi_sjis
          @m_dic_enc = Encoding::SHIFT_JIS
        when meishi_utf8
          @m_dic_enc = Encoding::UTF_8
        else
          @m_dic_enc = Encoding.default_external
        end
      when 'chasen'
        Chasen.getopt('-F', '%H %m\n', '-j')
        @method = proc { |s| chasen(s) }
      when 'kakasi'
        @method = proc { |s| kakasi(s) }
      else
        raise "internal error: unknown method #{method}"
      end
    end

    def split(str)
      @method.call(str)
    end

    Reg_kanji = Regexp.compile("[\xb0\xa1-\xf4\xa4]+".dup.force_encoding('EUC-JP'))
    Reg_katakana = Regexp.compile("[\xa1\xbc\xa5\xa1-\xa5\xf6]+".dup.force_encoding('EUC-JP'))
    Reg_kanji_katakana = Regexp.compile("[\xb0\xa1-\xf4\xa4\xa1\xbc\xa5\xa1-\xa5\xf6]".dup.force_encoding('EUC-JP'))
    Reg_not_kanji_katakana = Regexp.compile("[^\xb0\xa1-\xf4\xa4\xa1\xbc\xa5\xa1-\xa5\xf6]".dup.force_encoding('EUC-JP'))

    def kakasi(str)
      str = str.gsub(/[\x00-\x7f]/, ' ')
      return [] if (str =~ /\A +\z/)

      array = []
      Kakasi.kakasi('-oeuc -w', str).scan(/\S+/).each do |token|
        token.gsub!(Reg_not_kanji_katakana, '')
        array.push(token) if ((token =~ Reg_kanji) || (token.length > 2))
      end
      return array
    end

    def mecab(str)
      str = str.encode(@m_dic_enc, invalid: :replace, undef: :replace, replace: ' ')
      str = str.gsub(/[\x00-\x7f]/, ' ')
      return [] if (str.empty? || str =~ /\A +\z/)

      array = []
      node = @m.parseToNode(str)
      while (node &&
             (defined?(MeCab::VERSION) || (node.hasNode == 1)))
        if defined?(MeCab::VERSION)
          token = node.surface.encode('EUC-JP', @m_dic_enc)
          hinshi = node.feature.encode('EUC-JP', @m_dic_enc).split(/,/)[0]
        else
          token = node.getSurface.encode('EUC-JP', @m_dic_enc)
          hinshi = node.getFeature.encode('EUC-JP', @m_dic_enc).split(/,/)[0]
        end
        unless token.valid_encoding?
          # Scrub token
          token = token.each_char.map { |c| c.valid_encoding? ? c : '' }.join
        end
        case hinshi
        when 'BOS/EOS'
          # Skip BOS/EOS
        when "\xb5\xad\xb9\xe6".dup.force_encoding('EUC-JP')
          # Skip KIGOU
        when "\xcc\xbe\xbb\xec".dup.force_encoding('EUC-JP')
          # MEISHI
          array.push(token) if ((token =~ Reg_kanji_katakana) || (token.bytesize > 2))
        else
          token.gsub!(Reg_not_kanji_katakana, '')
          array.push(token) if ((token =~ Reg_kanji) || (token.bytesize > 2))
        end
        node = node.next
      end
      return array
    end

    def chasen(str)
      str = str.gsub(/[\x00-\x7f]/, ' ')
      return [] if (str =~ /\A +\z/)

      array = []
      Chasen.sparse(str).split("\n").each do |hinshi_token|
        next unless (hinshi_token =~ /(.*) (.*)/)

        hinshi = ::Regexp.last_match(1)
        token = ::Regexp.last_match(2)
        if (hinshi == "\xcc\xbe\xbb\xec")
          array.push(token) if ((token =~ Reg_kanji_katakana) || (token.length > 2))
        else
          token.gsub!(Reg_not_kanji_katakana, '')
          array.push(token) if ((token =~ Reg_kanji) || (token.length > 2))
        end
      end
      return array
    end

    def block(str)
      tokens = str.scan(Reg_kanji)
      tokens.concat(str.scan(Reg_katakana))
      return tokens
    end

    def bigram(str)
      tokens = []

      str.scan(Reg_kanji).each do |token|
        case token.length
        when 1, 2
          tokens.push(token)
        else
          l = token.length - 1
          (0..l).each do |i|
            tokens.push(token[i, 2])
          end
        end
      end
      tokens.concat(str.scan(Reg_katakana))
      return tokens
    end
  end

  def tokenize_headers(lang, headers)
    (lang,) = get_lang_from_headers(headers) if (! lang)

    head_db = TokenDB.new(lang)
    reg_token = Regexp.compile("\\b\\d[\\d\\.]+\\d\\b|[\\w#{@options['mark-in-token']}]+")

    if (headers['received'])
      str = headers['received']
      str =~ /envelope-from\s+([\w@.-]+)/
      efrom = ::Regexp.last_match(1)
      str =~ /for\s+<([\w@.-]+)>/
      foraddress = ::Regexp.last_match(1)
      str.sub!(/(\bid|;).*/im, '')
      str.sub!(/\(qmail[^)]*\)/, '')
      str += ' ' + efrom if efrom
      str += ' ' + foraddress if foraddress
      headers['received'] = str
    end

    #    if (headers["domainkey-signature"])
    #      headers["domainkey-signature"] = headers["domainkey-signature"].sub(/b=[^:;\s]+/, '')
    #    end

    #    "authentication-results", "domainkey-signature"
    headers.each do |header, content|
      next unless (@options['refer-all-header'] || @options['refer-header'][header])

      if (lang == 'ja')
        if (content =~ /=\?utf-8\?([bq])/i) && (! @options['utf-8'])
          content = ''
        else
          content = NKF.nkf('-e -X -Z0', content.gsub(/\?(iso-2202-jp|shift-jis)\?/i, '?ISO-2022-JP?')).validate_encoding
        end
      else
        content = latin2ascii(content)
      end

      unless content.valid_encoding?
        # Scrub str
        content = content.each_char.map { |c| c.valid_encoding? ? c : '' }.join
      end

      content.scan(reg_token).each do |token|
        head_db.add_scalar(header, token, 1) if (token.length < 20)
        @options['message-fh'].printf("tokenizer %s %s\n", header, token.to_utf8) if (@options['debug'])
      end
      if (lang == 'ja')
        @jtokenizer.split(content.gsub(/\s+/, '')).each do |token|
          token.force_encoding('ASCII-8BIT')
          head_db.add_scalar(header, token, 1)
          @options['message-fh'].printf("tokenizer %s %s\n", header, token.to_utf8) if (@options['debug'])
        end
      end
    end
    return head_db
  end

  def tokenize_buf(buf)
    lang = nil # lang in unknown at first

    separators = []
    delimiters = []
    (headers, buf, lang) = get_headers(buf, lang)
    if headers.empty? # this is not a mail
      (db, buf) = tokenize_body(lang, headers, buf, separators, delimiters)
      db.time = Time.new
      db.language = Default_Language unless db.language
      ##      db.language = Default_Language if (@options["unified-db"])
      return db
    end

    body_db = TokenDB.new(lang)
    body_db.message_id = headers['message-id'] || '-'

    sub_head_db = TokenDB.new(lang)
    main_head_db = tokenize_headers(lang, headers)
    lang = main_head_db.language if main_head_db

    found_html_part = false
    plain_bodies = []
    html_bodies = []

    until buf.empty?
      separators.push('--' + headers['boundary']) if (headers['boundary'])
      delimiters.push('--' + headers['boundary'] + '--') if (headers['boundary'])

      if ((!headers['content-type']) ||
          (headers['content-type'] !~ /rfc822/i))
        (db, buf) = tokenize_body(lang, headers, buf, separators, delimiters)
        lang = db.language
        if (headers['content-type'] =~ /html/i)
          found_html_part = true
          html_bodies.push(db)
        else
          plain_bodies.push(db)
        end
      end
      (headers, buf, lang) = get_headers(buf, lang)
      db = tokenize_headers(lang, headers)
      sub_head_db.add_db(db)
    end

    html_bodies.each do |db|
      body_db.add_db(db)
    end
    unless (@options['ignore-plain-text-part'] && found_html_part) # default
      plain_bodies.each do |db|
        body_db.add_db(db)
      end
    end

    body_db.add_db(main_head_db)
    body_db.add_db(sub_head_db)
    body_db.file_count = 1
    body_db.time = Time.new
    body_db.language = Default_Language unless body_db.language
    ##    body_db.language = Default_Language if (@options["unified-db"])
    return body_db
  end

  def i2eucjp(i)
    u2eucjp([i].pack('U'))
  end

  def i2ascii(i)
    latin2ascii(u2latin([i].pack('U')))
  end

  def i2u(i)
    [i].pack('U')
  end

  def decode_character_reference2u(str)
    reg = Regexp.compile('\&\#(\d{1,5}|x[\da-f]{1,4});'.dup.force_encoding('ASCII-8BIT'), Regexp::IGNORECASE)
    newstr = if (@options['utf-8'])
               str.gsub(reg) do
                 hex_or_dec = ::Regexp.last_match(1)
                 if (hex_or_dec =~ /^x(.*)/i)
                   hex_str = ::Regexp.last_match(1)
                   i2u(hex_str.hex).force_encoding('ASCII-8BIT')
                 else
                   i2u(hex_or_dec.to_i).force_encoding('ASCII-8BIT')
                 end
               end
             else
               str.gsub(reg, '')
             end
    return newstr
  end

  def decode_character_reference(str, lang)
    newstr = if (@options['utf-8'])
               str.gsub(/&\#(\d{1,5}|x[\da-f]{1,4});/i) do
                 hex_or_dec = ::Regexp.last_match(1)
                 if (hex_or_dec =~ /^x(.*)/i)
                   hex_str = ::Regexp.last_match(1)
                   if (lang == 'ja')
                     i2eucjp(hex_str.hex)
                   else
                     i2ascii(hex_str.hex)
                   end
                 elsif (lang == 'ja')
                   i2eucjp(hex_or_dec.to_i)
                 else
                   i2ascii(hex_or_dec.to_i)
                 end
               end
             else
               str.gsub(/&\#(\d{1,5}|x[\da-f]{1,4});/i, '')
             end
    return newstr
  end

  def tokenize_str(str, lang)
    body_hash = DBHash.new(0)
    url_hash = DBHash.new(0)

    reg_token = Regexp.compile("(?:http:|www)[\\w\\-\\.\\/@%:\?=]+|[\\w\\-\\.]+@[\\w\\-\\.]+|\\b\\d[\\d\\.]+\\d\\b|[\\w#{@options['mark-in-token']}]+")
    reg_url = Regexp.compile('(^http:|https:|^www|@)')
    reg_token2 = Regexp.compile('\b\d[\d\.]+\d\b|[\w%]+')
    #     reg_noret = Regexp::compile('[\r\n]*\z')

    unless str.valid_encoding?
      # Scrub str
      str = str.each_char.map { |c| c.valid_encoding? ? c : '' }.join
    end

    str.scan(reg_token).each do |token|
      if (token =~ reg_url)
        token.scan(reg_token2).each do |token2|
          if (token2.length < 20)
            url_hash[token2] += 1
            @options['message-fh'].printf("tokenizer %s %s\n", 'url', token2.to_utf8) if (@options['debug'])
          end
        end
      elsif (token.length < 20)
        body_hash[token] += 1
        @options['message-fh'].printf("tokenizer C %s %s\n", 'body', token.to_utf8) if (@options['debug'])
      end
    end

    if (lang == 'ja')
      str = str.gsub(Regexp.compile("^[ -\\~]*[\|\>]+".dup.force_encoding('EUC-JP')), '')
      str.gsub!(Regexp.compile("^[ \\t\xa1\xa1]+".dup.force_encoding('EUC-JP')), '') # delete white space
      str.gsub!(Regexp.compile('(\\r?\\n){2,}'.dup.force_encoding('EUC-JP')), ' ') # keep multiple newline as space
      str.gsub!(Regexp.compile('[\\r\\n]+'.dup.force_encoding('EUC-JP')), '') # delete newline
      str.split.each do |s|
        @jtokenizer.split(s).each do |token|
          token.force_encoding('ASCII-8BIT')
          body_hash[token] += 1
          @options['message-fh'].printf("tokenizer ja %s %s\n", 'body', token.to_utf8) if (@options['debug'])
        end
      end
    end
    return [body_hash, url_hash]
  end

  def base64_encoded?(buf)
    [buf.dup, buf.reverse].each do |b|
      while (str = b.shift)
        #        if (str =~ /\A[\.\s\r\n]*\z/)
        if (str =~ /\A[.\s]*\z/)
          next
        elsif (str =~ %r{\A[A-z0-9=+/]+\s*\z})
          break
        else
          return false
        end
      end
    end
    return true
  end

  def tokenize_body(lang, headers, body, separators, delimiters)
    reg_return_codes = Regexp.compile('[\r\n]*\z')

    db = TokenDB.new(lang)
    body = body.dup

    buf = []

    delimiter = delimiters.last
    separator = separators.last

    if separators.empty?
      buf = body
      body = []
    else
      while (str = body.shift)
        str_noret = str.sub(reg_return_codes, '')
        case str_noret
        when separator
          break
        when delimiter
          delimiters.pop
          separators.pop
          delimiter = delimiters.last
          separator = separators.last
          break
        else
          buf.push(str)
        end
      end
    end

    if (headers['content-type'] && headers['content-type'] !~ /text/i)
      return [db, body] # skip non-text body
    end

    case headers['content-transfer-encoding']
    when /base64/i
      if base64_encoded?(buf)
        ##        buf.map! {|str| str.unpack("m*").to_s}
        buf = buf.join.gsub(/[\r\n]/, '').unpack('m*')
      end
    when /quoted-printable/i
      buf.map! { |str| str.unpack('M*').join }
    end

    lang_backup = lang
    if (headers['content-type'] =~ /html/i)
      (lang, code) = get_lang_from_buf(buf, true)
    else
      (lang, code) = get_lang_from_buf(buf, false)
    end
    lang ||= lang_backup

    str = buf.join
    str.gsub!(/^begin[^\r\n]+(([\r\n]+M)([^\r\n]+))*/, '') # remove uuencoded lines

    if (lang == 'ja')
      if (code == 'utf8')
        if (@options['utf-8'])
          str = u2eucjp(str)
        else
          lang = Default_Language # can't use iconv / stop ja tokenizer
        end
      elsif (code == 'gb18030')
        if (@options['utf-8'])
          str = gb180302eucjp(str)
        else
          lang = Default_Language
        end
      else
        str = NKF.nkf('-e -X -Z0', str).validate_encoding
      end
    else
      str = latin2ascii(str)
    end

    tags = []
    if (headers['content-type'] =~ /html/i)
      # remove salad at head of part
      encoding = str.encoding
      str.force_encoding('ASCII-8BIT')
      if (str =~ Regexp.compile('\A[^<>]*?(<(\?xml|!doctype|html|body)\b.*)\z',
                                Regexp::MULTILINE | Regexp::IGNORECASE))
        str = ::Regexp.last_match(1)
      end

      # remove salad in head, except style
      if (str =~ /\A(.*?)(<body.*)\z/im)
        before_body_tag = ::Regexp.last_match(1)
        after_body_tag = ::Regexp.last_match(2)
        before_body_tag.gsub!(%r{>[^<>]*<(?!/style)}im, '><')
        str = before_body_tag + after_body_tag
      end

      # remove <p style="font-size:0px..>
      str.gsub!(%r{(<p[^<>]*font-size\s*:\s*[01]\s*(;|px)[^<>]*>)([^<>]*)(</p>)}im, '')
      str.gsub!(%r{(<font[^<>]*font-size\s*:\s*[01]\s*(;|px)[^<>]*>)([^<>]*)(</font>)}im, '')

      # remove <span style="DISPLAY: none..>
      str.gsub!(%r{(<span[^<>]*display\s*:\s*none[^>]*>)([^<>]*)(</span>)}im, '')

      str = ::Regexp.last_match(1) if (@options['ignore-after-last-atag']) && (str =~ %r{\A(.*)</a>}im)

      # remove salad after body or html
      if (str =~ Regexp.compile('\A(.*)</html>[^<>]*?\z', Regexp::MULTILINE | Regexp::IGNORECASE))
        str = ::Regexp.last_match(1)
      end
      if (str =~ Regexp.compile('\A(.*)</body>[^<>]*?\z', Regexp::MULTILINE | Regexp::IGNORECASE))
        str = ::Regexp.last_match(1)
      end
      str.gsub!(Regexp.compile('<[^>]*>', Regexp::MULTILINE)) do |t|
        t = t.gsub(/\n/, '')
        if (t =~ RE_ALL_TAGS) # end tags are thrown away
          t.force_encoding(encoding)
          tags.push(t)
        end

        t.force_encoding('ASCII-8BIT')
        t.force_encoding(encoding)
        if (t =~ RE_SPACE_TAGS)
          ' '
        else
          ''
        end
      end
      str.force_encoding(encoding)
      body_str = decode_character_reference(str, lang) # out of tags
      tag_str = decode_character_reference(tags.join, lang) # in tags
    else # if plain text
      body_str = str
      tag_str = ''
    end
    (body_hash, url_body_hash) = tokenize_str(body_str, lang)
    (tag_hash, url_tag_hash) = tokenize_str(tag_str, lang)

    db.add_hash({ 'body' => body_hash }) if (!body_hash.empty? && @options['use-body'])
    db.add_hash({ 'tag' => tag_hash }) unless tag_hash.empty?
    db.add_hash({ 'url' => url_body_hash }) unless url_body_hash.empty?
    db.add_hash({ 'url' => url_tag_hash }) unless url_tag_hash.empty?
    db.file_count = 1
    db.language = lang
    return [db, body]
  end

  # for each lang
  class Probability
    def initialize(options, lang)
      @options = options
      @filename = @options['homedir'] + lang + Prob_ext
      case (@options['db'])
      when 'ndbm'
        @clean = TokenNDBM.new(@options, lang, Clean_ext)
        @spam = TokenNDBM.new(@options, lang, Spam_ext)
        @prob = TokenNDBM.new(@options, lang, Prob_ext)
      when 'sdbm'
        @clean = TokenSDBM.new(@options, lang, Clean_ext)
        @spam = TokenSDBM.new(@options, lang, Spam_ext)
        @prob = TokenSDBM.new(@options, lang, Prob_ext)
      when 'gdbm'
        @clean = TokenGDBM.new(@options, lang, Clean_ext)
        @spam = TokenGDBM.new(@options, lang, Spam_ext)
        @prob = TokenGDBM.new(@options, lang, Prob_ext)
      when 'bdb1'
        @clean = TokenBDB1.new(@options, lang, Clean_ext)
        @spam = TokenBDB1.new(@options, lang, Spam_ext)
        @prob = TokenBDB1.new(@options, lang, Prob_ext)
      when 'bdb'
        @clean = TokenBDB.new(@options, lang, Clean_ext)
        @spam = TokenBDB.new(@options, lang, Spam_ext)
        @prob = TokenBDB.new(@options, lang, Prob_ext)
      when 'qdbm'
        @clean = TokenQDBM.new(@options, lang, Clean_ext)
        @spam = TokenQDBM.new(@options, lang, Spam_ext)
        @prob = TokenQDBM.new(@options, lang, Prob_ext)
      end

      @language = lang
    end

    attr_accessor :prob, :clean, :spam, :spam_cutoff, :language

    def merge_dbs_of_lang(token_dbs)
      new_db = TokenDB.new
      token_dbs.each do |db|
        new_db.add_db(db) if (@language == db.language)
      end
      return new_db
    end
  end

  class Graham < Probability
    def initialize(options, lang)
      @spam_cutoff = 0.9
      @default_probability = 0.4
      super
    end

    def product(a)
      n = 1
      a.each do |v|
        n *= v if (v != 0)
      end
      return n
    end

    def get_combined_probability(token_db)
      prob_db = TokenDB.new # temporary

      token_db.each_ct do |category, token|
        probability = @prob.value_with_degene(category, token)
        if probability
          prob_db.set_scalar(category, token, probability)
        else
          prob_db.set_scalar(category, token, @default_probability) # 0.4
        end
      end

      probs = prob_db.values.sort { |a, b| (b - 0.5).abs <=> (a - 0.5).abs }[0, 15]

      if (@options['debug'])
        prob_array = []
        prob_db.each_ct do |c, t|
          prob_array.push([[c, t], prob_db.value(c, t)])
        end
        prob_array.sort! { |a, b| (b[1] - 0.5).abs <=> (a[1] - 0.5).abs }
        prob_array = prob_array[0, 15]
        prob_array.sort! { |a, b| b[1] <=> a[1] }
        prob_array.each do |k, v|
          @options['message-fh'].printf("word probability %s %s %f\n", k[0], k[1].to_str, v)
        end
      end

      prod = product(probs)
      token_db.probability = prod / (prod + product(probs.map { |x| 1 - x }))
      token_db.spam_flag = if (token_db.probability > @spam_cutoff)
                             true
                           else
                             false
                           end
      return token_db
    end

    def update_probability(token_dbs)
      c_count = [@clean.file_count, 1].max
      s_count = [@spam.file_count, 1].max

      if token_dbs.empty?
        incremental = false
        target_cts = @clean.key_cts | @spam.key_cts
        @prob.open('w')
        @prob.clear
      else
        incremental = true
        merged_db = merge_dbs_of_lang(token_dbs)
        target_cts = merged_db.key_cts
        return if target_cts.empty?

        @prob.open('rw')
      end
      old_file_count = @prob.file_count
      new_file_count = 0

      cnum = c_count.to_f
      snum = s_count.to_f

      target_cts.each do |(category, token)|
        c_count = @clean.value(category, token) || 0
        s_count = @spam.value(category, token) || 0
        if (incremental && @prob.value(category, token))
          @prob.sub_scalar(category, token, 1.0) # 1.0 is big enough for delete
          new_file_count -= 1
        end
        if c_count.zero?
          if (s_count > 10)
            new_file_count += 1
            @prob.set_scalar(category, token, 0.9999)
          elsif (s_count > 5)
            new_file_count += 1
            @prob.set_scalar(category, token, 0.9998)
          end
        elsif s_count.zero?
          if (c_count > 10)
            new_file_count += 1
            @prob.set_scalar(category, token, 0.0001)
          elsif (c_count > 5)
            new_file_count += 1
            @prob.set_scalar(category, token, 0.0002)
          end
        elsif (c_count + s_count > 5)
          c = c_count * 2
          s = s_count
          p = [[[s / snum, 1.0].min / ([c / cnum, 1.0].min + [s / snum, 1.0].min),
                0.9999].min,
               0.0001].max
          new_file_count += 1
          @prob.set_scalar(category, token, p)
        end
      end
      @prob.file_count = new_file_count + old_file_count if incremental
      @prob.close
    end
  end

  class Robinson < Probability
    def initialize(options, lang)
      @robx_max = 1
      @min_dev = 0.1
      @spam_cutoff = 0.582
      @center = 0.5
      @robs = 0.001 # from bogofilter/robinson.h
      @default_robx = 0.415	# from bogofilter/robinson.h / not used
      super
    end

    def get_pw(_category, _token, _g, _b)
      return pw
    end

    def update_probability(token_dbs)
      pwdb = TokenDB.new
      c_count = [@clean.file_count, 1].max
      s_count = [@spam.file_count, 1].max

      if token_dbs.empty?
        incremental = false
        target_cts = @clean.key_cts | @spam.key_cts
      else
        incremental = true
        merged_db = merge_dbs_of_lang(token_dbs)
        target_cts = merged_db.key_cts
        return if target_cts.empty?
      end

      ## loop1
      ## get pw and robx(average of pw)
      count = 0
      pw_sum = 0.0

      good_mail = [1, @clean.file_count].max.to_f
      bad_mail = [1, @spam.file_count].max.to_f
      target_cts.each do |(category, token)|
        g = [@clean.value(category, token) || 0, c_count].min
        b = [@spam.value(category, token) || 0, s_count].min
        n = g + b
        if n.zero?
          pwdb.set_scalar(category, token, nil) # need to delete this token from prob.db
        else
          pw = (b / bad_mail) / (b / bad_mail + g / good_mail)
          if (@robx_max.zero? || (n <= @robx_max))
            pw_sum += pw
            count += 1
          end
          pwdb.set_scalar(category, token, pw)
        end
      end

      if incremental
        @prob.open('rw')
        old_file_count = @prob.file_count
        old_robx = @prob.value('.internal', 'robx') || @default_robx
        robx = (pw_sum + old_file_count * old_robx) / (count + old_file_count)
      else
        @prob.open('w')
        @prob.clear
        robx = if (count != 0)
                 pw_sum / count
               else
                 @default_robx
               end
      end
      robs = @robs
      ## loop2
      ## get fw from pw
      new_file_count = 0
      pwdb.key_cts.each do |(category, token)|
        g = [@clean.value(category, token) || 0, c_count].min
        b = [@spam.value(category, token) || 0, s_count].min
        n = g + b
        pw = pwdb.value(category, token)
        if (incremental && @prob.value(category, token))
          new_file_count -= 1
          @prob.sub_scalar(category, token, 1.0) # 1.0 is big enough for delete
        end
        if pw
          new_file_count += 1
          @prob.set_scalar(category, token, (robs * robx + n * pw) / (robs + n)) # fw
        end
      end
      @prob.set_scalar('.internal', 'robx', robx)
      @prob.file_count = new_file_count + old_file_count if incremental
      @prob.close
    end

    def get_probability(pminus, qminus, count)
      r = 1.0 / [1, count].max
      p = 1.0 - Math.exp(pminus.ln * r)
      q = 1.0 - Math.exp(qminus.ln * r)
      s = (1.0 + (p - q) / (p + q)) / 2.0
      return s
    end

    def get_combined_probability(token_db)
      robx = @prob.value('.internal', 'robx') || @default_robx

      count = 0
      pminus = FLOAT.new(1)
      qminus = FLOAT.new(1)
      token_db.each_ct do |category, token|
        probability = @prob.value_with_degene(category, token) || robx
        next unless ((probability - @center).abs > @min_dev)

        if (probability <= 0.0)
          probability = 0.0000001
        elsif (probability >= 1.0)
          probability = 0.9999999
        end
        c = token_db.value(category, token)
        count += c
        pminus *= FLOAT.new(1.0 - probability, c)
        qminus *= FLOAT.new(probability, c)
        if (@options['debug'])
          @options['message-fh'].printf("word probability %s %s %d %f\n", category, token.to_utf8, c,
                                        probability)
        end
      end

      token_db.probability = if count.zero?
                               0.0
                             else
                               get_probability(pminus, qminus, count)
                             end
      token_db.spam_flag = if (token_db.probability > @spam_cutoff)
                             true
                           else
                             false
                           end
      return token_db
    end
  end

  class RobinsonFisher < Robinson
    def initialize(options, lang)
      super
      @spam_cutoff = 0.95
    end

    def chi2q(x2, v)
      m = x2 / 2.0
      sum = Math.exp(0.0 - m)
      term = FLOAT.new
      term.exp = 0.0 - m
      term.mant = 1

      (1..(v / 2) - 1).each do |i|
        term *= FLOAT.new(m / i)
        sum += term.to_f
      end
      return sum < 1.0 ? sum : 1.0
    end

    def get_probability(pminus, qminus, count)
      p = 1 - chi2q(-2.0 * pminus.ln, 2 * count)
      q = 1 - chi2q(-2.0 * qminus.ln, 2 * count)
      s = (1.0 + p - q) / 2.0
      return s
    end
  end

  def init_dir(dir)
    return if FileTest.directory?(dir)

    Dir.mkdir(dir, 0o700)
  end

  def soft_raise(str = nil)
    warn str if str
    warn "Try `#{File.basename($PROGRAM_NAME)} --help' for more information."
    exit 2
  end

  def usage
    print <<~EOM

      NAME
      	#{File.basename($PROGRAM_NAME)} - bayesian spam filter

      SYNOPSIS
      	#{File.basename($PROGRAM_NAME)} [options] [commands] < MAIL
      	#{File.basename($PROGRAM_NAME)} [options] [commands] MAIL ...


      DESCRIPTION
      	filter spam.
      	If commands are specified, bsfilter is in maintenance mode, otherwise it is in filtering mode.
      	If bsfilter does not find spam in filtering mode, exit status is 1.
      	If bsfilter runs with --pipe option or finds spam, exit status is 0.

      COMMANDS
      	--add-clean|-c
      		add mails into the clean token database

      	--add-spam|-s
      		add mails into the spam token database

      	--sub-clean|-C
      		subtract mails from the clean token database

      	--sub-spam|-S
      		subtract mails from the spam token database

      	--update|-u
      		update the probability table from clean and spam token databases

      	--export-clean
      		export the clean token database

      	--export-spam
      		export the spam token database

      	--import-clean
      		import the clean token database

      	--import-spam
      		import the spam token database

      	--export-probability
      		export the probability database (for debugging purpose)
      OPTIONS
              --homedir directory
      		specify the name of the bsfilter\'s home directory
      		If this option is not used, a directory specified with the environment variable "BSFILTERHOME" is used
      		If the variable "BSFILTERHOME" is not defined, ".bsfilter" directory under your home is used
      		If the variable "HOME" is not defined, a directory which bsfilter is located at is used

      	--config-file file
      		specify the name of the bsfilter\'s configuration file
      		"bsfilter.conf" in bsfilter\'s home directory is used by default

              --max-line number
      		check and/or study the first number of lines
      		default is #{Default_max_line}. 0 means all

      	--db ndbm|sdbm|gdbm|bdb1|bdb|qdbm
      		specify the name of database type
      		"sdbm" by default

              --jtokenizer|-j bigram|block|mecab|chasen|kakasi
      		specify algorithm of a tokenizer for Japanese language
      		"bigram" by default

      	--list-clean
      		print filename of clean mail

      	--list-spam
      		print filename of spam

      	--imap
      		access IMAP server

      	--imap-server hostname
      		specify hostname of IMAP server

      	--imap-port number
      		specify port number of IMAP server. default is #{Default_imap_port}

      	--imap-auth method
      		specify authorization method. default is "auto"
      		"cram-md5"	use "AUTHENTICATE CRAM-MD5" command
      		"login"		use "AUTHENTICATE LOGIN" command
      		"loginc"	use "LOGIN" command
      		"auto"		try #{Default_imap_auth_preference.join(', ')} in this order.#{' '}

      	--imap-user name
      		specify user name of IMAP server

      	--imap-password password
      		specify password of imap-user

      	--imap-folder-clean folder
      		specify destination folder for clean mails. "inbox.clean" for example

      	--imap-folder-spam folder
      		specify destination folder for spams. "inbox.spam" for example

      	--imap-fetch-unseen
      		filter or study mails without SEEN flag

      	--imap-fetch-unflagged
      		filter or study mails without "X-Spam-Flag" header

      	--imap-reset-seen-flag
      		reset SEEN flag when bsfilter moves or modifies mails

      	--pop
      		work as POP proxy

      	--pid-file file
      		specify filename for logging process ID of bsfilter
      		"bsfilter.pid" in bsfilter\'s home directory is used by default
                      this function is valid when "--pop" is specified

      	--tasktray
      		sit in tasktray
      		this is valid with "--pop" on VisualuRuby

      	--pop-server hostname
      		specify hostname of POP server

      	--pop-port number
      		specify port number of POP server. default is #{Default_pop_port}

      	--pop-proxy-if address
      		specify address of interface which bsfilter listens at
      		default is 0.0.0.0 and all interfaces are active

      	--pop-proxy-port number
      		specify port number which bsfilter listens at. default is #{Default_pop_proxy_port}

      	--pop-user name
      		optional. specify username of POP server.
      		bsfilter checks match between value of this options and a name which MUA sends.
      		in case of mismatch, bsfilter closes sockets.

      	--pop-proxy-set set[,set...]
      		specify rules of pop proxy.
      		alternative way of pop-server, pop-port, pop-proxy-port and pop-user option.
      		format of "set" is "pop-server:pop-port:[proxy-interface]:proxy-port[:pop-user]"
      		If proxy-interface is specified and isn\'t 0.0.0.0 , other interfaces are not used.
      		"--pop-proxy-set 192.168.1.1:110::10110" is equivalent with
      		"--pop-server 192.168.1.1 --pop-port 110 --pop-proxy-port 10110"

      	--pop-max-size number
      		When mail is longer than the specified number, the mail is not filtered.
      		When 0 is specified, all mails are tested and filtered.
      		unit is byte. default is #{Default_pop_max_size}

      	--ssl
      		use POP over SSL with --pop option
      		use IMAP over SSL with --imap option

      	--ssl-cert filename|dirname
      		specify a filename of a certificate of a trusted CA or
      		a name of a directory of certificates

      	--method|-m g|r|rf
      		specify filtering method. "rf" by default
      		"g" means Paul Graham method,
      		"r" means Gary Robinson method,
      		and "rf" means Robinson-Fisher method

      	--spam-cutoff number
      		specify spam-cutoff value
      		0.9 by default for Paul Graham method
      		0.582 by default for Gary Robinson method
      		0.95 by default for Robinson-Fisher method

      	--auto-update|-a
      		recognize mails, add them into clean or spam token database
      		and update the probability table

        --disable-degeneration|-D
                disable degeneration during probability table lookup

        --disable-utf-8
                disable utf-8 support

      	--refer-header header[,header...]
      		refer specified headers of mails
      		"#{Default_refer_header}"
      		by default

      	--refer-all-header
      		refer all headers of mails

      	--ignore-header|-H
      		ignore headers of mails
      		same as --refer-header ""

      	--ignore-body|-B
      		ignore body of mails, except URL or mail address

      	--ignore-plain-text-part
      		ignore plain text part if html part is included in the mail

      	--ignore-after-last-atag
      		ignore text after last "A" tag

              --mark-in-token "characters"
      		specify characters which are allowable in a token
      		"#{Default_mark_in_token}" by default

      	--show-process
      		show summary of execution

      	--show-new-token
      		show tokens which are newly added into the token database

      	--mbox
      		use "unix from" to divide mbox format file

      	--max-mail number
      		reduce token database when the number of stored mails is larger than this one
      		#{Default_max_mail} by default

      	--min-mail number
      		reduce token database as if this number of mails are stored
      		#{Default_min_mail} by default

      	--pipe
      		write a mail to stdout.
      		this options is invalid when "--imap" or "--pop" is specified

      	--insert-revision
      		insert "X-#{Default_header_prefix}-Revision: bsfilter release..." into a mail

      	--insert-flag
      		insert "X-#{Default_header_prefix}-Flag: Yes" or "X-#{Default_header_prefix}-Flag: No" into a mail

      	--insert-probability
      		insert "X-#{Default_header_prefix}-Probability: number" into a mail

      	--header-prefix string
      		valid with --insert-flag and/or --insert-probability option
      		insert "X-specified_string-..." headers, instead of "#{Default_header_prefix}"

      	--mark-spam-subject
      		insert "#{Default_spam_subject_prefix}" at the beginning of Subject header

      	--spam-subject-prefix string
      		valid with --mark-spam-subject option
      		insert specified string, instead of "#{Default_spam_subject_prefix}"

      	--show-db-status
      		show numbers of tokens and mails in databases and quit

        --help|-h
                help

      	--quiet|-q
      		quiet mode

      	--verbose|-v
      		verbose mode

      	--debug|-d
      		debug mode

      EXAMPLES

      % bsfilter -s ~/Mail/spam/*			## add spam
      % bsfilter -u -c ~/Mail/job/* ~/Mail/private/*	## add clean mails and update probability table
      % bsfilter ~/Mail/inbox/1			## show spam probability

      ## recipe of procmail (1)
      :0 HB
      * ? bsfilter -a
      spam/.

      ## recipe of procmail (2)
      :0 fw
      | bsfilter -a --pipe --insert-flag --insert-probability

      :0
      * ^X-Spam-Flag: Yes
      spam/.

      LICENSE
      	this file is distributed under GPL version2

      RELEASE
      	#{Release}
    EOM
  end

  class Mbox
    def initialize(options, fh)
      @options = options
      @buf = fh.readlines
      return unless ((@buf.length == 1) && (@buf.last =~ /\r\z/)) # Mac style EOL

      @buf = @buf.last.scan(/.*?\r/)
    end

    def read
      return nil if @buf.empty? # EOF

      if (!@options['mbox']) # one file == one mail
        ret_buf = @buf.dup
        @buf.clear
      else
        ##    reg_ufrom = Regexp::compile('^From .*@.* \d{2}:\d{2}:\d{2} ')
        ret_buf = []
        while (str = @buf.shift)
          if (str =~ /^From /)
            if ret_buf.empty? # head of mail
              ret_buf.push(str)
            else                # head of next mail
              @buf.unshift(str) # rewind
              return ret_buf
            end
          else
            ret_buf.push(str)
          end
        end
      end
      return ret_buf
    end
  end

  def update_token_db_one(db, command = @options)
    maintenance_command = ''
    maintenance_command += 'c' if (command['add-clean'])
    maintenance_command += 's' if (command['add-spam'])
    maintenance_command += 'C' if (command['sub-clean'])
    maintenance_command += 'S' if (command['sub-spam'])
    maintenance_command = '-' if (maintenance_command == '')

    show_process(db, maintenance_command) if (@options['show-process'])

    if (command['add-clean'] || command['import-clean'])
      @db_hash[db.language].clean.show_new_token(db) if (@options['show-new-token'])
      @db_hash[db.language].clean.add_db(db)
    end
    if (command['add-spam'] || command['import-spam'])
      @db_hash[db.language].spam.show_new_token(db) if (@options['show-new-token'])
      @db_hash[db.language].spam.add_db(db)
    end
    @db_hash[db.language].clean.sub_db(db) if (command['sub-clean'])
    return unless (command['sub-spam'])

    @db_hash[db.language].spam.sub_db(db)
  end

  def read_exported_text(fh)
    dbs = DBHash.new
    @options['languages'].each do |lang|
      dbs[lang] = TokenDB.new(lang)
      dbs[lang].time = Time.new
    end
    while (str = fh.gets)
      str.chomp!
      next if (str =~ /^\s*#/)

      (lang, category, token, val) = str.split
      val = val.to_f.to_i
      if (category == '.internal')
        dbs[lang].file_count = dbs[lang].file_count + val if (token == 'file_count')
      else
        dbs[lang].add_scalar(category, token, val)
        dbs[lang].file_count = dbs[lang].file_count - 1
      end
    end
    return dbs
  end

  def update_token_dbs(files)
    dbs = []
    @options['languages'].each do |lang|
      @db_hash[lang].clean.open('rw')
      @db_hash[lang].spam.open('rw')
    end

    if (@options['imap'])
      if (@options['ssl'])
        imap = Net::IMAP.new(@options['imap-server'], port: @options['imap-port'], ssl: {cert: @options['ssl-cert']})
      else
        imap = Net::IMAP.new(@options['imap-server'], port: @options['imap-port'])
      end
      imap.auto_authenticate(@options, @options['imap-auth'], @options['imap-user'], @options['imap-password'],
                             @options['imap-auth-preference'])

      files.each do |mailbox|
        target_mailbox = mailbox
        target_mailbox = @options['imap-folder-clean'] if (@options['add-clean'] && @options['imap-folder-clean'])
        target_mailbox = @options['imap-folder-spam'] if (@options['add-spam'] && @options['imap-folder-spam'])
        uids = imap_get_target_uids(imap, mailbox)
        uids.each do |uid|
          imapm = IMAPMessage.new(@options, imap, uid)
          imapm.fetch_rfc822
          db = tokenize_buf(imapm.buf)
          db.filename = uid
          update_token_db_one(db)
          updated = imapm.insert_rfc822_headers!((@options['add-spam'] || @options['sub-clean']), nil)
          if updated
            imapm.append(target_mailbox)
            imapm.set_delete_flag
          elsif (target_mailbox != mailbox)
            imapm.copy(target_mailbox)
            imapm.set_delete_flag
          end
        end
        imap.close
      end
      imap.logout
    else
      files.each do |file|
        open_ro(file) do |fh|
          if (@options['import-clean'] || @options['import-spam'])
            imported_dbs = read_exported_text(fh)
            imported_dbs.each do |_lang, db|
              update_token_db_one(db)
            end
          else
            mbox = Mbox.new(@options, fh)
            while (buf = mbox.read)
              db = tokenize_buf(buf)
              db.filename = file
              dbs.push(db)
              if (@options['pipe'])
                insert_headers!(buf, (@options['add-spam'] || @options['sub-clean']), nil)
                @options['pipe-fh'].print buf.join
              end
              update_token_db_one(db)
            end
          end
        end
      end
    end

    slimed = false
    @options['languages'].each do |lang|
      slimed |= @db_hash[lang].clean.check_size(@options['max-mail'], @options['min-mail'])
      slimed |= @db_hash[lang].spam.check_size(@options['max-mail'], @options['min-mail'])
      @db_hash[lang].clean.close
      @db_hash[lang].spam.close
    end
    dbs.clear if slimed # disable incremental
    return dbs
  end

  def auto_update(token_dbs)
    command = {}
    updated_langs = []
    token_dbs.each do |token_db|
      updated_langs.push(token_db.language)
    end
    updated_langs.uniq.each do |lang|
      @db_hash[lang].clean.open('rw')
      @db_hash[lang].spam.open('rw')
    end

    command['sub-clean'] = false
    command['sub-spam'] = false
    command['import-clean'] = false
    command['import-spam'] = false

    token_dbs.each do |token_db|
      if token_db.spam_flag
        command['add-clean'] = false
        command['add-spam'] = true
      else
        command['add-clean'] = true
        command['add-spam'] = false
      end
      update_token_db_one(token_db, command)
    end

    slimed = false
    updated_langs.uniq.each do |lang|
      slimed |= @db_hash[lang].clean.check_size(@options['max-mail'], @options['min-mail'])
      slimed |= @db_hash[lang].spam.check_size(@options['max-mail'], @options['min-mail'])
    end
    token_dbs.clear if slimed # can't use incremental mode

    updated_langs.uniq.each do |lang|
      @db_hash[lang].update_probability(token_dbs)
      @db_hash[lang].clean.close
      @db_hash[lang].spam.close
    end
  end

  def read_config_file(file)
    configs = []

    open(file) do |fh|
      while (str = fh.gets)
        next if ((str =~ /\A\s*#/) || (str =~ /\A\s*\z/))

        str.chomp!
        str.sub!(/\s+\z/, '')
        str.sub!(/\A\s+/, '')
        tokens = str.split(/\s+/, 2)
        unless tokens.empty?
          tokens[0] = '--' + tokens[0]
          configs.concat(tokens)
        end
      end
    end
    return configs
  end

  def imap_get_target_uids(imap, mailbox)
    if (mailbox =~ %r{(.*)/(.*)})
      mailbox = ::Regexp.last_match(1)
      seqs = ::Regexp.last_match(2)
    else
      seqs = nil
    end
    imap.select(mailbox)
    uids = if (@options['imap-fetch-unseen'])
             if seqs
               imap.uid_search(['UNSEEN', seqs])
             else
               imap.uid_search(['UNSEEN'])
             end
           elsif seqs
             imap.uid_search([seqs])
           else
             imap.uid_search(['ALL'])
           end
    if (@options['imap-fetch-unflagged'])
      yes = imap.uid_search(['HEADER', x_spam_flag.sub(/:$/, ''), 'Yes'])
      no = imap.uid_search(['HEADER', x_spam_flag.sub(/:$/, ''), 'No'])
      if (@options['verbose'])
        @options['message-fh'].printf("imap-fetch-unflagged working original %d Yes %d No %d\n",
                                      uids.length, yes.length, no.length)
      end
      ##      uids = uids - imap.uid_search(["HEADER", x_spam_flag.sub(/:$/, ''), ""])
      ## Sendmail Advanced Message Server returns all mails when search string is zero-length ???
      uids = uids - yes - no
      if (@options['verbose'])
        @options['message-fh'].printf("imap-fetch-unflagged worked %d\n",
                                      uids.length)
      end
    end
    return uids
  end

  class IMAPMessage
    include Bsutil
    def initialize(options, imap, uid = nil)
      @options = options
      @seqno = nil
      @seen = nil
      @uid = uid
      @imap = imap
      @buf = []
    end
    attr_accessor :seqno, :uid, :imap, :buf, :seen

    def fetch_rfc822
      #    @options["message-fh"].printf("fetch_rfc822 %d\n", @uid) if (@options["verbose"])
      fetched = @imap.uid_fetch(@uid, %w[RFC822 FLAGS])
      @seqno = fetched[0].seqno
      @buf = fetched[0].attr['RFC822'].split("\n")
      @seen = fetched[0].attr['FLAGS'].include?(:Seen)
      return if @seen

      @imap.uid_store(@uid, '-FLAGS', [:Seen])
    end

    def insert_rfc822_headers!(*args)
      return insert_headers!(@buf, *args)
    end

    def insert_rfc822_header!(header, content)
      #    @options["message-fh"].printf("insert_rfc822_header %d %s %s\n", @uid, header, content) if (@options["verbose"])
      insert_header!(@buf, header, content)
    end

    def append(mailbox)
      @buf.map! do |str|
        str.sub(/[\r\n]*\z/, "\r\n")
      end
      # @options["message-fh"].printf("append %d %s\n", @uid, mailbox) if (@options["verbose"])
      if @seen
        @imap.append(mailbox, @buf.join, [:Seen])
      else
        @imap.append(mailbox, @buf.join, [])
      end
    end

    def copy(mailbox)
      #    @options["message-fh"].printf("copy %d %s\n", @uid, mailbox) if (@options["verbose"])
      @imap.uid_copy(@uid, mailbox)
    end

    def set_delete_flag
      #    @options["message-fh"].printf("set_delete_flag %d\n", @uid) if (@options["verbose"])
      @imap.uid_store(@uid, '+FLAGS', [:Deleted])
    end

    def reset_seen_flag
      #    @options["message-fh"].printf("reset_seen_flag %d\n", @uid) if (@options["verbose"])
      @seen = false
      @imap.uid_store(@uid, '-FLAGS', [:Seen])
    end
  end

  def socket_send_rec(command, socket)
    buf = []
    if command
      if (@options['debug'])
        @options['message-fh'].printf('send %s %s', socket,
                                      command.sub(/\APASS.*/i, 'PASS ********'))
      end
      socket.write_timeout(command) # pass command to pop-server
    end
    response = socket.gets_timeout # get response from pop-server
    buf.push(response)
    if (@options['debug'])
      @options['message-fh'].printf('resp %s %s', socket,
                                    response.sub(/\APASS.*/i, 'PASS ********'))
    end
    if ((response =~ /\A\+OK/) &&
        ((command =~ /\A(RETR|TOP|CAPA)/i) ||
         (command =~ /\A(UIDL|LIST)[^\d]*\z/i)))
      while (response != ".\r\n")
        response = socket.gets_timeout
        buf.push(response)
      end
    end
    return buf
  end

  def pop_proxy_multi(pop_proxy_sets)
    trap('SIGINT') do
      @options['message-fh'].printf("SIGINT received\n") if (@options['verbose'])
      @threads.each do |thread| # kill child threads
        Thread.kill(thread)
      end
    end

    pop_proxy_sets.split(/,/).each do |pop_proxy_set|
      (pop_server, pop_port, pop_proxy_if, pop_proxy_port, pop_user) = pop_proxy_set.split(/:/)
      pop_port = Default_pop_port if (!pop_port || pop_port == '')
      pop_proxy_if = Default_pop_proxy_if if (!pop_proxy_if || pop_proxy_if == '')
      pop_proxy_port = Default_pop_proxy_port if (!pop_proxy_port || pop_proxy_port == '')
      t = Thread.start do        # start child threads
        pop_proxy_one(pop_server, pop_port, pop_proxy_if, pop_proxy_port, pop_user)
      end
      @threads.push(t)
    end
    @threads.each(&:join)

    Thread.list.each do |t|      # join grandchild threads
      t.join if (t != Thread.current)
    end
    return 0
  end

  def pop_bypass_large_mail(command, pop_socket, pop_proxy_socket)
    pop_socket.write_timeout(command) # RETR to server
    str = pop_socket.gets_timeout # response from server
    pop_proxy_socket.write_timeout(str) # forward
    return if (str =~ /^\A-ERR/)

    while (str != ".\r\n")
      require 'timeout'
      Timeout.timeout(SOCKET_TIMEOUT) do
        pop_proxy_socket.write(str = pop_socket.gets) # forward
      end
    end
    return
  end

  def snoop_list_response(strs)
    h = DBHash.new
    if (strs[0] =~ /\A\+OK\s*(\d+)\s+(\d+)/)
      h[::Regexp.last_match(1)] = ::Regexp.last_match(2).to_i
    else
      strs.each do |str|
        h[::Regexp.last_match(1)] = ::Regexp.last_match(2).to_i if (str =~ /^(\d+)\s+(\d+)/)
      end
    end
    return h
  end

  def pop_proxy_one(pop_server, pop_port, pop_proxy_if, pop_proxy_port, pop_user)
    gs = TCPServer.open(pop_proxy_if, pop_proxy_port)
    addr = gs.addr
    addr.shift
    @options['message-fh'].printf("pop_proxy is on %s\n", addr.join(':')) if (@options['verbose'])
    loop do
      Thread.start(gs.accept) do |pop_proxy_socket| # start grandchild threads
        @options['message-fh'].print(pop_proxy_socket, " is accepted\n") if (@options['verbose'])
        begin
          pop_socket = nil
          Timeout.timeout(SOCKET_TIMEOUT) do
            pop_socket = TCPSocket.open(pop_server, pop_port)
          end
          @options['message-fh'].print(pop_socket, " is connected\n") if (@options['verbose'])

          pop_socket = get_ssl_socket(pop_socket, @options['ssl-cert']) if (@options['ssl'])

          hello = socket_send_rec(nil, pop_socket)[0]
          hello.sub!(/(.*)\r/, "\\1(pop_proxy by bsfilter)\r")
          pop_proxy_socket.write(hello)

          sizes = DBHash.new
          while (command = socket_send_rec(nil, pop_proxy_socket)[0]) # get command from MUA
            if (command =~ /\ARETR\s+(\d+)/i)
              n = ::Regexp.last_match(1)
              if (sizes[n] &&
                  (@options['pop-max-size']).positive? && (@options['pop-max-size'] < sizes[n]))
                pop_bypass_large_mail(command, pop_socket, pop_proxy_socket)
                next
              end
            end
            response = socket_send_rec(command, pop_socket)
            if (command =~ /\ALIST/i)
              sizes.update(snoop_list_response(response))
            elsif ((command =~ /\A(TOP|RETR)/i) && (response[0] =~ /\A\+OK/))
              buf = response[1..].dup
              token_db = tokenize_buf(buf)
              @db_hash[token_db.language].prob.open('r')
              @db_hash[token_db.language].get_combined_probability(token_db)
              @db_hash[token_db.language].prob.close
              if (@options['auto-update'])
                auto_update([token_db])
              elsif (@options['show-process'])
                show_process(token_db, '-')
              end
              @options['message-fh'].printf("combined probability %f\n", token_db.probability) if (@options['verbose'])
              insert_headers!(buf, token_db.spam_flag, token_db.probability)
              response[1..-1] = buf
            end
            # don't use elsif
            if (command =~ /QUIT/i)
              @options['message-fh'].printf('send %s %s', pop_proxy_socket, response[0]) if (@options['debug'])
              pop_proxy_socket.write(response.join) # return response to MUA
              break
            elsif ((command =~ /\AUSER\s*(\S*)\r/) &&
                   (pop_user && pop_user != ::Regexp.last_match(1)))
              @options['message-fh'].printf("username unmatch error\n")
              pop_proxy_socket.write("-ERR unregistered user\r\n") # return response to MUA
              break
            else
              @options['message-fh'].printf('send %s %s', pop_proxy_socket, response[0]) if (@options['debug'])
              pop_proxy_socket.write(response.join) # return response to MUA
            end
          end
        rescue Timeout::Error
          if (@options['verbose'])
            @options['message-fh'].printf("Timeout::Error exception %s %s %s\n", pop_server, pop_port,
                                          pop_proxy_port)
          end
        rescue
          if (@options['verbose'])
            @options['message-fh'].printf("pop exception caught %s %s %s\n", pop_server, pop_port,
                                          pop_proxy_port)
          end
          @options['message-fh'].puts($ERROR_INFO.inspect) if (@options['verbose'])
          @options['message-fh'].puts($ERROR_POSITION) if (@options['debug'])
        ensure
          if (pop_proxy_socket && !pop_proxy_socket.closed?)
            @options['message-fh'].print(pop_proxy_socket, " is gone\n") if (@options['verbose'])
            pop_proxy_socket.close
          end
          if (pop_socket && !pop_socket.closed?)
            @options['message-fh'].print(pop_socket, " is gone\n") if (@options['verbose'])
            pop_socket.close
          end
        end
      end
    end
  end

  def check_options_for_pop!(options)
    options['icon_number'] = (options['icon-number'] || Default_icon_number).to_i
    options['pop-port'] = Default_pop_port unless (options['pop-port'])
    options['pop-proxy-if'] = Default_pop_proxy_if unless (options['pop-proxy-if'])
    options['pop-proxy-port'] = Default_pop_proxy_port unless (options['pop-proxy-port'])
    options['pop-max-size'] = (options['pop-max-size'] || Default_pop_max_size).to_i

    if (options['tasktray'])
      require('vr/vrcontrol')
      require('vr/vrtray')
    end

    if (options['pop-proxy-set'] || options['pop-server'])
      ## ok
    else
      soft_raise("#{$PROGRAM_NAME}: pop-server unspecified")
    end

    return
  end

  def check_options_for_imap!(options)
    error = false
    options['imap-port'] = Default_imap_port unless (options['imap-port'])
    %w[imap-server imap-auth imap-user imap-password].each do |name|
      unless (options[name])
        printf("specify %s\n", name)
        error = true
      end
    end

    raise 'error found in imap options' if error
    return
  end

  def do_imap(command_line_args, token_dbs)
    ret_code = CODE_CLEAN
    if (@options['ssl'])
      imap = Net::IMAP.new(@options['imap-server'], port: @options['imap-port'], ssl: {cert: @options['ssl-cert']})
    else
      imap = Net::IMAP.new(@options['imap-server'], port: @options['imap-port'])
    end
    imap.auto_authenticate(@options, @options['imap-auth'], @options['imap-user'], @options['imap-password'],
                           @options['imap-auth-preference'])

    imap.select(@options['imap-folder-clean']) if (@options['imap-folder-clean']) # only for check
    imap.select(@options['imap-folder-spam']) if (@options['imap-folder-spam']) # only for check
    command_line_args.each do |mailbox|
      uids = imap_get_target_uids(imap, mailbox)
      uids.each do |uid|
        imapm = IMAPMessage.new(@options, imap, uid)
        imapm.fetch_rfc822
        token_db = tokenize_buf(imapm.buf)
        token_db.filename = uid
        @db_hash[token_db.language].get_combined_probability(token_db)
        token_dbs.push(token_db)
        if (@options['verbose'])
          @options['message-fh'].printf("combined probability %s %d %f\n", mailbox, imapm.seqno,
                                        token_db.probability)
        end
        target_mailbox = mailbox
        if token_db.spam_flag
          target_mailbox = @options['imap-folder-spam'] if (@options['imap-folder-spam'])
          ret_code = CODE_SPAM
        elsif (@options['imap-folder-clean'])
          target_mailbox = @options['imap-folder-clean']
        end
        updated = imapm.insert_rfc822_headers!(token_db.spam_flag, token_db.probability)
        if updated
          imapm.reset_seen_flag if (@options['imap-reset-seen-flag'])
          imapm.append(target_mailbox)
          imapm.set_delete_flag
        elsif (target_mailbox != mailbox)
          imapm.reset_seen_flag if (@options['imap-reset-seen-flag'])
          imapm.copy(target_mailbox)
          imapm.set_delete_flag
        end
      end
      imap.close
    end
    imap.logout
    return ret_code
  end

  def do_export(command_line_args)
    file = if command_line_args.empty?
             '-'
           else
             command_line_args[0]
           end
    if (@options['export-clean'])
      open_wo(file) do |fh|
        @options['languages'].each do |lang|
          @db_hash[lang].clean.open('r')
          @db_hash[lang].clean.export(fh) if @db_hash[lang].clean.file_count.positive?
          @db_hash[lang].clean.close
        end
      end
    end
    if (@options['export-spam'])
      open_wo(file) do |fh|
        @options['languages'].each do |lang|
          @db_hash[lang].spam.open('r')
          @db_hash[lang].spam.export(fh) if @db_hash[lang].spam.file_count.positive?
          @db_hash[lang].spam.close
        end
      end
    end
    return unless (@options['export-probability'])

    open_wo(file) do |fh|
      @options['languages'].each do |lang|
        @db_hash[lang].prob.open('r')
        @db_hash[lang].prob.export(fh) if @db_hash[lang].prob.file_count.positive?
        @db_hash[lang].prob.close
      end
    end
  end

  def setup_imap
    Net::IMAP.class_eval <<EOM, __FILE__, __LINE__ + 1
      def auto_authenticate(options, auth, user, password, auth_list=[])
        case auth.downcase
        when "loginc"
          if (options["verbose"])
            options["message-fh"].printf("try to login imap server for %s with login command\n", user)
          end
          return login(user, password)
        when "auto"
          capa = capability
          auth_list.each do |auth|
            if (auth == "loginc")
              return auto_authenticate(options, "loginc", user, password)
            elsif (capa.include?("AUTH=" + auth.upcase))
              return auto_authenticate(options, auth, user, password)
            end
          end
          raise sprintf("can't login imap server for %s with %s", user, auth_list)
        else
          if (options["verbose"])
            options["message-fh"].printf("try to login imap server for %s with authenticate %s\n", user, auth)
          end
          return authenticate(auth, user, password)
        end
      end
EOM
  end

  def setup_socket_timeout
    TCPSocket.class_eval <<~EOM, __FILE__, __LINE__ + 1
            def write_timeout(str)
              Timeout.timeout(SOCKET_TIMEOUT) do
                return self.write(str)
              end
            end
            def gets_timeout
              Timeout.timeout(SOCKET_TIMEOUT) do
                s = self.gets
                if (s == nil)
                  raise "socket.gets returned nil"
                else
                  return s.force_encoding('ASCII-8BIT')
                end
              end
            end
    EOM
  end

  def setup_ssl_socket_timeout
    OpenSSL::SSL::SSLSocket.class_eval <<EOM, __FILE__, __LINE__ + 1
      def write_timeout(str)
        Timeout.timeout(SOCKET_TIMEOUT) do
          return self.write(str)
        end
      end
      def gets_timeout
        Timeout.timeout(SOCKET_TIMEOUT) do
          s = self.gets
          if (s == nil)
            raise "ssl_socket.gets returned nil"
          else
            return s
          end
        end
      end
EOM
  end

  def get_ssl_socket(socket, cert = nil)
    context = OpenSSL::SSL::SSLContext.new

    if cert
      if FileTest.file?(cert)
        @options['message-fh'].print(cert, " is used for SSL ca_file\n") if (@options['verbose'])
        context.ca_file = cert
      elsif FileTest.directory?(cert)
        @options['message-fh'].print(cert, " is used for SSL ca_path\n") if (@options['verbose'])
        context.ca_path = cert
      end
      context.verify_mode = OpenSSL::SSL::VERIFY_PEER
    end
    ssl = OpenSSL::SSL::SSLSocket.new(socket, context)
    ssl.connect
    print(ssl, " is connected\n") if (@options['verbose'])
    return ssl
  end

  def setup_tasktray
    eval <<EOM
    class MyForm < VRForm
      include VRTrayiconFeasible
      include VRMenuUseable
      LoadIcon = Win32API.new("user32", "LoadIcon", "II", "I")

      def construct
        @traymenu = newPopupMenu
        @traymenu.set([
                       ["exit", "exit"]
                     ])
        @mytrayicon=0
      end
      def self_trayrbuttonup(iconid)
        showPopup @traymenu
      end
      def into_trayicon(icon_number)
        create_trayicon(LoadIcon.call(0, icon_number),
                        "bsfilter release #{Release} revision #{Revision}", @mytrayicon)
        myexstyle = self.exwinstyle
        myexstyle.ws_ex_toolwindow = true
        myexstyle.ws_ex_appwindow = false
      end

      def exit_clicked
        delete_trayicon(@mytrayicon)
        self.close
      end
    end
EOM
    frm = VRLocalScreen.newform(nil, nil, MyForm)
    frm.create
    frm.into_trayicon(@options['icon_number'])
    VRLocalScreen.messageloop
    @threads.each do |thread| # kill child threads
      Thread.kill(thread)
    end
  end

  def do_pop
    Thread.abort_on_exception = true
    @options['message-fh'].print('pop mode start ', Time.new.to_s, "\n") if (@options['verbose'])

    if (@options['tasktray'])
      Thread.start do
        setup_tasktray
      end
    end

    if (@options['pop-proxy-set'])
      pop_proxy_sets = @options['pop-proxy-set'].gsub(/\s/, '')
    else
      pop_proxy_sets = [@options['pop-server'], @options['pop-port'],
                        @options['pop-proxy-if'], @options['pop-proxy-port'], @options['pop-user']].join(':')
    end
    ret_code = pop_proxy_multi(pop_proxy_sets)

    # never reached
    @options['message-fh'].print('pop mode end ', Time.new.to_s, "\n") if (@options['verbose'])
    return ret_code
  end

  def write_pid_file(file)
    File.open(file, 'w') do |fh|
      fh.print Process.pid, "\n"
    end
  end

  def parse_command_line
    options = DBHash.new

    parser = GetoptLong.new
    parser.ordering = GetoptLong::REQUIRE_ORDER
    parser.set_options(
      ['--icon-number', GetoptLong::REQUIRED_ARGUMENT],
      ['--ssl', GetoptLong::NO_ARGUMENT],
      ['--ssl-cert', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop', GetoptLong::NO_ARGUMENT],
      ['--tasktray', GetoptLong::NO_ARGUMENT],
      ['--pop-proxy-set', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-server', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-port', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-proxy-if', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-proxy-port', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-user', GetoptLong::REQUIRED_ARGUMENT],
      ['--pop-max-size', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap', GetoptLong::NO_ARGUMENT],
      ['--imap-server', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-port', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-auth', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-user', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-password', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-folder-clean', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-folder-spam', GetoptLong::REQUIRED_ARGUMENT],
      ['--imap-fetch-unseen', GetoptLong::NO_ARGUMENT],
      ['--imap-fetch-unflagged', GetoptLong::NO_ARGUMENT],
      ['--imap-reset-seen-flag', GetoptLong::NO_ARGUMENT],
      ['--homedir', GetoptLong::REQUIRED_ARGUMENT],
      ['--config-file', GetoptLong::REQUIRED_ARGUMENT],
      ['--pid-file', GetoptLong::REQUIRED_ARGUMENT],
      ['--db', GetoptLong::REQUIRED_ARGUMENT],
      ##                     ["--unified-db", GetoptLong::NO_ARGUMENT],
      ['--max-line', GetoptLong::REQUIRED_ARGUMENT],
      ['--export-clean', GetoptLong::NO_ARGUMENT],
      ['--export-spam', GetoptLong::NO_ARGUMENT],
      ['--export-probability', GetoptLong::NO_ARGUMENT],
      ['--import-clean', GetoptLong::NO_ARGUMENT],
      ['--import-spam', GetoptLong::NO_ARGUMENT],
      ['--mbox', GetoptLong::NO_ARGUMENT],
      ['--jtokenizer', '-j', GetoptLong::REQUIRED_ARGUMENT],
      ['--method', '-m', GetoptLong::REQUIRED_ARGUMENT],
      ['--spam-cutoff', GetoptLong::REQUIRED_ARGUMENT],
      ['--mark-in-token', GetoptLong::REQUIRED_ARGUMENT],
      ['--max-mail', GetoptLong::REQUIRED_ARGUMENT],
      ['--min-mail', GetoptLong::REQUIRED_ARGUMENT],
      ['--show-new-token', GetoptLong::NO_ARGUMENT],
      ['--auto-update', '-a', GetoptLong::NO_ARGUMENT],
      ['--update', '-u', GetoptLong::NO_ARGUMENT],
      ['--add-clean', '-c', GetoptLong::NO_ARGUMENT],
      ['--add-spam', '-s', GetoptLong::NO_ARGUMENT],
      ['--sub-clean', '-C', GetoptLong::NO_ARGUMENT],
      ['--sub-spam', '-S', GetoptLong::NO_ARGUMENT],
      ['--disable-degeneration', '-D', GetoptLong::NO_ARGUMENT],
      ['--disable-utf-8', GetoptLong::NO_ARGUMENT],
      ['--ignore-body', '-B', GetoptLong::NO_ARGUMENT],
      ['--refer-header', GetoptLong::REQUIRED_ARGUMENT],
      ['--refer-all-header', GetoptLong::NO_ARGUMENT],
      ['--ignore-header', '-H', GetoptLong::NO_ARGUMENT],
      ['--ignore-plain-text-part', GetoptLong::NO_ARGUMENT],
      ['--ignore-after-last-atag', GetoptLong::NO_ARGUMENT],
      ['--pipe', GetoptLong::NO_ARGUMENT],
      ['--insert-revision', GetoptLong::NO_ARGUMENT],
      ['--insert-flag', GetoptLong::NO_ARGUMENT],
      ['--insert-probability', GetoptLong::NO_ARGUMENT],
      ['--header-prefix', GetoptLong::REQUIRED_ARGUMENT],
      ['--mark-spam-subject', GetoptLong::NO_ARGUMENT],
      ['--spam-subject-prefix', GetoptLong::REQUIRED_ARGUMENT],
      ['--list-clean', GetoptLong::NO_ARGUMENT],
      ['--list-spam', GetoptLong::NO_ARGUMENT],
      ['--show-db-status', GetoptLong::NO_ARGUMENT],
      ['--show-process', GetoptLong::NO_ARGUMENT],
      ['--help', '-h', GetoptLong::NO_ARGUMENT],
      ['--revision', GetoptLong::NO_ARGUMENT],
      ['--quiet', '-q', GetoptLong::NO_ARGUMENT],
      ['--debug', '-d', GetoptLong::NO_ARGUMENT],
      ['--verbose', '-v', GetoptLong::NO_ARGUMENT]
    )

    allow_multi = { 'pop-proxy-set' => true }

    parser.quiet = true
    begin
      parser.each_option do |n, arg|
        name = n.sub(/^--/, '')
        if (options[name] && allow_multi[name])
          options[name] += (',' + arg)
        else
          options[name] = arg.dup
        end
      end
    rescue
      soft_raise(format("#{$PROGRAM_NAME}: %s", parser.error_message))
    end
    return options
  end

  def get_options
    argv_backup = Marshal.load(Marshal.dump(ARGV)) # shallow copy is enough?
    options = parse_command_line

    if (options['config-file'] && !File.file?(options['config-file']))
      soft_raise(format("#{$PROGRAM_NAME}: can't open config file `%s'. check argument of --config-file\n",
                        options['config-file']))
    end

    unless (options['homedir'])
      options['homedir'] = if (ENV['BSFILTERHOME'])
                             ENV['BSFILTERHOME']
                           elsif (ENV['HOME'])
                             ENV['HOME'] + '/' + Default_homedir
                           elsif defined?(ExerbRuntime)
                             File.dirname(ExerbRuntime.filepath)
                           else
                             File.dirname($PROGRAM_NAME)
                           end
    end

    options['config-file'] = options['homedir'] + '/' + Default_conf_file unless (options['config-file'])
    if (options['config-file'] && File.file?(options['config-file']))
      ARGV.clear
      argv_config = read_config_file(options['config-file'])
      (argv_config + argv_backup).reverse.each do |argv|
        ARGV.unshift(argv)
      end
      options.update(parse_command_line)
    end

    if (options['help'])
      usage
      exit 0
    end
    if (options['revision'])
      print("bsfilter release #{Release} revision #{Revision}\n")
      exit 0
    end

    options['homedir'] = options['homedir'].sub(%r{/*$}, '') + '/'

    if (options['method'])
      if (options['method'] !~ /\A(g|r|rf)\z/)
        soft_raise(format("#{$PROGRAM_NAME}: unsupported method `%s' for --method or -m\n", options['method']))
      end
    else
      options['method'] = Default_method
    end

    options['header-prefix'] = Default_header_prefix unless (options['header-prefix'])
    options['spam-subject-prefix'] = Default_spam_subject_prefix unless (options['spam-subject-prefix'])

    options['db'] = Default_db unless (options['db'])
    case options['db']
    when 'ndbm'
      require 'dbm'
    when 'sdbm'
      require 'sdbm'
    when 'gdbm'
      require 'gdbm'
    when 'bdb1'
      require 'bdb1'
    when 'bdb'
      require 'bdb'
    when 'qdbm'
      require 'depot'
    else
      soft_raise(format("#{$PROGRAM_NAME}: unsupported argument `%s' for --db\n", options['db']))
    end

    if (options['jtokenizer'])
      options['jtokenizer'].downcase!
    else
      options['jtokenizer'] = Default_jtokenizer
    end
    case options['jtokenizer']
    when 'bigram'
    when 'block'
    when 'mecab'
      require 'MeCab'
    when 'chasen'
      require 'chasen.o'
    when 'kakasi'
      require 'kakasi'
    else
      soft_raise(format("#{$PROGRAM_NAME}: unsupported argument `%s' for --jtokenizer or -j\n", options['jtokenizer']))
    end
    @jtokenizer = Jtokenizer.new(options['jtokenizer'])

    ##    if (options["unified-db"])
    ##      options["languages"] = [Default_Language]
    ##    else
    ##      options["languages"] = Languages
    ##    end

    options['languages'] = Languages

    options['mark-in-token'] = Default_mark_in_token unless (options['mark-in-token'])
    options['mark-in-token'] = options['mark-in-token'].gsub(/\s/, '')
    options['max-line'] = (options['max-line'] || Default_max_line).to_i
    options['max-mail'] = (options['max-mail'] || Default_max_mail).to_i
    options['min-mail'] = (options['min-mail'] || Default_min_mail).to_i

    options['degeneration'] = options['disable-degeneration'] ? false : true

    array = if (options['refer-header'])
              options['refer-header'].downcase.split(',')
            elsif (options['ignore-header'])
              []
            else
              Default_refer_header.downcase.split(',')
            end
    options['refer-header'] = {}
    array.each do |header|
      options['refer-header'][header] = true
    end

    options['use-body'] = options['ignore-body'] ? false : true

    options['pid-file'] = options['homedir'] + Default_pid_file unless (options['pid-file'])

    options['imap-auth'] = options['imap-auth'] || Default_imap_auth
    options['imap-auth-preference'] = Default_imap_auth_preference # can't modify with command line option

    options['utf-8'] = if ((!options['disable-utf-8']))
                         true
                       else
                         false
                       end

    if (options['pop'])
      check_options_for_pop!(options)
      require 'timeout'
      require 'socket'
      setup_socket_timeout
    end
    if (options['imap'])
      check_options_for_imap!(options)
      require 'net/imap'
      setup_imap
    end
    if (options['ssl'])
      if (options['ssl-cert']) && !File.readable?(options['ssl-cert'])
        soft_raise(format("#{$PROGRAM_NAME}: can't read %s. check --ssl-cert option", options['ssl-cert']))
      end
      require 'openssl'
      setup_ssl_socket_timeout
    end
    return options
  end

  def show_db_status
    @options['languages'].each do |lang|
      @db_hash[lang].clean.open('r')
      @db_hash[lang].spam.open('r')
      @db_hash[lang].prob.open('r')
      @options['message-fh'].printf("db %s %d %d %d %d %d\n", lang,
                                    @db_hash[lang].clean.size,
                                    @db_hash[lang].clean.file_count,
                                    @db_hash[lang].spam.size,
                                    @db_hash[lang].spam.file_count,
                                    @db_hash[lang].prob.size)
      @db_hash[lang].prob.close
      @db_hash[lang].spam.close
      @db_hash[lang].clean.close
    end
  end

  def show_process(token_db, maintenance_command)
    if (@options['pop'])
      prot = 'pop'
    elsif (@options['imap'])
      prot = "imap"
    else
      prot = "file"
    end

    case token_db.spam_flag
    when nil
      filter_result = '-'
    when true
      filter_result = 'spam'
    when false
      filter_result = 'clean'
    else
      raise 'internal error: unknown spam_flag'
    end

    @options['message-fh'].printf("%s %s %s %s %s %s %s\n",
                                  prot,
                                  token_db.language,
                                  filter_result,
                                  maintenance_command,
                                  token_db.time.strftime('%Y%m%d%H%M%S'),
                                  token_db.message_id,
                                  token_db.filename)
  end

  def spam?
    @token_dbs.last.spam_flag
  end

  def probability
    @token_dbs.last.probability
  end

  def setup(command_line_options)
    @options.clear
    @db_hash.clear

    command_line_options_backup = command_line_options.dup
    argv_backup = ARGV.dup
    ARGV.clear
    ARGV.unshift(*command_line_options_backup) unless command_line_options_backup.empty?

    @options.update(get_options)

    $stdin.binmode
    if (@options['quiet'])
      @options['message-fh'] = DevNull.new
      @options['pipe-fh'] = DevNull.new
    elsif (((@options['export-clean'] || @options['export-spam'] || @options['export-probability']) &&
         (ARGV.empty? || (ARGV[0] == '-'))) || # export to stdout
        @options['list-clean'] || @options['list-spam'] || @options['pipe'])
      @options['message-fh'] = $stderr
      @options['pipe-fh'] = $stdout
      $stdout.binmode
    else
      @options['message-fh'] = $stdout
      @options['pipe-fh'] = $stdout
      # keep STDOUT in text mode
      @options['message-fh'].sync = true
    end

    @options['mark-in-token'] = Regexp.quote(@options['mark-in-token'])

    init_dir(@options['homedir'])

    @options['languages'].each do |lang|
      case @options['method']
      when 'rf'
        @db_hash[lang] = RobinsonFisher.new(@options, lang)
      when 'r'
        @db_hash[lang] = Robinson.new(@options, lang)
      when 'g'
        @db_hash[lang] = Graham.new(@options, lang)
      else
        raise format('internal error: unknown method %s', @options['method'])
      end
      @db_hash[lang].spam_cutoff = @options['spam-cutoff'].to_f if (@options['spam-cutoff'])
    end

    rest_options = ARGV.dup
    ARGV.clear
    ARGV.unshift(*argv_backup) unless argv_backup.empty?

    return rest_options
  end

  def run(command_line_args)
    @options['message-fh'].print('start ', Time.new.to_s, "\n") if (@options['verbose'])
    if (@options['show-db-status'])
      show_db_status
      return EXIT_NORMAL
    end

    if (@options['pop'])
      write_pid_file(@options['pid-file'])
      do_pop
      File.unlink(@options['pid-file'])
      return EXIT_NORMAL
    end

    filtering_mode = true

    token_dbs = []
    @token_dbs = token_dbs
    if (@options['import-clean'] ||
        @options['import-spam'] ||
        @options['add-clean'] ||
        @options['add-spam'] ||
        @options['sub-clean'] ||
        @options['sub-spam'])
      filtering_mode = false
      if (command_line_args.empty? && ! @options['imap'])
        token_dbs = update_token_dbs(['-'])
      else
        token_dbs = update_token_dbs(command_line_args)
      end
    end

    if (@options['export-clean'] || @options['export-spam'] || @options['export-probability'])
      filtering_mode = false
      do_export(command_line_args)
    end

    if (@options['update'])
      filtering_mode = false
      @options['languages'].each do |lang|
        @db_hash[lang].clean.open('r')
        @db_hash[lang].spam.open('r')
        @db_hash[lang].update_probability(token_dbs) # dbs = Array of TokenDB for -c, -s
        @db_hash[lang].clean.close
        @db_hash[lang].spam.close
      end
    end

    ret_code = CODE_NORMAL
    if filtering_mode
      @options['languages'].each do |lang|
        @db_hash[lang].prob.open('r')
      end
      if (@options['imap'])
        ret_code = do_imap(command_line_args, token_dbs)
      else
        command_line_args = ['-'] if command_line_args.empty?
        ret_code = CODE_CLEAN unless (@options['pipe'])
        command_line_args.each do |file|
          open_ro(file) do |fh|
            number = 1
            mbox = Mbox.new(@options, fh)
            while (buf = mbox.read)
              token_db = tokenize_buf(buf)
              token_db.filename = file
              @db_hash[token_db.language].get_combined_probability(token_db)
              insert_headers!(buf, token_db.spam_flag, token_db.probability)
              @options['pipe-fh'].print buf.join if (@options['pipe'])
              printf("%s\n", file) if (token_db.spam_flag && @options['list-spam'])
              printf("%s\n", file) if (!token_db.spam_flag && @options['list-clean'])
              ret_code = CODE_SPAM if (token_db.spam_flag && (!@options['pipe']))
              token_dbs.push(token_db)
              if defined?(fh.path)
                @options['message-fh'].printf("combined probability %s %d %f\n",
                                              fh.path, number, token_db.probability)
              end
              number += 1
            end
          end
        end
      end
      @options['languages'].each do |lang|
        @db_hash[lang].prob.close
      end
      $stdout.flush
      if (@options['auto-update'])
        auto_update(token_dbs)
      elsif (@options['show-process'])
        token_dbs.each do |token_db|
          show_process(token_db, '-')
        end
      end
    end
    @options['message-fh'].print('end ', Time.new.to_s, "\n") if (@options['verbose'])

    return ret_code
  end
end

class String
  def to_utf8
    if (Bsfilter::LOG_CODESET)
      return dup.encode(Bsfilter::LOG_CODESET, Encoding::EUC_JP, undef: :replace, invalid: :replace)
    else
      self
    end
  end
  def validate_encoding
    self.encode(self.encoding, self.encoding, undef: :replace, invalid: :replace)
  end
end


if ($PROGRAM_NAME == __FILE__)
  bsfilter = Bsfilter.new
  args = bsfilter.setup(ARGV)
  if bsfilter.run(args)
    exit 0
  else
    exit 1
  end
end
