require 'rake/clean'

ERL = "erl -boot start_clean -noshell"
INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include("ebin/*.beam")

directory 'ebin'

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

task :compile => ['ebin'] + OBJ
task :default => :compile

task :test => [:compile] do
  OBJ.each do |obj|
    obj[%r{.*/(.*).beam}]
    mod = $1
    puts "#{mod}..."
    test_output = `#{ERL} -pa ebin -run #{mod} test -run init stop`
    puts test_output
  end
end
