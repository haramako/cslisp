task :format do
  sh 'astyle', '--options=.astyle', *Dir.glob('**/*.cs').reject{|f| f=~ %r{/obj/} }
end

task :test do
  sh 'dotnet', 'Main/bin/Debug/netcoreapp2.1/Main.dll', 'test.scm'
end

namespace :githook do
  task :setup do
    IO.write('.git/hooks/pre-commit', "#!/bin/sh\nrake githook:precommit")
  end
  
  task :precommit do
    puts 'precommit hook by rake githook:precommit'
    against = `git rev-parse --verify HEAD`
    diff_str = `git diff-index --cached --name-only #{against}`
    # '"' で始まる文字列は、日本語を含むので無視する
    diff_str.force_encoding(Encoding::ASCII_8BIT)
    diff_str = diff_str.split(/\n/).reject { |s| s.include? '"' }.join("\n")
    diff_str.force_encoding(Encoding::UTF_8)
    files = diff_str.encode(Encoding::UTF_8).split(/\n/)

    # .csファイルのインデントを修正する
    cs_files = files.select { |f| f =~ /\.cs$/ }
    unless cs_files.empty?
      cs_files = cs_files.select { |f| File.exist?(f) }
      puts "reindent #{cs_files.join(' ')} by astyle"
      sh 'astyle', '-Q', '--options=.astyle', *cs_files
      sh 'git', 'add', *cs_files
    end
  end
end