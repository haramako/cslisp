task :format do
  sh 'astyle', '--options=.astyle', *Dir.glob('**/*.cs').reject{|f| f=~ %r{/obj/} }
end
