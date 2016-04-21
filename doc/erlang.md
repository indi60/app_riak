how to check erlang version:
   
   erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell

source: http://stackoverflow.com/questions/9560815/how-to-get-erlangs-release-version-number-from-a-shell

ECHO
    io:fwrite("Hello, world!\n").
	source: http://www.thegeekstuff.com/2010/05/erlang-hello-world-example/

	io:format("Message number ~p: ~p", [MyNum, MyMessage])
	http://stackoverflow.com/questions/22900348/ioformat-with-multiple-variables-erlang?rq=1

install erlang
	http://stackoverflow.com/questions/27215936/how-to-upgrade-the-erlang-version-on-ubuntu-r14b04-to-latest-stable-version
	https://www.erlang-solutions.com/resources/download.html

run multiple erlang in a machine
	http://stackoverflow.com/questions/14894447/installing-more-than-one-version-of-erlang-otp-on-a-machine
	https://aloiroberto.wordpress.com/2010/11/24/how-to-manage-multiple-erlang-installations/

compile/ build
	rebar
	https://github.com/rebar/rebar/wiki/Getting-started

install erlang r16b
	since riak_ensemble depricated to r18, I run a backward to r16
	https://gist.github.com/bryanhunter/5487621#file-build-erlang-r16b-sh	

good start
	http://learnyousomeerlang.com/starting-out-for-real#numbers
	variable must start from CAPITAL letter

what is /x mean
	http://stackoverflow.com/questions/13597522/erlang-export-syntax-x-understanding	

what is ||
	http://stackoverflow.com/questions/5962299/what-does-mean-in-erlang
	eg:
	If we have a list L:
	 L = [1,2,3,4,5].
	And we want to double every element, we can do:
	 lists:map(fun(X) -> 2*X end, L).
	But with List comprehensions we can do:
 	 [2*X || X <- L].

what is <<>>
	http://stackoverflow.com/questions/15731477/what-does-mean-in-erlang-code
	http://erlang.org/doc/reference_manual/expressions.html#bit_syntax
	<<>>
	<<E1,...,En>>

	Ei = Value |
     Value:Size |
     Value/TypeSpecifierList |
     Value:Size/TypeSpecifierList

     shoud use space after =

tupple:
	{}

list:
	[]

pattern maching pipe |
	
init
	The init module is pre-loaded and contains the code for the init system 
	process which coordinates the start-up of the system.		
	  http://erlang.org/doc/man/init.html

erl
	Starts an Erlang runtime system.
	  http://erlang.org/doc/man/erl.html

?MODULE
	macros- 
	 ?MODULE
	 The name of the current module.
	 ?MODULE_STRING.
	 The name of the current module, as a string.
	 ?FILE.
	 The file name of the current module.
	 ?LINE.
	 The current line number.
	 ?MACHINE.
	 The machine name, 'BEAM'.
	http://erlang.org/doc/reference_manual/macros.html


lists:seq(1,4)
	the output will be [1,2,3,4]