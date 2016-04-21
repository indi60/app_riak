1. install the riak
2. run the system with 3 nodes
3. while the system run with 3 nodes, inspect the nodes' state (who are the leader, who are the followers)
4. find a way to run a client
5. through the client, store a key-value to the running 3 nodes
6. from the client, again, get the value based on the key to the running 3 nodes


steps installation:
1. make sure you have a new version of 


Trouble shoots:
1. we need to install rebar before able to run riak_ensemble. rebar is used to build and setup the dependencies 
of riak_ensamble. here're the steps:

	install rebar
	source:https://github.com/rebar/rebar/


	install the rebar dependencies:
	source: https://github.com/rebar/rebar/wiki/Dependency-management	
	 run: rebar get-deps
	 this command will download the dependencies of redepencies that you use

	 if still error: rebar delete-deps and then rebar clean
	source:http://stackoverflow.com/questions/10059579/rebar-unable-to-get-dependency-from-github

2.  time deprecated
	change erlang:now() to erlang:timestamp()
	http://erlang.org/documentation/doc-7.0-rc2/erts-7.0/doc/html/time_correction.html