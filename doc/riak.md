mar/14'16
    run enable in the begining and after that you can run other functionalities

mar/15'16
    



io:format("ag:true-handle call~n"),
io:format("Hello, Mr. ~s!", [Name]);
erl -pa ebin -pa deps/*/ebin -name node1@127.0.0.1

question to nick:

1. K/V
2. join node in cluster


==============================
=installation Riak_ensemble===
==============================

1. install erlang R16B02 use build-erlang-r16b02.sh script
2. get the source from https://github.com/basho/riak_ensemble/
3. setup the env:   {env, [{data_root, "/tmp/riak_ensemble/"}]}
3. get the all dependencies by run make deps
    problem in dependecies: neotoma
            {neotoma, "1.7.2-9-g2f2b8e6", {git, "git://github.com/seancribbs/neotoma.git", {tag, "1.7.2-9-g2f2b8e6"}}}

4. start the command line: erl -pa ebin deps/*/ebin
5. after enter the shell, start_up riak_ensemble by enter:  applicaton:ensure_all(riak_ensemble)
6. let's play!

==============================
===Video on youtube====
==============================

CAP theorm:
	consistency
	avail
	partition-tolerance

consensus:
 we need: strong consistency, replicas
  quorum consesnsus algo
    *paxos* , ZK, raft
    	K/V => 2 times - quorum+ commit
    	::multi-paxos
    		paxos+ replication log
    			log recovery
    			log trimming:
    			rollup:
    			--
    			--
    			"paxos made live-an engineeing prospective"
    	raft: forcus to be easy-- insearch fo an understable: consensus algorithm

    riak-enesemble
    	paxos bases
    	focus: micro-states
    	-solve:scalability, odd numbers
    	k/v - key is independent state
    	semantic
    	 conditional, atomic

    	 design:
    	 simple multi-paxos

    	 put- 



  chain replication
  virtual synchro


strong consistency
	-recebct
	- patial writers
	- atomically

conditional
	single key 
	atomic 

solution
	paxos + replicated Log
		Zab : zookeeper code
		Raft - In search of an understanable consensus Algorithm - paper
			key/value
			key adre independent
			active anti-entropy??
			tunable backends
let's go to min 24
                
==============================
Developer
==============================

Joseph Blomstedt
Engel A. Sanchez
Nick Marino


==============================
term in erlang-
==============================

export declaration, and tells the compiler which functions (separated by commas) should be visible from the outside
module declaration, -module , something not function or comment
should end with .
- variable start by UPPERCASE letter, also can be started by _Var (underscore)












----------------------------
when you run 3 nodes..
1. you should see 1 node become a leader while the other become followers
2. when you request to store a key-value data to the servers from the client, you should be able to read back the value based on the key..



1. install the riak
2. run the system with 3 nodes
3. while the system run with 3 nodes, inspect the nodes' state (who are the leader, who are the followers)
4. find a way to run a client
5. through the client, store a key-value to the running 3 nodes
6. from the client, again, get the value based on the key to the running 3 nodes

Hi Nick, 

Thanks 

I am working on a part of model checker tool called SAMC(Semantic-Aware Model Checking). SAMC is designed to find deep bugs and work systematically for cloud systems bug testing.

In the part of the project, we plan to integrate SAMC to multi-paxos consensus riak_ensemble. We interested to do it because riak_ensemble based on erlang which really good in distributed system. Moreover, riak_ensemble is needed to maintain strong consistency across a cluster of nodes.

In order to integrated SAMC to riak_ensemble, my tasks are:
1. Install and run riak_ensemble with 3 nodes or more.
3. while the system run with 3 nodes, inspect the nodes' state (who are the leader, who are the followers)
4. find a way to run a client
5. through the client, store a key-value to the running 3 nodes
6. from the client, again, get the value based on the key to the running 3 nodes

During the exploration of riak ensemble, I am interesting to learn more erlang coding structure because it's very unique and so much different with programming language that I've ever learned. 