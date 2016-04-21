-module(app_riak_cluster).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {nodes}).

start_link() ->
  io:format("ag:cluster_start_link~n"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  io:format("ag:cluster_init~n"),
  {ok, NodeList} = application:get_env(app_riak, nodes),
  schedule_tick(),
  %% TODO: Link to riak_ensemble_sup?
  {ok, #state{nodes=NodeList}}.

handle_call(_Request, _From, State) ->
	io:format("ag:demo_cluster:handle_call~n"),
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
  io:format("ag:handle_cast~n"),
  {noreply, State}.

handle_info(tick, State) ->
  io:format("ag:handle_info~n"),
  State2 = tick(State),
  schedule_tick(),
  {noreply, State2};

handle_info(_Info, State) ->
  {noreply, State}.

schedule_tick() ->
  io:format("ag:schedule_tick~n"),
  erlang:send_after(1000, self(), tick).

tick(State=#state{nodes=Nodes}) ->
  io:format("ag:tick~n"),
  maybe_bootstrap_ensembles(Nodes),
  State.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.


% ag:
%  check if no node elable and total online nodes >= quorums
%  so enable a cluster
%  meaning create a cluster
% 
maybe_bootstrap_ensembles(Nodes) ->
  io:format("ag:demo_cluster:maybe_bootstrap_ensembles~n"),
  Members = riak_ensemble_manager:get_members(root),
  io:format("The members are: ~p.", [Members]),
  case riak_ensemble_manager:enabled() of
    false ->
      OnlineNodes = online_nodes(Nodes),
      QuorumNum = quorum_num(Nodes),
      EnabledNodes = find_enabled_node(OnlineNodes),

      if
        (EnabledNodes =:= []) and (length(OnlineNodes) >= QuorumNum) ->
          global:trans({ensemble_bootstrap,self()},
                       fun() ->
                           try
                             riak_ensemble_manager:enable(),
                             wait_stable(),
                             ok
                           catch
                             _:_ ->
                               error
                           end
                       end,
                       OnlineNodes,0);
        EnabledNodes =/= [] ->
          join_cluster(EnabledNodes);
        true ->
          ok
      end;
    true ->
      ok
  end.


quorum_num(Nodes) ->
  trunc(length(Nodes)/2)+1.

online_nodes(Nodes) ->
  io:format("ag:demo_cluster:online_nodes~n"),
  NodeStatus = [
                begin
                  case net_adm:ping(X) of
                    pong ->
                      case catch rpc:call(X,erlang,whereis,[?MODULE]) of
                        Y when is_pid(Y) ->
                          {X, ok};
                        _ ->
                          {X, error}
                      end;
                    _ ->
                      {X, error}
                  end
                end || X <- Nodes],
  proplists:get_keys(lists:filter(fun({X,Y}) -> {X,Y} =:= {X,ok} end,NodeStatus)).


wait_stable() ->
  % io:format("ag:wait_stable_demo_cluster~n"),
  case check_stable() of
    true ->
      ok;
    false ->
      wait_stable()
  end.


check_stable() ->
  % io:format("ag:check_stable_demo_cluster~n"),
  case riak_ensemble_manager:check_quorum(root, 1000) of
    true ->
      case riak_ensemble_peer:stable_views(root, 1000) of
        {ok, true} ->
          io:format("ag:check_stable~n"),
          true;
        _ ->
          false
      end;
    false ->
      false
  end.

find_enabled_node(Nodes) ->
  io:format("ag:demo_cluster:find_enabled_node~n"),
  lists:filter(
    fun(X) ->
        io:format("ag:find_enabled_node~n"),
        true =:= rpc:call(X,riak_ensemble_manager,enabled,[])
    end,
    Nodes
   ).


join_cluster([H|_T]) ->
  io:format("ag::join_cluster~n"),
  case riak_ensemble_manager:join(H,node()) of
    ok ->
      io:format("ag:jdemo_cluster:oin_cluster_OK~n"),
      wait_stable(),
      riak_ensemble_peer:update_members(
        riak_ensemble_manager:get_leader_pid(root),
        [{add,{root,node()}}],
        5000),
      ok;
    _ ->
      join_cluster([H])
  end.
