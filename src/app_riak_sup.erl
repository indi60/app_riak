
-module(app_riak_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	 io:format("ag:supervisor_start_link~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  	io:format("ag:supervisor_init~n"),
	{ok, DataRoot} = application:get_env(app_riak,data_root),

	Ensemble = {riak_ensemble_sup, {riak_ensemble_sup, start_link, [DataRoot ++ atom_to_list(node())]},
            permanent, 20000, supervisor, [riak_ensemble_sup]},

	ClusterWatcher = {app_riak_cluster, {app_riak_cluster, start_link, []},
            permanent, 20000, worker, [app_riak_cluster]},

    % io:format("The DataRoot is: ~p.", [Ensemble]),
    {ok, { {one_for_one, 5, 10}, [Ensemble,ClusterWatcher]} }.

