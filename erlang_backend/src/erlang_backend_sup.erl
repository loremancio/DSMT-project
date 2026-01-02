-module(erlang_backend_sup).
-behavior(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %% Legge il ruolo dal file di configurazione
  {ok, Role} = application:get_env(project, role),

  io:format(">> [SUPERVISOR] Avvio come ruolo: ~p~n", [Role]),

  %% Definisce il FIGLIO direttamente in base al ruolo
  ChildSpec = case Role of
                coordinator ->
                  #{
                    id => erlang_coordinator,
                    start => {erlang_coordinator, start_link, []}, %% Avvia direttamente il GenServer
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlang_coordinator]
                  };
                worker ->
                  #{
                    id => erlang_actor,
                    start => {erlang_actor, start_link, []}, %% Avvia direttamente il GenServer
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlang_actor]
                  }
              end,

  %% Avvia il processo scelto
  {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [ChildSpec]}}.