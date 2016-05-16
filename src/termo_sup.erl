-module(termo_sup).

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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Termo_gs_runner_module_id			= util:get_ev (termo, termo_gs_runner_module_id, termo_gs_runner_id),
	Termo_gs_runner_module_name			= util:get_ev (termo, termo_gs_runner_module_name, termo_gs_runner),
	
    Termo_gs_log_driver_module_id      	= util:get_ev (termo, termo_gs_log_driver_module_id, termo_gs_log_driver_module_id),
    Termo_gs_log_driver_module_name   	= util:get_ev (termo, termo_gs_log_driver_module_name, termo_gs_log_driver),

    Wialon_token     					= util:get_ev (termo, termo_wialon_token, 'f16039bab187c0ef5aaf68cb1f72787668B3EA1BC557A1C8966C97799CEF60BE46AEAB50'),



    {ok, 
        {
            {one_for_one, 10, 1000},                                % Стратегия перезапуска. Если падает кто-то, то перезапускаем только его (они друг другу не мешают)
                                                                    % максимум 10 перезапусков с интервалом 10 секунд
            [
              {
                Termo_gs_runner_module_id,                                                                             % Id          = Внутренний идентификатор процесса внутри супервизора
                {Termo_gs_runner_module_name, start_link, [Wialon_token]},       % StartFun    = Запускаем из модуля fc_gs_sender_module_id
                permanent,                                                                                              % Restart     = permanent | transient | temporary - перезапуск всегда когда падает                                                                       
                2000,                                                                                                   % Shutdown    = brutal_kill | int() >= 0 | infinity - пару секунд на выполнение terminate
                worker,                                                                                                 % Type        = worker | supervisor - тип: рабочий процесс
                [Termo_gs_runner_module_name]                                                                           % Modules     = [Module] | dynamic - определен только в этом модуле 
              },
              {
                Termo_gs_log_driver_module_id,                                                                             % Id          = Внутренний идентификатор процесса внутри супервизора
                {Termo_gs_log_driver_module_name, start_link, []},       % StartFun    = Запускаем из модуля fc_gs_sender_module_id
                permanent,                                                                                              % Restart     = permanent | transient | temporary - перезапуск всегда когда падает                                                                       
                2000,                                                                                                   % Shutdown    = brutal_kill | int() >= 0 | infinity - пару секунд на выполнение terminate
                worker,                                                                                                 % Type        = worker | supervisor - тип: рабочий процесс
                [Termo_gs_log_driver_module_name]                                                                           % Modules     = [Module] | dynamic - определен только в этом модуле 
              }
           ]
        }
    }.

