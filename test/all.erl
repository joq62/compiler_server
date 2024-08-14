%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).

-define(Application,"adder3").
-define(GitPath,"https://github.com/joq62/adder3.git").
-define(ApplicationDir,"adder3").
-define(ReleaseFile,"adder3/_build/default/rel/adder3/bin/adder3").
-define(CompileServerVm,test_compiler_server@c50).
-define(AdderVm,adder3@c50).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=test1(),
    


    timer:sleep(2000),
    io:format("Test OK !!! ~p~n",[?MODULE]),
    Application=?Application,
    TargetDir=Application++"_dir",
    LogFile=TargetDir++"/logs/"++Application++"/log.logs/test_logfile.1",
    LogStr=os:cmd("cat "++LogFile),
    L1=string:lexemes(LogStr,"\n"),
    [io:format("~p~n",[Str])||Str<-L1],

 %   rpc:call(?Vm,init,stop,[],5000),
    timer:sleep(4000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),


    pong=rpc:call(?CompileServerVm,compiler_server,ping,[],5000),
    CloneResult=rpc:call(?CompileServerVm,compiler_server,git_clone,[?GitPath,?ApplicationDir],5*5000),
    io:format("CloneResult ~p~n",[CloneResult]),
    CompileResult=rpc:call(?CompileServerVm,compiler_server,compile,[?ApplicationDir],5*5000),
    io:format("CompileResult ~p~n",[CompileResult]),
    ReleaseResult=rpc:call(?CompileServerVm,compiler_server,release,[?ApplicationDir],5*5000),
    io:format("ReleaseResult ~p~n",[ReleaseResult]),

    StartResult=rpc:call(?CompileServerVm,compiler_server,start_application,[?ReleaseFile,"daemon"],5*5000),
    io:format("StartResult ~p~n",[StartResult]),
    
    42=rpc:call(?AdderVm,adder3,add,[20,22],5000),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),


    AppVm=get_vm(?Application),
    rpc:call(AppVm,init,stop,[],5000),
    timer:sleep(2000),

    ok=application:start(log),
    ok=application:start(rd),
    ok=application:start(compiler_server),
    pong=log:ping(),
    pong=rd:ping(),
    pong=compiler_server:ping(),
%    []=os:cmd("./_build/default/rel/compile_server/bin/compile_server foreground"),
  %   []=os:cmd("./compile_server foreground"),

   
    ok.


get_vm(Application)->
    {ok,HostName}=net:gethostname(),
    list_to_atom(Application++"@"++HostName).
    

initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-[]],
    [rd:add_target_resource_type(TargetType)||TargetType<-[controller,adder3]],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.
