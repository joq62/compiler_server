%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_compile_server).

%% API
-export([
	 git_clone/2,
	 compile/1,
	 release/1,
	 start_application/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
git_clone(GitPath,ApplicationDir)->
    file:del_dir_r(ApplicationDir),
    [C||C<-os:cmd("git clone "++GitPath),
	C<256].
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
compile(ApplicationDir)->
    {ok,CWD}=file:get_cwd(),
    Result=case file:set_cwd(ApplicationDir) of
	       {error,Reason}->
		   {error,Reason};
	       ok->
		   % Clean up before 
		   file:delete("rebar.lock"),
		   file:del_dir_r("_build"),    	    
		   {ok,[C||C<-os:cmd("rebar3 compile"),
		       C<256]}
	   end,
    ok=file:set_cwd(CWD),
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
release(ApplicationDir)->
    {ok,CWD}=file:get_cwd(),
    Result=case file:set_cwd(ApplicationDir) of
	       {error,Reason}->
		   {error,Reason};
	       ok->    	    
		  {ok,[C||C<-os:cmd("rebar3 release"),
		       C<256]}
	   end,
    ok=file:set_cwd(CWD),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_application(ReleaseFile,"daemon")->
    StartCmd="./"++ReleaseFile,
    Result=case os:cmd(StartCmd++" "++"daemon") of
	       []->
		   ok;
	       ErrorMsg->
		   Reason=[C||C<-ErrorMsg,
			      C<256],
		   {error,Reason}
	   end,
    Result;
start_application(ReleaseFile,"foreground")->
    StartCmd="./"++ReleaseFile,
    Result=case os:cmd(StartCmd++" "++"foreground") of
	       []->
		   ok;
	       ErrorMsg->
		   Reason=[C||C<-ErrorMsg,
			      C<256],
		   {error,Reason}
	   end,
    Result;
start_application(ReleaseFile,NotSupportedType)->
    {error,["Not supported Type ",NotSupportedType]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
