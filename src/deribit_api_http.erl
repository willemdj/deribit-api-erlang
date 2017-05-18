-module(deribit_api_http).
-behaviour(gen_server).

-export([
  start/5,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  owner,
  key,
  secret,
  host,
  port,
  pid,
  pids_map = maps:new(),
  partialData = maps:new()
}).

start(Key, Secret, Protocol, Host, Port) ->
  gen_server:start(?MODULE, [self(), Key, Secret, Protocol, Host, Port], []).

init([Owner, Key, Secret, Protocol, Host, Port]) ->
  monitor(process, Owner),

  Configuration = case Protocol of
    http  -> #{ retry_timeout => 100, http_opts => #{ keepalive => 30000 }};
    https -> #{ transport => ssl, retry_timeout => 100, http_opts => #{ keepalive => 30000 } };
    _     -> throw(unknown_protocol)
  end,

  {ok, ConnPid} = gun:open(Host, Port, Configuration),
  {ok, _Protocol} = gun:await_up(ConnPid),
  NewState = #state{
    owner = Owner,
    key = Key,
    secret = Secret,
    host = Host,
    port = Port,
    pid = ConnPid
  },
  {ok, NewState}.

terminate(_Reason, #state{ pid = ConnPid }) ->
  gun:shutdown(ConnPid).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call({request, Uri, Data, Pid}, _From, #state{ pid = ConnPid, key = Key, secret = Secret, pids_map = PidsMap } = State) ->
  EncDataString = deribit_api_utils:to_string(maps:to_list(Data)),
  Method = case string:str(Uri, "/api/v1/public") > 0 of
    true ->
      get;
    false ->
      post
  end,

  StreamRef = case Method of
    get ->
      gun:get(ConnPid, Uri ++ "?" ++ EncDataString, []);
    post ->
      Signature = deribit_api_utils:generate_signature(Key, Secret, Uri, Data),
      Headers = [{<<"x-deribit-sig">>, Signature}, {<<"content-type">>, "application/x-www-form-urlencoded; charset=UTF-8"}],
      gun:post(ConnPid, Uri, Headers, EncDataString)
  end,
  {reply, StreamRef, State#state{pids_map = maps:put(StreamRef, Pid, PidsMap)}};

handle_call(_Request, _From, State) ->
  {reply, State, State}.

handle_info({gun_response, _, _Ref, nofin, 200, _Headers}, State) ->
  %wait for body
  {noreply, State};
handle_info({gun_respone, _, Ref, nofin, Status, _Headers}, State) ->
  prepareResponse(Ref, State,  {error, list_to_binary(io_lib:format("Wrong respone status: ~B", [Status]))});
handle_info({gun_data, _, Ref, nofin, NewData}, #state{partialData = PartialData} = State ) ->
  CurrentData = maps:get(Ref, PartialData, <<"">>),
  NewPartialData = maps:put(Ref, <<CurrentData/binary, NewData/binary>>, PartialData),
  {noreply, State#state{ partialData = NewPartialData }};
handle_info({gun_data, _, Ref, fin, Body}, #state{ partialData = PartialData } = State) ->
  Partial = maps:get(Ref, PartialData, <<"">>),
  Json = jiffy:decode(<<Partial/binary, Body/binary>>, [return_maps]),
  Result = case Json of
    #{ <<"success">> := true,  <<"result">>  := R } -> {ok, R};
    #{ <<"success">> := true,  <<"message">> := M } -> {ok, M};
    #{ <<"success">> := false, <<"message">> := M } -> {error, M};
    _ -> {error, <<"unknown response">>}
  end,
  NewPartialData = case Partial of
    <<"">> -> PartialData;
    _ -> maps:remove(Ref, PartialData)
  end,
  prepareResponse(Ref, State#state{ partialData = NewPartialData }, Result);
handle_info({'DOWN',_,process,Pid,_}, #state{ owner = Pid } = State) ->
  {stop, shutdown, State};
handle_info(_Info, State) ->
  {noreply, State}.

prepareResponse(Ref, #state{ pids_map = PidsMap} = State, Response) ->
  case maps:get(Ref, PidsMap, undefined) of
    undefined ->
      {noreply, State};
    Key ->
      case Key of
        undefined ->
          ok;
        _ when is_pid(Key) ->
          Key ! {self(), erlang:insert_element(1, Response, Ref)};
        _ when is_function(Key) ->
          erlang:apply(Key, [Response])
      end,
      {noreply, State#state{pids_map = maps:remove(Ref, PidsMap)}}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.