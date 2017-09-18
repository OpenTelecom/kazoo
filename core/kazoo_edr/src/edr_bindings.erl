%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Conversant Ltd
%%% @doc
%%% EDR event distribution bindings
%%% @end
%%% @contributors
%%%    Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(edr_bindings).

-include("edr.hrl").

-export([bind/3, bind/4
        ,binding_keys/1
        ,event_binding_key/1
        ,distribute/1
        ,bindings_from_json/1
        ,bindings_to_json/1
        ]).

%% kazoo_bindings callbacks
-export([push/2]).

-record(binding_payload, {module     :: atom()
                         ,function   :: atom()
                         ,binding    :: edr_binding()
                         ,payload    :: any()
                         }).

-type bind_resp() :: ['ok' | {'error', 'exists'}].

-spec bind(edr_binding(), atom(), atom()) -> bind_resp().
-spec bind(edr_binding(), atom(), atom(), any()) -> bind_resp().
bind(Bindings, Module, Fun) ->
    bind(Bindings, Module, Fun, 'undefined').
bind(Bindings, Module, Fun, Payload) when is_list(Bindings) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) ->
    kazoo_bindings:bind(binding_keys(Binding), ?MODULE, 'push', #binding_payload{module=Module
                                                                                ,function=Fun
                                                                                ,binding=Binding
                                                                                ,payload=Payload
                                                                                }).

-spec binding_keys(edr_binding()) -> ne_binaries().
%% Accounts
binding_keys(#edr_binding{include_descendants='true'}=Binding) ->
    binding_keys(Binding#edr_binding{account_id= <<"*">>, include_descendants='false'});
binding_keys(#edr_binding{account_id=AccountId}=Binding) when not is_list(AccountId) ->
    binding_keys(Binding#edr_binding{account_id=[AccountId]});
%% Severity
binding_keys(#edr_binding{exact_severity='false', severity=Severity}=Binding) when not is_list(Severity) ->
    binding_keys(Binding#edr_binding{exact_severity='true', severity=levels(Severity, ?EDR_SEVERITY_LEVELS)});
binding_keys(#edr_binding{severity=Severity}=Binding) when not is_list(Severity) ->
    binding_keys(Binding#edr_binding{severity=[Severity]});
%% Verbosity
binding_keys(#edr_binding{exact_verbosity='false', verbosity=Verbosity}=Binding) when not is_list(Verbosity) ->
    binding_keys(Binding#edr_binding{exact_verbosity='true', verbosity=levels(Verbosity, ?EDR_VERBOSITY_LEVELS)});
binding_keys(#edr_binding{verbosity=Verbosity}=Binding) when not is_list(Verbosity) ->
    binding_keys(Binding#edr_binding{verbosity=[Verbosity]});
%% App name
binding_keys(#edr_binding{app_name=AppName}=Binding) when not is_list(AppName) ->
    binding_keys(Binding#edr_binding{app_name=[AppName]});
%% Actually get the bindings
binding_keys(#edr_binding{severity=Severities
                         ,verbosity=Verbosities
                         ,account_id=AccountIds
                         ,app_name=AppNames
                         }) ->
    [binding_key(S, V, Acc, App) || S <- Severities, V <- Verbosities, Acc <- AccountIds, App <- AppNames].

%% Levels equal to or more significant than the specified level
-spec levels(edr_severity(), [edr_severity()]) -> [edr_severity()];
            (edr_verbosity(), [edr_verbosity()]) -> [edr_verbosity()].
levels(Level, AllLevels) ->
    lists:dropwhile(fun(L) -> L =/= Level end, AllLevels).

-spec binding_key(edr_severity(), edr_verbosity(), api_binary(), ne_binary()) -> ne_binary().
binding_key(Severity, Verbosity, AccountId, AppName) ->
    <<"edr.", (kz_term:to_binary(Severity))/binary, "."
    ,(kz_term:to_binary(Verbosity))/binary, "."
    ,(kz_term:to_binary(AccountId))/binary, "."
    ,(kz_term:to_binary(AppName))/binary>>.

-spec event_binding_key(edr_event()) -> ne_binary().
event_binding_key(#edr_event{account_id=AccountId
                            ,severity=Severity
                            ,verbosity=Verbosity
                            ,app_name=AppName
                            }) ->
    binding_key(Severity, Verbosity, AccountId, AppName).

-spec distribute(edr_event()) -> 'ok'.
distribute(#edr_event{}=Event) ->
    kazoo_bindings:map(event_binding_key(Event), Event),
    'ok'.

-spec push(edr_binding(), edr_event()) -> any().
push(#binding_payload{module=Module, function=Fun, binding=Binding, payload=Payload}, #edr_event{}=Event) ->
    case should_push(Binding, Event) of
        'true' when Payload =:= 'undefined' -> erlang:apply(Module, Fun, [Event]);
        'true' -> erlang:apply(Module, Fun, [Payload, Event]);
        'false' -> 'ok'
    end.

%% Handle more complex bindings that can't be solved with routing keys here
-spec should_push(edr_binding(), edr_event()) -> boolean().
should_push(#edr_binding{account_id=AccountId, include_descendants=true}
           ,#edr_event{account_tree=AccountTree}) ->
    AccountTree =/= 'undefined'
    andalso lists:member(AccountId, AccountTree);
should_push(_Binding, _Event) ->
    'true'.

-spec bindings_from_json(kz_json:object() | kz_json:objects()) -> edr_binding() | [edr_binding()].
bindings_from_json(JObjs) when is_list(JObjs) ->
    [bindings_from_json(JObj) || JObj <- JObjs];
bindings_from_json(JObj) ->
    #edr_binding{account_id=kz_doc:account_id(JObj, <<"*">>)
                ,include_descendants=kz_json:is_true(<<"include_descendants">>, JObj)
                ,app_name=kz_json:get_binary_value(<<"app_name">>, JObj)
                ,severity=kz_json:get_atom_value(<<"severity">>, JObj, 'ok')
                ,exact_severity=kz_json:is_true(<<"exact_severity">>, JObj)
                ,verbosity=kz_json:get_atom_value(<<"verbosity">>, JObj, 'info')
                ,exact_verbosity=kz_json:is_true(<<"exact_verbosity">>, JObj)
                }.

-spec bindings_to_json(edr_binding() | [edr_binding()]) -> kz_json:object() | kz_json:objects().
bindings_to_json(Bindings) when is_list(Bindings) ->
    [bindings_to_json(Binding) || Binding <- Bindings];
bindings_to_json(Binding) ->
    kz_json:from_list([{<<"account_id">>, Binding#edr_binding.account_id}
                      ,{<<"include_descendants">>, Binding#edr_binding.include_descendants}
                      ,{<<"app_name">>, Binding#edr_binding.app_name}
                      ,{<<"severity">>, Binding#edr_binding.severity}
                      ,{<<"exact_severity">>, Binding#edr_binding.exact_severity}
                      ,{<<"verbosity">>, Binding#edr_binding.verbosity}
                      ,{<<"exact_verbosity">>, Binding#edr_binding.exact_verbosity}
                      ]).