-module(kazoo_tts_aws_polly).

-behaviour(gen_tts_provider).

-export([
      create/4
    , set_api_key/1, set_aws_key_id/1 , set_aws_secret_key/1
    , get_aws_config/0
    , check_voice/0
]).

-include("kazoo_speech.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").


-define(AWS_VOICE_MAPPINGS,[
     {<<"female/da-dk">>, <<"Naja">>}
    ,{<<"female/da-dk/naja">>, <<"Naja">>}
    ,{<<"male/da-dk/mads">>, <<"Mads">>}
    ,{<<"female/nl-nl">>, <<"Lotte">>}
    ,{<<"female/nl-nl/lotte">>, <<"Lotte">>}
    ,{<<"male/nl-nl/ruben">>, <<"Ruben">>}
    ,{<<"female/en-au">>, <<"Nicole">>}
    ,{<<"female/en-au/nicole">>, <<"Nicole">>}
    ,{<<"male/en-au/russell">>, <<"Russell">>}
    ,{<<"female/en-gb">>, <<"Amy">>}
    ,{<<"male/en-gb">>, <<"Brian">>}
    ,{<<"female/en-gb/amy">>, <<"Amy">>}
    ,{<<"male/en-gb/brian">>, <<"Brian">>}
    ,{<<"female/en-gb/emma">>, <<"Emma">>}
    ,{<<"female/en-in/aditi">>, <<"Aditi">>}
    ,{<<"female/en-in/raveena">>, <<"Raveena">>}
    ,{<<"female/en-us">>, <<"Ivy">>}
    ,{<<"male/en-us">>, <<"Joey">>}
    ,{<<"female/en-us/ivy">>, <<"Ivy">>}
    ,{<<"female/en-us/joanna">>, <<"Joanna">>}
    ,{<<"male/en-us/joey">>, <<"Joey">>}
    ,{<<"male/en-us/justin">>, <<"Justin">>}
    ,{<<"female/en-us/kendra">>, <<"Kendra">>}
    ,{<<"female/en-us/kimberly">>, <<"Kimberly">>}
    ,{<<"male/en-us/matthew">>, <<"Matthew">>}
    ,{<<"female/en-us/salli">>, <<"Salli">>}
    ,{<<"female/en-ca">>, 'undefined'}
    ,{<<"male/en-ca/geraint">>, <<"Geraint">>}
    ,{<<"male/en-gb-wls/geraint">>, <<"Geraint">>}
    ,{<<"female/fr-fr">>, <<"Celine">>}
    ,{<<"male/fr-fr">>, <<"Mathieu">>}
    ,{<<"female/fr-fr/celine">>, <<"Celine">>}
    ,{<<"male/fr-fr/mathieu">>, <<"Mathieu">>}
    ,{<<"female/fr-ca">>, <<"Chantal">>}
    ,{<<"male/fr-ca">>, 'undefined'}
    ,{<<"female/fr-ca/chantal">>, <<"Chantal">>}
    ,{<<"female/de-de">>, <<"Marlene">>}
    ,{<<"male/de-de">>, <<"Hans">>}
    ,{<<"female/de-de/marlene">>, <<"Marlene">>}
    ,{<<"male/de-de/hans">>, <<"Hans">>}
    ,{<<"female/de-de/vicki">>, <<"Vicki">>}
    ,{<<"female/is-is/dora">>, <<"Dora">>}
    ,{<<"male/is-is/karl">>, <<"Karl">>}
    ,{<<"female/it-it">>, <<"Carla">>}
    ,{<<"male/it-it">>, <<"Giorgio">>}
    ,{<<"female/it-it/carla">>, <<"Carla">>}
    ,{<<"male/it-it/giorgio">>, <<"Giorgio">>}
    ,{<<"female/ja-jp">>, <<"Mizuki">>}
    ,{<<"male/ja-jp">>, <<"Takumi">>}
    ,{<<"female/ja-jp/mizuki">>, <<"Mizuki">>}
    ,{<<"male/ja-jp/takumi">>, <<"Takumi">>}
    ,{<<"female/ko-kr">>, <<"Seoyeon">>}
    ,{<<"male/ko-kr">>, 'undefined'}
    ,{<<"female/ko-kr/seoyeon">>, <<"Seoyeon">>}
    ,{<<"female/nb-no">>, <<"Liv">>}
    ,{<<"female/nb-no/liv">>, <<"Liv">>}
    ,{<<"female/pl-pl">>, <<"Ewa">>}
    ,{<<"female/pl-pl/ewa">>, <<"Ewa">>}
    ,{<<"female/pl-pl/maja">>, <<"Maja">>}
    ,{<<"male/pl-pl/jacek">>, <<"Jacek">>}
    ,{<<"male/pl-pl/jan">>, <<"Jan">>}
    ,{<<"female/pt-br">>, <<"Vitoria">>}
    ,{<<"female/pt-br/vitoria">>, <<"Vitoria">>}
    ,{<<"male/pt-br/ricardo">>, <<"Ricardo">>}
    ,{<<"female/pt-pt">>, <<"Ines">>}
    ,{<<"male/pt-pt">>, <<"Cristiano">>}
    ,{<<"female/pt-pt/ines">>, <<"Ines">>}
    ,{<<"male/pt-pt/cristiano">>, <<"Cristiano">>}
    ,{<<"female/ro-ro/carmen">>, <<"Carmen">>}
    ,{<<"female/ru-ru">>, <<"Tatyana">>}
    ,{<<"male/ru-ru">>, <<"Maxim">>}
    ,{<<"female/ru-ru/tatyana">>, <<"Tatyana">>}
    ,{<<"male/ru-ru/maxim">>, <<"Maxim">>}
    ,{<<"female/es-es">>, <<"Conchita">>}
    ,{<<"male/es-es">>, <<"Enrique">>}
    ,{<<"female/es-es/conchita">>, <<"Conchita">>}
    ,{<<"male/es-es/enrique">>, <<"Enrique">>}
    ,{<<"female/es-us">>, <<"Penelope">>}
    ,{<<"male/es-us">>, <<"Miguel">>}
    ,{<<"female/es-us/penelope">>, <<"Penelope">>}
    ,{<<"male/es-us/miguel">>, <<"Miguel">>}
    ,{<<"female/ca-es">>, 'undefined'}
    ,{<<"female/sv-se">>, <<"Astrid">>}
    ,{<<"female/sv-se/astrid">>, <<"Astrid">>}
    ,{<<"female/tr-tr">>, <<"Filiz">>}
    ,{<<"male/tr-tr">>, 'undefined'}
    ,{<<"female/tr-tr/filiz">>, <<"Filiz">>}
    ,{<<"female/cy-gb/gwyneth">>, <<"Gwyneth">>}
    ,{<<"female/cs-cz">>, 'undefined'}
    ,{<<"female/hu-hu">>, 'undefined'}
    ,{<<"female/fi-fi">>, 'undefined'}
    ,{<<"female/zh-cn">>, 'undefined'}
    ,{<<"male/zh-cn">>, 'undefined'}
    ,{<<"female/zh-hk">>, 'undefined'}
    ,{<<"female/zh-tw">>, 'undefined'}
    ,{<<"female/us-us">>, 'undefined'}]
).

-define(AWS_POLLY_SPEECH_METHOD, 'post').

-define(AWS_POLLY_VOICES_URL, <<"https://polly.eu-west-2.amazonaws.com/v1/voices">>).
-define(AWS_POLLY_VOICES_METHOD, 'get').
-define(AWS_POLLY_VOICES_LANGUAGE_CODE_MAP, #{<<"LanguageCode">> => <<"en-GB">>}).


-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_api_key">>, Key),
    'ok'.

-spec set_aws_key_id(kz_term:ne_binary()) -> 'ok'.
set_aws_key_id(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_aws_key_id">>, Key),
    'ok'.

-spec set_aws_secret_key(kz_term:ne_binary()) -> 'ok'.
set_aws_secret_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"tts_aws_secret_key">>, Key),
    'ok'.

-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
create(Text, Voice, Format, Options) ->
    VoiceMappings = ?AWS_VOICE_MAPPINGS,

    case props:get_value(kz_term:to_lower_binary(Voice), VoiceMappings) of
        'undefined' ->
            {'error', 'invalid_voice'};
        VoiceId ->
            make_request(Text, VoiceId, Format, Options)
    end.

-spec make_request(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> create_resp().
make_request(Text, VoiceId, Format, Options) ->
    BaseUrl = kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_aws_polly_url">>, <<"https://polly.eu-west-2.amazonaws.com/v1/speech">>),
    case http_uri:parse(BaseUrl) of
        {'ok', {_ProtocolAtom,_UserInfo, Host, _Port, Path, _Query}} ->
            make_request(Text, VoiceId, Format, Options, BaseUrl, Host, Path);
        _ -> {'error', 'invalid_aws_service_url'}
    end.
make_request(Text, VoiceId, Format, Options, BaseUrl, Host, Path) ->
    {Service, Config} = get_aws_config(),
    Method = ?AWS_POLLY_SPEECH_METHOD,
    Region = Config#aws_config.aws_region,

    Props = [
                 {<<"OutputFormat">>, Format}
                ,{<<"SampleRate">>, erlang:integer_to_binary(kapps_config:get_integer(?MOD_CONFIG_CAT, <<"tts_speed">>, 22050))}
                ,{<<"Text">>, Text}
                ,{<<"TextType">>, <<"text">>}
                ,{<<"VoiceId">>, VoiceId}
            ],
    Payload = kz_json:encode(kz_json:from_list(Props)),

    Headers = [
        {"Content-Type", "application/json; charset=UTF-8"},
        {"Host", Host},
        {"Content-Length", erlang:integer_to_list(string:len(binary_to_list(Payload)))}
    ],

    HTTPOptions = props:delete('receiver', Options),

    SignedHeaders = erlcloud_aws:sign_v4(Method, Path, Config, Headers, Payload, Region, Service, HTTPOptions),

    case props:get_value('receiver', Options) of
        Pid when is_pid(Pid) ->
            Response = kz_http:async_req(Pid, 'post', BaseUrl, SignedHeaders, Payload, HTTPOptions),
            create_response(Response);
        _ ->
            Response = kz_http:post(BaseUrl, SignedHeaders, Payload, HTTPOptions),
            create_response(Response)
    end.

-spec create_response(kz_http:ret()) ->
                             kz_http:req_id() |
                             {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                             {'error', 'tts_provider_failure', binary()}.
create_response({'error', _R}) ->
    lager:warning("creating speech file failed with error ~p", [_R]),
    {'error', 'tts_provider_failure', <<"unexpected error encountered accessing provider">>};
create_response({'http_req_id', ReqID}) ->
    lager:debug("speech file streaming as ~p", [ReqID]),
    {'ok', ReqID};
create_response({'ok', 200, Headers, Content}) ->
    ContentType = props:get_value("content-type", Headers),
    ContentLength = props:get_value("content-length", Headers),
    lager:debug("created speech file ~s of length ~s", [ContentType, ContentLength]),
    {'ok', kz_term:to_binary(ContentType), Content};
create_response({'ok', _Code, RespHeaders, Content}) ->
    lager:warning("creating speech file failed with code ~p: ~p", [_Code, Content]),
    _ = [lager:debug("hdr: ~p", [H]) || H <- RespHeaders],
    {'error', 'tts_provider_failure', kz_json:get_value(<<"message">>, kz_json:decode(Content))}.

-spec get_aws_config() -> {string(),aws_config()}.
get_aws_config() ->
    {_ok, {_Protocol,_UserInfo, Host, _Port, _Path, _Query}} = http_uri:parse(binary_to_list(?AWS_POLLY_VOICES_URL)),
    [Service |[ Region | _ ]] = re:split(Host, "[.]",[{return,list}]),
    {
        Service,
        #aws_config{
        access_key_id = kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_aws_key_id">>),
        secret_access_key = kapps_config:get_string(?MOD_CONFIG_CAT, <<"tts_aws_secret_key">>),
        security_token = 'undefined',
        aws_region = Region,
        http_client = 'httpc'}
    }.

-spec check_voice() -> {atom(),_}.
check_voice() ->
    {Service, Config} = get_aws_config(),
    Method = ?AWS_POLLY_VOICES_METHOD,
    {'ok', {ProtocolAtom,_UserInfo, Host, Port, Path, _Query}} = http_uri:parse(binary_to_list(?AWS_POLLY_VOICES_URL)),
    Protocol = atom_to_list(ProtocolAtom),
    Params = maps:to_list(?AWS_POLLY_VOICES_LANGUAGE_CODE_MAP),
    Region = Config#aws_config.aws_region,
    lager:debug("Method=~p~nProtocol=~p~nHost=~p~nPort=~p~nPath=~p~nParams=~p~nService=~p~nRegion=~p~n",
        [Method,Protocol,Host,Port,Path,Params,Service,Region]),
    case erlcloud_aws:aws_request4(Method, Protocol, Host, Port, Path, Params, Service,
        Config) of
    {ok, RespBody} ->
        {ok, jsx:decode(RespBody)};
    {error, Reason} ->
        {error, Reason}
    end.