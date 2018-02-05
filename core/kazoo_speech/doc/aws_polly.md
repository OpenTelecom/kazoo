# AWS Polly

## Configuration

```
{
    "data": {
        "default": {
            "tts_aws_polly_url": "https://polly.eu-west-2.amazonaws.com/v1/speech",
            "tts_aws_key_id": "YOURAWSKEYID00000000",
            "tts_aws_secret_key": "YOURAWSSECRETKEY-000-CANCOPYONLYONCREATE",
            "tts_speed": 22050,
            "tts_media_format": "mp3",
            "tts_provider": "aws_polly"
        },
        "id": "speech"
    }
}
```
In configuration section you must specify AWS api keys as described in AWS user summary of your account (in the tab "Security credentials" -> "Access keys"). More information in AWS docs [Understanding and Getting Your Security Credentials](https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html)

Additionl information about [AWS SynthesizeSpeech api](https://docs.aws.amazon.com/polly/latest/dg/API_SynthesizeSpeech.html).


## Available voices

You can set default voices for languages with more then two AWS voices in `AWS_VOICE_MAPPINGS` macro in module.



country / aws voice id |sex|language code|voice|voice mapping string|voice type
---------------------- |----|-------------|-----|--------------------|----------
**English (US) (en-US)** |
Ivy|Female|en-US|female/en-US|,{<<"female/en-us">>, <<"Ivy">>}|default
Joey|Male|en-US|male/en-US|,{<<"male/en-us">>, <<"Joey">>}|default
Ivy|Female|en-US|female/en-US/Ivy|,{<<"female/en-us/ivy">>, <<"Ivy">>}|
Joanna|Female|en-US|female/en-US/Joanna|,{<<"female/en-us/joanna">>, <<"Joanna">>}|
Joey|Male|en-US|male/en-US/Joey|,{<<"male/en-us/joey">>, <<"Joey">>}|
Justin|Male|en-US|male/en-US/Justin|,{<<"male/en-us/justin">>, <<"Justin">>}|
Kendra|Female|en-US|female/en-US/Kendra|,{<<"female/en-us/kendra">>, <<"Kendra">>}|
Kimberly|Female|en-US|female/en-US/Kimberly|,{<<"female/en-us/kimberly">>, <<"Kimberly">>}|
Matthew|Male|en-US|male/en-US/Matthew|,{<<"male/en-us/matthew">>, <<"Matthew">>}|
Salli|Female|en-US|female/en-US/Salli|,{<<"female/en-us/salli">>, <<"Salli">>}|
**English (Canada) (en-CA)** |
unsupported|Female|en-CA|female/en-CA|,{<<"female/en-ca">>, 'undefined'}|default
Nicole|Male|en-CA|male/en-CA/Nicole|,{<<"male/en-ca/nicole">>, <<"Nicole">>}|
**English (Australian) (en-AU)** |
Nicole|Female|en-AU|female/en-AU|,{<<"female/en-au">>, <<"Nicole">>}|default
Nicole|Female|en-AU|female/en-AU/Nicole|,{<<"female/en-au/nicole">>, <<"Nicole">>}|
Russell|Male|en-AU|male/en-AU/Russell|,{<<"male/en-au/russell">>, <<"Russell">>}|
**English (British) (en-GB)** |
Amy|Female|en-GB|female/en-GB|,{<<"female/en-gb">>, <<"Amy">>}|default
Brian|Male|en-GB|male/en-GB|,{<<"male/en-gb">>, <<"Brian">>}|default
Amy|Female|en-GB|female/en-GB/Amy|,{<<"female/en-gb/amy">>, <<"Amy">>}|
Brian|Male|en-GB|male/en-GB/Brian|,{<<"male/en-gb/brian">>, <<"Brian">>}|
Emma|Female|en-GB|female/en-GB/Emma|,{<<"female/en-gb/emma">>, <<"Emma">>}|
**English (Indian) (en-IN)** |
Aditi|Female|en-IN|female/en-IN/Aditi|,{<<"female/en-in/aditi">>, <<"Aditi">>}|
Raveena|Female|en-IN|female/en-IN/Raveena|,{<<"female/en-in/raveena">>, <<"Raveena">>}|
**English (Welsh) (en-GB-WLS)** |
Geraint|Male|en-GB-WLS|male/en-GB-WLS/Geraint|,{<<"male/en-gb-wls/geraint">>, <<"Geraint">>}|
**Spanish (Latin American) (es-US)** |
Penelope|Female|es-US|female/es-US|,{<<"female/es-us">>, <<"Penelope">>}|default
Miguel|Male|es-US|male/es-US|,{<<"male/es-us">>, <<"Miguel">>}|default
Penelope|Female|es-US|female/es-US/Penelope|,{<<"female/es-us/penelope">>, <<"Penelope">>}|
Miguel|Male|es-US|male/es-US/Miguel|,{<<"male/es-us/miguel">>, <<"Miguel">>}|
**us-US** |
unsupported|Female|us-US|female/us-US|,{<<"female/us-us">>, 'undefined'}|default
**Chinese (Simplified) (zh-CN)** |
unsupported|Female|zh-CN|female/zh-CN|,{<<"female/zh-cn">>, 'undefined'}|default
unsupported|Male|zh-CN|male/zh-CN|,{<<"male/zh-cn">>, 'undefined'}|default
**Chinese (Traditional, Hong Kong) (zh-HK)** |
unsupported|Female|zh-HK|female/zh-HK|,{<<"female/zh-hk">>, 'undefined'}|default
**Chinese (Traditional) (zh-TW)** |
unsupported|Female|zh-TW|female/zh-TW|,{<<"female/zh-tw">>, 'undefined'}|default
**Japanese (ja-JP)** |
Mizuki|Female|ja-JP|female/ja-JP|,{<<"female/ja-jp">>, <<"Mizuki">>}|default
Takumi|Male|ja-JP|male/ja-JP|,{<<"male/ja-jp">>, <<"Takumi">>}|default
Mizuki|Female|ja-JP|female/ja-JP/Mizuki|,{<<"female/ja-jp/mizuki">>, <<"Mizuki">>}|
Takumi|Male|ja-JP|male/ja-JP/Takumi|,{<<"male/ja-jp/takumi">>, <<"Takumi">>}|
**Korean (ko-KR)** |
Seoyeon|Female|ko-KR|female/ko-KR|,{<<"female/ko-kr">>, <<"Seoyeon">>}|default
unsupported|Male|ko-KR|male/ko-KR|,{<<"male/ko-kr">>, 'undefined'}|default
Seoyeon|Female|ko-KR|female/ko-KR/Seoyeon|,{<<"female/ko-kr/seoyeon">>, <<"Seoyeon">>}|
**Danish (da-DK)** |
Naja|Female|da-DK|female/da-DK|,{<<"female/da-dk">>, <<"Naja">>}|default
Naja|Female|da-DK|female/da-DK/Naja|,{<<"female/da-dk/naja">>, <<"Naja">>}|
Mads|Male|da-DK|male/da-DK/Mads|,{<<"male/da-dk/mads">>, <<"Mads">>}|
**German (de-DE)** |
Marlene|Female|de-DE|female/de-DE|,{<<"female/de-de">>, <<"Marlene">>}|default
Hans|Male|de-DE|male/de-DE|,{<<"male/de-de">>, <<"Hans">>}|default
Marlene|Female|de-DE|female/de-DE/Marlene|,{<<"female/de-de/marlene">>, <<"Marlene">>}|
Hans|Male|de-DE|male/de-DE/Hans|,{<<"male/de-de/hans">>, <<"Hans">>}|
Vicki|Female|de-DE|female/de-DE/Vicki|,{<<"female/de-de/vicki">>, <<"Vicki">>}|
**Spanish(Catalian) (ca-ES)** |
unsupported|Female|ca-ES|female/ca-ES|,{<<"female/ca-es">>, 'undefined'}|default
**Spanish (Castilian) (es-ES)** |
Conchita|Female|es-ES|female/es-ES|,{<<"female/es-es">>, <<"Conchita">>}|default
Enrique|Male|es-ES|male/es-ES|,{<<"male/es-es">>, <<"Enrique">>}|default
Conchita|Female|es-ES|female/es-ES/Conchita|,{<<"female/es-es/conchita">>, <<"Conchita">>}|
Enrique|Male|es-ES|male/es-ES/Enrique|,{<<"male/es-es/enrique">>, <<"Enrique">>}|
**Finland (Finnish) (fi-FI)** |
unsupported|Female|fi-FI|female/fi-FI|,{<<"female/fi-fi">>, 'undefined'}|default
**French (Canadian) (fr-CA)** |
Chantal|Female|fr-CA|female/fr-CA|,{<<"female/fr-ca">>, <<"Chantal">>}|default
unsupported|Male|fr-CA|male/fr-CA|,{<<"male/fr-ca">>, 'undefined'}|default
Chantal|Female|fr-CA|female/fr-CA/Chantal|,{<<"female/fr-ca/chantal">>, <<"Chantal">>}|
**French (fr-FR)** |
Celine|Female|fr-FR|female/fr-FR|,{<<"female/fr-fr">>, <<"Celine">>}|default
Mathieu|Male|fr-FR|male/fr-FR|,{<<"male/fr-fr">>, <<"Mathieu">>}|default
Celine|Female|fr-FR|female/fr-FR/Celine|,{<<"female/fr-fr/celine">>, <<"Celine">>}|
Mathieu|Male|fr-FR|male/fr-FR/Mathieu|,{<<"male/fr-fr/mathieu">>, <<"Mathieu">>}|
**Italian (it-IT)** |
Carla|Female|it-IT|female/it-IT|,{<<"female/it-it">>, <<"Carla">>}|default
Giorgio|Male|it-IT|male/it-IT|,{<<"male/it-it">>, <<"Giorgio">>}|default
Carla|Female|it-IT|female/it-IT/Carla|,{<<"female/it-it/carla">>, <<"Carla">>}|
Giorgio|Male|it-IT|male/it-IT/Giorgio|,{<<"male/it-it/giorgio">>, <<"Giorgio">>}|
**Norwegian (nb-NO)** |
Liv|Female|nb-NO|female/nb-NO|,{<<"female/nb-no">>, <<"Liv">>}|default
Liv|Female|nb-NO|female/nb-NO/Liv|,{<<"female/nb-no/liv">>, <<"Liv">>}|
**Dutch (nl-NL)** |
Lotte|Female|nl-NL|female/nl-NL|,{<<"female/nl-nl">>, <<"Lotte">>}|default
Lotte|Female|nl-NL|female/nl-NL/Lotte|,{<<"female/nl-nl/lotte">>, <<"Lotte">>}|
Ruben|Male|nl-NL|male/nl-NL/Ruben|,{<<"male/nl-nl/ruben">>, <<"Ruben">>}|
**Polish (pl-PL)** |
Ewa|Female|pl-PL|female/pl-PL|,{<<"female/pl-pl">>, <<"Ewa">>}|default
Ewa|Female|pl-PL|female/pl-PL/Ewa|,{<<"female/pl-pl/ewa">>, <<"Ewa">>}|
Maja|Female|pl-PL|female/pl-PL/Maja|,{<<"female/pl-pl/maja">>, <<"Maja">>}|
Jacek|Male|pl-PL|male/pl-PL/Jacek|,{<<"male/pl-pl/jacek">>, <<"Jacek">>}|
Jan|Male|pl-PL|male/pl-PL/Jan|,{<<"male/pl-pl/jan">>, <<"Jan">>}|
**Portuguese (Brazilian) (pt-BR)** |
Vitoria|Female|pt-BR|female/pt-BR|,{<<"female/pt-br">>, <<"Vitoria">>}|default
Vitoria|Female|pt-BR|female/pt-BR/Vitoria|,{<<"female/pt-br/vitoria">>, <<"Vitoria">>}|
Ricardo|Male|pt-BR|male/pt-BR/Ricardo|,{<<"male/pt-br/ricardo">>, <<"Ricardo">>}|
**Portuguese (European) (pt-PT)** |
Ines|Female|pt-PT|female/pt-PT|,{<<"female/pt-pt">>, <<"Ines">>}|default
Cristiano|Male|pt-PT|male/pt-PT|,{<<"male/pt-pt">>, <<"Cristiano">>}|default
Ines|Female|pt-PT|female/pt-PT/Ines|,{<<"female/pt-pt/ines">>, <<"Ines">>}|
Cristiano|Male|pt-PT|male/pt-PT/Cristiano|,{<<"male/pt-pt/cristiano">>, <<"Cristiano">>}|
**Russian (ru-RU)** |
Tatyana|Female|ru-RU|female/ru-RU|,{<<"female/ru-ru">>, <<"Tatyana">>}|default
Maxim|Male|ru-RU|male/ru-RU|,{<<"male/ru-ru">>, <<"Maxim">>}|default
Tatyana|Female|ru-RU|female/ru-RU/Tatyana|,{<<"female/ru-ru/tatyana">>, <<"Tatyana">>}|
Maxim|Male|ru-RU|male/ru-RU/Maxim|,{<<"male/ru-ru/maxim">>, <<"Maxim">>}|
**Swedish (sv-SE)** |
Astrid|Female|sv-SE|female/sv-SE|,{<<"female/sv-se">>, <<"Astrid">>}|default
Astrid|Female|sv-SE|female/sv-SE/Astrid|,{<<"female/sv-se/astrid">>, <<"Astrid">>}|
**Hungary (Hungarian) (hu-Hu)** |
unsupported|Female|hu-Hu|female/hu-Hu|,{<<"female/hu-hu">>, 'undefined'}|default
**Czech (cs-CZ)** |
unsupported|Female|cs-CZ|female/cs-CZ|,{<<"female/cs-cz">>, 'undefined'}|default
**Turkish (tr-TR)** |
Filiz|Female|tr-TR|female/tr-TR|,{<<"female/tr-tr">>, <<"Filiz">>}|default
unsupported|Male|tr-TR|male/tr-TR|,{<<"male/tr-tr">>, 'undefined'}|default
Filiz|Female|tr-TR|female/tr-TR/Filiz|,{<<"female/tr-tr/filiz">>, <<"Filiz">>}|
**Icelandic (is-IS)** |
Dora|Female|is-IS|female/is-IS/Dora|,{<<"female/is-is/dora">>, <<"Dora">>}|
Karl|Male|is-IS|male/is-IS/Karl|,{<<"male/is-is/karl">>, <<"Karl">>}|
**Romanian (ro-RO)** |
Carmen|Female|ro-RO|female/ro-RO/Carmen|,{<<"female/ro-ro/carmen">>, <<"Carmen">>}|
**Welsh (cy-GB)** |
Gwyneth|Female|cy-GB|female/cy-GB/Gwyneth|,{<<"female/cy-gb/gwyneth">>, <<"Gwyneth">>}|