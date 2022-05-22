# Nginx-Clickhouse

## The goal

* Create a program to parse a log file based on the log format

## Run the project

* Clone this repository and go there
```sh
git clone https://github.com/br4ch1st0chr0n3/nginx-clickhouse-hs
cd nginx-clickhouse-hs
```

* Build and run
```sh
stack build
stack run
```

## Workflow

1. Takes a mapping `column name` -> `database type` from [schema.yml](./files/schema.yml) (the yml-ed version of [schema.sql](./files/schema.sql)). This is an  unnecessary step, since for now, the parser types for log elements are chosen by magic words, as in the original [solution](https://github.com/mintance/nginx-clickhouse/blob/4d36a9dded1ed1f9c90f3e89987ffef4766cc9db/nginx/nginx.go#L29)

1. Takes a mapping `column name` -> `magic word` and log format from [nginx_config.yml](./files/nginx_config.yml). I changed the log format to avoid collisions of `\x22` with `"` when using `megaparsec`

1. Parses log format, creates a sequence of parsers, parses a single line according to the log format. Now, it has a mapping `magic word` -> `log element`

1. Prepares a mapping `magic word` -> `parser for a log element`

1. Combines the mappings `column name` -> `magic word`, `magic word` -> `log element`, `magic word` -> `parser for a log element` to get a mapping `column name` -> `parsed log element`

1. Prints the result

## Sample run

* Input:
```haskell
"213.158.8.97 - - [06/Apr/2022:00:17:01 +0300] 'GET /v2/rt-wink/live/playlist?key=00000141-1a10-726f-7374-656c65636f6d&client_time=1649193421&channel_id=3541715&san=10215020191125&client_id=fc%3A44%3A9f%3A36%3A03%3Adb&location=200004&mcast=igmp%3A%2F%2F225.78.32.76%3A5000&event_id=00000000-0000-0000-0000-000000000001&age_value=18 HTTP/1.1' 'GET' '-' '0.002' 200 401 '-' 'RT-STB-FW/7.46.4 (zte_amls805, B700V7L) stbapp/1.54.4-gd649a14ff' '0.002' 'north-west.rt.getshop.tv' 'spb-1' '1.1 rt-nw-hub-proxy01 (squid/4.8)' 'fc:44:9f:36:03:db' '089D73ED-1045-4839-8D82-AC46662612FD' '10215020191125' '{\x22device_timestamp\x22:1.649193421468805927e9,\x22events\x22:[{\x22banner\x22:null,\x22id\x22:\x22 00000000-0000-0000-0000-000000000001\x22,\x22type\x22:\x22playlist_update\x22,\x22datetime\x22:1.649199847477686405181e9}]}' '-' '3541715' '-' '-' 'fc%3A44%3A9f%3A36%3A03%3Adb' '-' '-' '-' '-' '00000000-0000-0000-0000-000000000001' '200004' '10215020191125'"
```


```haskell
fromList
    [
        ( "arg_client_version"
        , Just
            ( EString "-" )
        )
    ,
        ( "arg_channel_name"
        , Just
            ( EString "-" )
        )
    ,
        ( "req_header_x_rt_mac"
        , Just
            ( EString "fc:44:9f:36:03:db" )
        )
    ,
        ( "ResponseBody"
        , Just
            ( EString "{⋞vice_timestamp":1.649193421468805927e9,Ȯvents":[{⊺nner":null,"id":" 00000000-0000-0000-0000-000000000001","type":"playlist_update",⋚tetime":1.649199847477686405181e9}]}" )
        )
    ,
        ( "arg_event_id"
        , Just
            ( EString "00000000-0000-0000-0000-000000000001" )
        )
    ,
        ( "RequestBody"
        , Just
            ( EString "-" )
        )
    ,
        ( "HttpReferer"
        , Just
            ( EString "-" )
        )
    ,
        ( "Request"
        , Just
            ( EString "GET /v2/rt-wink/live/playlist?key=00000141-1a10-726f-7374-656c65636f6d&client_time=1649193421&channel_id=3541715&san=10215020191125&client_id=fc%3A44%3A9f%3A36%3A03%3Adb&location=200004&mcast=igmp%3A%2F%2F225.78.32.76%3A5000&event_id=00000000-0000-0000-0000-000000000001&age_value=18 HTTP/1.1" )
        )
    ,
        ( "arg_channel_id"
        , Just
            ( EString "3541715" )
        )
    ,
        ( "HostName"
        , Just
            ( EString "spb-1" )
        )
    ,
        ( "RequestMethod"
        , Just
            ( EString "GET" )
        )
    ,
        ( "req_header_x_rt_uid"
        , Just
            ( EString "089D73ED-1045-4839-8D82-AC46662612FD" )
        )
    ,
        ( "Host"
        , Just
            ( EString "north-west.rt.getshop.tv" )
        )
    ,
        ( "Status"
        , Just
            ( EInt32 200 )
        )
    ,
        ( "RemoteUser"
        , Just
            ( EString "-" )
        )
    ,
        ( "UpstreamResponseTime"
        , Just
            ( EFloat32 2.0 e- 3 )
        )
    ,
        ( "RemoteAddr"
        , Just
            ( EString "213.158.8.97" )
        )
    ,
        ( "arg_location"
        , Just
            ( EString "200004" )
        )
    ,
        ( "BytesSent"
        , Just
            ( EInt32 401 )
        )
    ,
        ( "RequestTime"
        , Just
            ( EFloat32 2.0 e- 3 )
        )
    ,
        ( "arg_campaign_id"
        , Just
            ( EString "-" )
        )
    ,
        ( "arg_display_height"
        , Just
            ( EString "-" )
        )
    ,
        ( "req_header_x_rt_san"
        , Just
            ( EString "10215020191125" )
        )
    ,
        ( "arg_chmap"
        , Just
            ( EString "-" )
        )
    ,
        ( "HttpUserAgent"
        , Just
            ( EString "RT-STB-FW/7.46.4 (zte_amls805, B700V7L) stbapp/1.54.4-gd649a14ff" )
        )
    ,
        ( "arg_display_width"
        , Just
            ( EString "-" )
        )
    ,
        ( "arg_san"
        , Just
            ( EString "10215020191125" )
        )
    ,
        ( "TimeLocal"
        , Just
            ( EDateTime 2022 - 04 - 05 21 : 17 : 01 UTC )
        )
    ,
        ( "arg_client_id"
        , Just
            ( EString "fc%3A44%3A9f%3A36%3A03%3Adb" )
        )
    ,
        ( "req_header_via"
        , Just
            ( EString "1.1 rt-nw-hub-proxy01 (squid/4.8)" )
        )
    ,
        ( "arg_client_session_id"
        , Just
            ( EString "-" )
        )
    ]
```

## TODO

* Provide better error handling
* Write comments