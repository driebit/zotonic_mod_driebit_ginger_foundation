-module(mod_driebit_ginger_foundation).
-author("Driebit <info@driebit.nl>").

-mod_title("Driebit Ginger Foundation").
-mod_description("Provides templates and css compatible with Ginger.").
-mod_prio(200).
-mod_config([
    #{
        module => site,
        key => recaptcha_key,
        type => text,
        default => <<>>,
        title => "Key voor Google reCAPTCHA",
        description => "Key voor Google reCAPTCHA voor gebruik in formulieren"
    },
    #{
        module => site,
        key => recaptcha_secret_key,
        type => text,
        default => <<>>,
        title => "Secret site key voor Google reCAPTCHA",
        description => "Secret site key voor Google reCAPTCHA validaties"
    }
]).

-export([
    event/2,
    observe_rsc_get/3,
    observe_rsc_update/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message={map_infobox, _Args}}, Context) ->
    %% Click on a map marker (ids), show the items in an infobox/dialog.
    Ids = z_context:get_q(<<"ids">>, Context),
    {Template, Vars} = case z_context:get_q(<<"data">>, Context) of
        None when None =:= undefined; None =:= [] ->
            {"map/map-infobox.tpl", [ {results, Ids} ]};
        Data ->
            {"map/map-infobox-data-item..tpl", [ {item, Data} ]}
    end,
    {HTML, _HtmlContext} = z_template:render_to_iolist(Template, Vars, Context),
    Element = z_context:get_q(<<"element">>, Context),
    EscapedRender = z_utils:js_escape(iolist_to_binary(HTML)),
    JS = erlang:iolist_to_binary(
        [
            <<"$('#">>,
            Element,
            <<"').data('ui-googlemap').showInfoWindow(">>,
            z_convert:to_binary(lists:last(Ids)),
            <<", \"">>,
            z_convert:to_binary(EscapedRender),
            <<"\");">>
        ]
    ),
    z_render:wire({script, [{script, JS}]}, Context).


%% @doc On the fly fix for property renames.
observe_rsc_get(#rsc_get{}, #{ <<"is_feature_enable_comments">> := _ } = Raw, _Context) ->
    Raw;
observe_rsc_get(#rsc_get{}, #{ <<"feature_enable_comments">> := V } = Raw, _Context) ->
    Raw1 = Raw#{
        <<"is_feature_enable_comments">> => z_convert:to_bool(V)
    },
    maps:remove(<<"feature_enable_comments">>, Raw1);
observe_rsc_get(#rsc_get{}, Raw, _Context) ->
    Raw.


%% @doc Default inserted resources to the content group in the postback data.
%% This is used when adding media and links to remarks.
observe_rsc_update(#rsc_update{ action = insert }, {ok, Raw}, Context) ->
    DefaultCG = m_rsc:rid(<<"default_content_group">>, Context),
    case maps:get(<<"content_group_id">>, Raw, undefined) of
        undefined -> maybe_postback_cg(Raw, Context);
        DefaultCG -> maybe_postback_cg(Raw, Context);
        _ -> {ok, Raw}
    end;
observe_rsc_update(#rsc_update{}, Acc, _Context) ->
    Acc.

maybe_postback_cg(Raw, Context) ->
    case z_context:get_q(<<"z_postback_data">>, Context) of
        #{ <<"content_group_id">> := CGId } when is_integer(CGId) ->
            case m_rsc:is_a(CGId, content_group, Context) of
                true ->
                    {ok, Raw#{ <<"content_group_id">> => CGId }};
                false ->
                    {ok, Raw}
            end;
        _ ->
            {ok, Raw}
    end.
