{% if remark_id and not remark_id.is_editable %}
    <p>{_ You are not allowed to edit this remark. _}</p>
{% elseif not remark_id and not m.acl.is_allowed.insert.remark %}
    <p>{_ You are not allowed to add a remark. _}</p>
{% else %}
    <div class="remark" id="remark_form">
        {% wire id="rscform" type="submit"
                postback={remark_save
                    wrapper_element="remark_form"
                    body_element="remark-body-wrapper"
                    remark_id=remark_id
                    about_id=id
                }
                delegate="ginger_remark"
        %}
        <form id="rscform" method="post" action="postback" class="remark-form" data-tinyname="rsc-tiny{{#ident}}">

            <fieldset>
                {% if not m.acl.user and not remark_id %}
                    <div class="remark-form__anonymous">
                        <p class="remark-form__name">
                            <label for="anonymous_name">{_ Name _}</label>
                            <input type="text" name="anonymous_name" id="anonymous_name" value="{{ remark_id.anonymous_name }}" required>
                            {% validate id="anonymous_name" type={presence} %}
                        </p>
                        <p class="remark-form__email">
                            <label for="anonymous_email">{_ E-mail _}</label>
                            <input type="email" name="anonymous_email" id="anonymous_email" value="{{ remark_id.anonymous_email }}" required>
                            <small>
                                <label for="anonymous_email_visible">
                                    <input type="checkbox" name="anonymous_email_visible" id="anonymous_email_visible"> {_ visible for public _}
                                </label>
                            </small>
                            {% validate id="anonymous_email" type={presence} type={email} %}
                        </p>
                    </div>
                {% endif %}

                <p>
                    <label for="title">{_ Title _}</label>
                    <input type="text" name="title" id="title" value="{{ remark_id.title }}" required>
                    {% validate id="title" type={presence} %}
                </p>

                <div id="remark-body-wrapper">
                    <textarea rows="10" cols="10" id="rsc-tiny{{#ident}}" name="body" class="body z_editor-init form-control">{{ remark_id.body }}</textarea>
                </div>
            </fieldset>

            <div class="remark-form__buttons">
                <button class="remark-cancel btn--secondary" title="{_ cancel _}">{_ Cancel _}</button>
                <button class="remark-save btn--primary" type="submit">{_ Save _}</button>
            </div>
        </form>
    </div>

    {% javascript %}
        {% if m.rsc.cg_user_generated.id as cg_id %}
            z_postback_data_set('content_group_id', {{ cg_id }});
        {% endif %}

        window.zEditLanguage = function() {
            return '{{ z_language }}';
        };
        window.zAdminLinkDone = function(v) {
            window.z_zlink(v.url_language, v.title_language);
        };
        window.zAdminMediaDone = function(v) {
            window.z_choose_zmedia(v.object_id, v);
        };

        $(document).trigger('remark:editing');
        {% if is_new %}
            $(document).trigger('remark:new');
        {% endif %}
        $('#title').focus();
    {% endjavascript %}

    {% include "_ginger-editor.tpl"
            overrides_tpl="_tinymce_overrides.tpl"
            media_tab="upload"
            media_tabs_enabled=["upload", "url"]
            id=remark_id
    %}
{% endwith %}

