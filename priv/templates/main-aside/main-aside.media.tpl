{% if id.s.depiction or id.s.hasbanner %}
        {% with m.search[{query hasanyobject=[id,['depiction','hasbanner']] pagelen=6}] as result %}

            {% include "list/list-header.tpl" id=id list_title=_"Displayed in" items=result %}

            {% include "list/list.tpl" list_id="list--fixed-context" class="list--sided" items=result extraClasses="" id=id hide_showmore_button hide_showall_button %}

        {% endwith %}
{% endif %}


{% if id.o.subject %}
        {% with m.search[{query match_objects=id cat="media" is_published is_findable pagelen=6}] as result %}
            {% include "list/list-header.tpl" id=id list_title=_"Related" items=result %}

            {% include "keywords/keywords-aside.tpl" id=id items=result %}

            {% include "list/list.tpl" list_id="list--fixed-context" class="list--sided" items=result extraClasses="" id=id hide_showmore_button hide_showall_button %}
        {% endwith %}
{% endif %}
