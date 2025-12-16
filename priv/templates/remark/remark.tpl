{% with
    editing|default:q.editing|to_boolean,
    remark_id|default:q.remark_id|escape,
    is_new|default:q.is_new|default:false|to_boolean,
    id|default:q.id|default:undefined|escape
   as
    editing,
    remark_id,
    is_new,
    id
%}
    {% if editing %}
        {% include "remark/remark-edit.tpl" remark_id=m.rsc[remark_id].id editing=editing id=id %}
    {% else %}
        {% include "remark/remark-view.tpl" remark_id=m.rsc[remark_id].id editing=editing id=id %}
    {% endif %}
{% endwith %}
