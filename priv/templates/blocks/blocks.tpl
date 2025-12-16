{% for blk in m.rsc[id].blocks %}
    {% if blk and blk.type %}
        {% include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id first=forloop.first last=forloop.last %}
    {% endif %}
{% endfor %}
