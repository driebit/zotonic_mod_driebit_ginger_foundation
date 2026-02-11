{% with
    class|default:"category-of"
as
    class
%}

<div class="{{ class }}">
    <i class="icon--{{ id.category_id.name }}"></i>{{ id.category_id.id.title }}
</div>

{% endwith %}
