{% for rid in result %}
    {
        "lat": "{{ rid.pivot_location_lat }}",
        "lng": "{{ rid.pivot_location_lng }}",
        "id": "{{ rid }}",
        "icon": "{{ rid.category_id.o.hasicon[1].medium.filename }}"
    }
    {% if not forloop.last %},{% endif %}
{% endfor %}