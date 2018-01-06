var old_form_data;

function get_report(form_data){
    window.location.href = '/clients/generate_clients_report/?' + $( '#format-form' ).serialize() + '&' + old_form_data;
}

$(document).ready(function() {
  old_form_data = $('#filter-form').serialize();
});
