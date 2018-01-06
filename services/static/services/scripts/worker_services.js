function get_all_reservations(){
  document.getElementById("filter-form").reset();
  $('#id_worker__first_name').val('');
  $('#id_worker__last_name').val('');
  $('#id_finish_timestamp').val('');
  $('#id_start_timestamp').val($.datepicker.formatDate('mm/dd/yy', new Date()));
  document.getElementById("filter-form").submit();
}

function get_all_past_reservations(){
  document.getElementById("filter-form").reset();
  $('#id_worker__first_name').val('');
  $('#id_worker__last_name').val('');
  $('#id_start_timestamp').val('');
  $('#id_finish_timestamp').val($.datepicker.formatDate('mm/dd/yy', new Date()));
  document.getElementById("filter-form").submit();
}

function get_all_my_future_reservations(){
  document.getElementById("filter-form").reset();

  $('#id_worker__first_name').val(worker_first_name);
  $('#id_worker__last_name').val(worker_last_name);

  $('#id_finish_timestamp').val('');
  $('#id_start_timestamp').val($.datepicker.formatDate('mm/dd/yy', new Date()));
  document.getElementById("filter-form").submit();
}

function get_all_my_past_reservations(){
  document.getElementById("filter-form").reset();

  $('#id_worker__first_name').val(worker_first_name);
  $('#id_worker__last_name').val(worker_last_name);

  $('#id_start_timestamp').val('');
  $('#id_finish_timestamp').val($.datepicker.formatDate('mm/dd/yy', new Date()));
  document.getElementById("filter-form").submit();
}

var old_form_data;

function get_report(form_data){
    window.location.href = '/services/generate_service_report/?' + $( '#format-form' ).serialize() + '&' + old_form_data;
}


$(document).ready(function() {
  old_form_data = $('#filter-form').serialize();
});
