function submitreservation(){
  $.ajax({
      url : "/services/save_worker_reservation/", // the endpoint
      type : "POST", // http method
      data : $('#reservation-form').serialize(), // data sent with the post request

      // handle a successful response
      success : function(response) {
          if (response == 'success'){
            $('#success-modal').modal('show');
            $('#ok-button').before('<p> Rezerwacja sie udala</p>');
          }
      },
      error : function(response) {
        $('#success-modal').modal('show');
        $('#ok-button').before('<p> Rezerwacja sie nie udala</p>');
    }
  });
}


$(document).ready(function() {

  $('#select-service-group').change(function(){
    showSelectService(this.value);

  });

  $('#submit').click(function(){
    submitreservation()
  });

  $('#resignation').click(function(){
    window.location.replace("/employee/");
  });

  $('#reservation-form').on('submit', function(e){
  e.preventDefault();
});

});
