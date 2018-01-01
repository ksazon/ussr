function submitreservation(){
  $.ajax({
      url : "/services/save_reservation/", // the endpoint
      type : "POST", // http method
      data : $('#reservation-form').serialize(), // data sent with the post request

      // handle a successful response
      success : function(response) {
          if (response == 'success'){
            $('#reservation-confirm-window').show();
            $('#ok-button').before('<p>Rezerwacja sie udala</p>');
          }
          else{
            $('#reservation-confirm-window').show();
            $('#ok-button').before('<p>Rezerwacja sie nie udala</p>');
          }
      },

  });

}

function submitWorkerReservation(){
  $.ajax({
      url : "/services/save_reservation/", // the endpoint
      type : "POST", // http method
      data : $('#reservation-form').serialize(), // data sent with the post request

      // handle a successful response
      success : function(response) {
          if (response == 'success'){
            $('#reservation-confirm-window').show();
            $('#ok-button').before('<p>Rezerwacja sie udala</p>');
          }
          else{
            $('#reservation-confirm-window').show();
            $('#ok-button').before('<p>Rezerwacja sie nie udala</p>');
          }
      },

  });

}

$(document).ready(function() {

  $('#select-service-group').change(function(){
    console.log(this.text);
    showSelectService(this.value);

  });

  $('#submit').click(function(){
    submitreservation()
  });

  $('#resignation').click(function(){
    window.location.replace("/services/reservation/");
  });

  $('#reservation-form').on('submit', function(e){
  e.preventDefault();
});

});
