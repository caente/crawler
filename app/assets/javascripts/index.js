$(function() {

    // add a click handler to the button
//    $("#getMessageButton").click(function(event) {
//        // make an ajax get request to get the message
//        jsRoutes.controllers.MessageController.getMessage().ajax({
//            success: function(data) {
//                console.log(data)
//                $(".well").append($("<h1>").text(data.value))
//            }
//        })
//    })
       function RecursiveUnbind($jElement) {
        // remove this element's and all of its children's click events
        $jElement.unbind();
        $jElement.removeAttr('onclick');
        $jElement.children().each(function () {
        RecursiveUnbind($(this));
        });
        }
        RecursiveUnbind($('#mock'));
        $("#mock").load("/load");
})