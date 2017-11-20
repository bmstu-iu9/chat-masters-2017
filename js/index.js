$('.emailField').focusout(function(){
	validateEmail(function(err)
	{
		if(err)
		{
			$(this).css("box-shadow", "0 0 4px 0 #f00");
			window.alert("E-mail занят!");
		}
		else
		{
			$(this).css("box-shadow", "0 0 4px 0 #0f0");
			enableSubmit();
		}
	})
});
 
function validateEmail(callback) {
    var emailToCheck = $('#emailField').val();
	$.get('checkingEmail.hs', {email: emailToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

$('.lognField').focusout(function(){
	validateLogin(function(err)
	{
		if(err)
		{
			$(this).css("box-shadow", "0 0 4px 0 #f00");
			window.alert("Выберите другое имя!");
		}
		else
		{
			$(this).css("box-shadow", "0 0 4px 0 #0f0");
			enableSubmit();
		}
	})
});
 
function validateLogin(callback) {
    var loginToCheck = $('#loginField').val();
	$.get('checkingLogin.hs', {login: loginToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

function enableSubmit() {
	if ($('.emailField').css("box-shadow") == "0 0 4px 0 #0f0" && $('.loginField').css("box-shadow") == "0 0 4px 0 #0f0") {
		$(".submitBtn").prop( "disabled", false );
	}
}