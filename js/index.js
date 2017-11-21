"use strict";

var email = document.getElementsByClassName('emailField')[0];
var login = document.getElementsByClassName('loginField')[0];

function checkEmail(){
	validateEmail(function(err)
	{
		if(err)
		{
			email.style.boxShadow =  "0 0 4px 0 #f00";
			window.alert("E-mail занят!");
		}
		else
		{
			email.style.boxShadow = "0 0 4px 0 #0f0";
			enableSubmit();
		}
	})
};

 
function validateEmail(callback) {
    var emailToCheck = $('#emailField').val();
	$.get('checkingEmail.hs', {email: emailToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

function checkLogin(){
	validateLogin(function(err)
	{
		if(err)
		{
			login.style.boxShadow =  "0 0 4px 0 #f00";
			window.alert("Выберите другое имя!");
		}
		else
		{
			login.style.boxShadow = "0 0 4px 0 #0f0";
			enableSubmit();
		}
	})
};
 
function validateLogin(callback) {
    var loginToCheck = $('#loginField').val();
	$.get('checkingLogin.hs', {login: loginToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

function enableSubmit() {
	if ((email.style.boxShadow == "rgb(0, 255, 0) 0px 0px 4px 0px") && (login.style.boxShadow == "rgb(0, 255, 0) 0px 0px 4px 0px")) {
		document.getElementsByClassName('submitBtn')[0].style.display = "block";
	}
}