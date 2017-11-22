"use strict";

var email = document.getElementsByClassName('email')[0];
var pass = document.getElementsByClassName('password')[0];

function check(callback){
	var emailToCheck = email.value;
	var passwordToCheck = pass.value;
	$.get('checkingEmail.hs', {email: emailToCheck, password: passwordToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

function onSubmit(){
	check(function(err)
	{
		if(err)
		{
			pass.value = '';
			alert.window("Неверный логин или пароль!");
		}
		else
		{
			windows.location = "ВПИСАТЬ АДРЕС ОСНОВНОЙ СТРАНИЦЫ";
		}
	}
	)
	
}