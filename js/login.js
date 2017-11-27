"use strict";

var email = document.getElementsByClassName('email')[0];
var pass = document.getElementsByClassName('password')[0];

function check(callback){
	var emailToCheck = email.value;
	var passwordToCheck = pass.value;
	$.get('http://localhost:5002/ajax', {login: emailToCheck, password: passwordToCheck})
		.success(function() { callback(false) })
		.error(function() { callback(true) })
}

function onSubmit(){
	check(function(err)
	{
		if(err)
		{
			pass.value = '';
			window.alert("Неверный логин или пароль!");
		}
		else
		{
			document.cookie = "user_id=".concat(email.value)
			window.location = "/main";
		}
		return false;
	}
	)
	
}