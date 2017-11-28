"use strict";

// const email = document.getElementsByClassName('email')[0];
// const pass = document.getElementsByClassName('password')[0];

const check = (email, password, callback) => {
	// const emailToCheck = email.value;
	// const passwordToCheck = pass.value;
	$.get('http://localhost:5002/ajax', {login: email, password: password})
		.success(() => { callback('good') })
		.error(() => { callback('bad') })
}

const onSubmit = () => {
	const email = document.getElementsByClassName('email')[0].value;
	const pass  = document.getElementsByClassName('password')[0].value;
	check(email, pass, (flag) => {
		if (flag == 'good') {
			document.cookie = "user_id=".concat(email)
			window.location.pathname = "/main";
		} else {
			window.alert("Неверный логин или пароль!");
		}
	});
	
};
	/*
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
*/
