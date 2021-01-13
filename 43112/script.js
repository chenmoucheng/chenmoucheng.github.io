show = buf => Array.from(new Uint8Array(buf)).map(x => ('00' + x.toString(16)).slice(-2)).join('');
read = str => new Uint8Array(str.match(/.{1,2}/g).map(x => parseInt(x,16)));

generate_challenges = _ => 
	window.crypto.subtle.digest('SHA-256', new TextEncoder('iso-8859-2').encode(document.getElementById('name').value))
	.then(h => {
		let e = BigInteger.parse(show(h),16);
		modular_exponentiate_inner('base1',e,'mod1','h1');
		modular_exponentiate_inner('base2',e,'mod2','h2');
		modular_exponentiate_inner('base3',e,'mod3','h3');
	});

modular_exponentiate = (base,exp,mod,res) => {
	let e = BigInteger(document.getElementById(exp).value);
	modular_exponentiate_inner(base,e,mod,res);
};

modular_exponentiate_inner = (base,e,mod,res) => {
	let b = BigInteger(document.getElementById(base).innerText);
	let m = BigInteger(document.getElementById(mod).innerText);
	document.getElementById(res).innerText = BigInteger.toString(modexp(b,e,m));
};

modexp = (b,e,m) => {
	let r = BigInteger(1);
	while (e.compare(0) > 0) {
		if (e.remainder(2).compare(1) == 0) {
			r = r.multiply(b).remainder(m);
		}
		b = b.multiply(b).remainder(m);
		e = e.divide(2);
	}
	return r;
};

