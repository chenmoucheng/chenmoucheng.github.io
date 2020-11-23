show = buf => Array.from(new Uint8Array(buf)).map(x => ('00' + x.toString(16)).slice(-2)).join('');
read = str => new Uint8Array(str.match(/.{1,2}/g).map(x => parseInt(x,16)));

encrypt = pt => {
	let iv = window.crypto.getRandomValues(new Uint8Array(16));
	return window.crypto.subtle.importKey('raw', new Uint8Array(16), document.getElementById('mode').value, false, ['encrypt'])
		.then(key => window.crypto.subtle.encrypt({name:document.getElementById('mode').value,iv:iv,counter:iv,length:128}, key, pt))
		.then(ct => Promise.resolve(Uint8Array.from([...iv, ...new Uint8Array(ct)])));
};

decrypt = ct =>
	window.crypto.subtle.importKey('raw', new Uint8Array(16), document.getElementById('mode').value, false, ['decrypt'])
	.then(key => window.crypto.subtle.decrypt({name:document.getElementById('mode').value,iv:ct.slice(0,16),counter:ct.slice(0,16),length:128}, key, ct.slice(16)));

encode = _ => document.getElementById('plaintext_encoded'           ).value = show(new TextEncoder('iso-8859-2').encode(     document.getElementById('plaintext_to_encrypt').value));
decode = _ => document.getElementById('ciphertext_decrypted_decoded').value =      new TextDecoder('iso-8859-2').decode(read(document.getElementById('ciphertext_decrypted').value));

generate_challenge = _ => {
	var h;
	window.crypto.subtle.digest('SHA-256', new TextEncoder('iso-8859-2').encode(document.getElementById('name').value))
	.then(_h => { h = new Uint8Array(_h); return window.crypto.subtle.importKey('raw', new Uint8Array(16), document.getElementById('mode').value, false, ['encrypt']); })
	.then(key => window.crypto.subtle.encrypt({name:document.getElementById('mode').value,iv:h.slice(0,16),counter:h.slice(0,16),length:128}, key, new TextEncoder('iso-8859-2').encode(('Flag: ' + show(h.slice(-5))))))
	.then(ct => document.getElementById('challenge_ciphertext').value = show(h.slice(0,16)) + show(ct));
};

submit_answer = _ =>
	decrypt(read(document.getElementById('challenge_ciphertext').value))
	.then(pt => {
		if (new TextDecoder('iso-8859-2').decode(pt) == document.getElementById('answer').value) {
			document.getElementById('check_result').value = '勝った';
		}
		else {
			document.getElementById('check_result').value = '負けた';
		}
	});

query_encrypt_oracle = _ =>
	encrypt(read(document.getElementById('plaintext_encoded').value))
	.then(ct => document.getElementById('plaintext_encoded_encrypted').value = show(ct))
	.catch(_ => document.getElementById('plaintext_encoded_encrypted').value = '失敗');

query_decrypt_oracle = _ => {
	let ct = document.getElementById('ciphertext_to_decrypt').value;
	if (ct == document.getElementById('challenge_ciphertext').value) {
		document.getElementById('ciphertext_decrypted').value = 'ウケる';
	}
	else {
		decrypt(read(ct))
		.then(pt => document.getElementById('ciphertext_decrypted').value = show(pt))
		.catch(_ => document.getElementById('ciphertext_decrypted').value = '失敗');
	}
};

