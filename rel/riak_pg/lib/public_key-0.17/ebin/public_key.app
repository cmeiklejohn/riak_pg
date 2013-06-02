{application, public_key,
  [{description, "Public key infrastructure"},
   {vsn, "0.17"},
   {modules, [	  public_key,
		  pubkey_pem,
		  pubkey_pbe,	
		  pubkey_ssh,
		  pubkey_cert,
		  pubkey_cert_records,
		  'OTP-PUB-KEY',
		  'PKCS-FRAME'
            ]},
   {applications, [crypto, kernel, stdlib]},
   {registered, []},
   {env, []}  
   ]
}.

