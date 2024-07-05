# docspell

## postgresql SSL
https://www.postgresql.org/docs/current/ssl-tcp.html

### localhost CA
https://gist.github.com/fntlnz/cf14feb5a46b2eda428e000157447309

Create root key:
```sh
openssl genrsa -des3 -out rootCA.key 4096
```
Use the key to create root certificate:
```sh
openssl req -x509 -new -nodes -key rootCA.key -sha256 -days 3650 -out rootCA.crt
# Country Name (2 letter code) [AU]:US
# State or Province Name (full name) [Some-State]:
# Organization Name (eg, company) [Internet Widgits Pty Ltd]:localhost
# Common Name (e.g. server FQDN or YOUR name) []:localhost
```

Create server key:
```sh
openssl genrsa -out postgresql.key 4096
```

Generate Certificate Signing Request, being sure to specify subject matter matching that of the root certificate:
```sh
openssl req -new -sha256 -key postgresql.key -subj "/C=US/ST=Some-State/O=localhost/CN=localhost" -out postgresql.csr
```

Verify the CSR:
```sh
openssl req -in postgresql.csr -noout -text
```

Generate Certificate:
```sh
openssl x509 -req -in postgresql.csr -CA rootCA.crt -CAkey rootCA.key -CAcreateserial -out postgresql.crt -days 365 -sha256
```

Verify the Certificate:
```sh
openssl x509 -in postgresql.crt -text -noout
```

### postgresql SSL

Add `localhost_ca_root_cert`, `postgresql_ssl_cert`, `postgresql_ssl_key` to
secrets:
```sh
nix-shell -p sops --run "SOPS_AGE_KEY_FILE=/home/${SUDO_USER:-$USER}/.config/sops/age/keys.txt sops modules/base-configuration/secrets.yaml"
```
Configure docspell to connect using SSL with full verification:
```
"jdbc": {
  "url": "jdbc:postgresql://localhost:5432/docspell?ssl=true&sslmode=verify-full&sslrootcert=/run/secrets/localhost_ca_root_cert",
  ...
},
```

Verify psql can connect and the connection is encrypted:
```sh
psql -U docspell "postgresql://localhost:5432/docspell?ssl=true&sslmode=require"
```
```
psql (15.4)
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, compression: off)
Type "help" for help.
```

Peek at logs to verify that docspell is behaving as expected:
```sh
journalctl -u docspell-restserver.service
```

## registration
https://docspell.org/docs/configure/registration/

Assuming `docspell.server.backend.signup.mode = "invite"`, generate a new
invitation key:
```sh
curl -X POST -d '{"password":"'$(pass show docspell/new-invite-password)'"}' "http://localhost:7880/api/v1/open/signup/newinvite"
```
Then visit http://localhost:7880/app/register and complete the signup form.

## backup/restore
https://docspell.org/docs/install/download-run/#backup-restore

Backup:
```sh
sudo -u postgres pg_dump --quote-all-identifiers --dbname=docspell > docspell_backup.sql
```

Restore:
```sh
sudo -u postgres psql docspell < docspell_backup.sql
```
