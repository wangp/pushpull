%-----------------------------------------------------------------------------%

:- module openssl.
:- interface.

:- import_module io.
:- import_module maybe.

:- import_module binary_string.

:- type method
    --->    tlsv1_client_method.

:- type bio.

:- pred library_init(io::di, io::uo) is det.

:- pred setup(method::in, string::in, maybe(string)::in, maybe_error(bio)::out,
    io::di, io::uo) is det.

    % Ugly.
:- pred print_errors(io.output_stream::in, io::di, io::uo) is det.

:- pred bio_do_connect(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_do_handshake(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_free_all(bio::in, io::di, io::uo) is det.

:- pred bio_get_fd(bio::in, maybe_error(int)::out, io::di, io::uo) is det.

:- pred bio_read_byte(bio::in, io.result(int)::out, io::di, io::uo) is det.

:- pred bio_read_bytes(bio::in, int::in, io.result(binary_string)::out,
    io::di, io::uo) is det.

:- pred bio_write_string(bio::in, string::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred bio_write_binary_string(bio::in, binary_string::in, maybe_error::out,
    io::di, io::uo) is det.

:- pred bio_flush(bio::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.

:- type method_ptr.

:- pragma foreign_type("C", method_ptr, "const SSL_METHOD *").

:- pragma foreign_type("C", bio, "BIO *").

:- pragma foreign_decl("C", local, "
    #include <openssl/ssl.h>
    #include <openssl/err.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    library_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    SSL_library_init();
    SSL_load_error_strings();
").

%-----------------------------------------------------------------------------%

:- func method_ptr(method) = method_ptr.

method_ptr(tlsv1_client_method) = tlsv1_client_method.

:- func tlsv1_client_method = method_ptr.

:- pragma foreign_proc("C",
    tlsv1_client_method = (Method::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Method = TLSv1_client_method();
").

%-----------------------------------------------------------------------------%

    % XXX perform host name checking, X509_check_host in OpenSSL 1.1
setup(Method, Host, MaybeCertificateFile, Res, !IO) :-
    (
        MaybeCertificateFile = yes(CertificateFileOrEmpty)
    ;
        MaybeCertificateFile = no,
        CertificateFileOrEmpty = ""
    ),
    setup_2(method_ptr(Method), Host, CertificateFileOrEmpty, Ok, Bio, Error,
        !IO),
    (
        Ok = yes,
        Res = ok(Bio)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred setup_2(method_ptr::in, string::in, string::in, bool::out, bio::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setup_2(Method::in, Host::in, CertificateFile::in, Ok::out, Bio::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Error = MR_make_string_const("""");
    Bio = do_setup(Method, Host, CertificateFile, &Error);
    Ok = (Bio != NULL) ? MR_YES : MR_NO;
").

:- pragma foreign_decl("C", local, "
static BIO *
do_setup(const SSL_METHOD *method, const char *host,
    const char *certificate_file, MR_String *error)
{
    SSL_CTX *ctx;
    SSL *ssl;
    BIO *bio;

    ctx = SSL_CTX_new(method);
    if (ctx == NULL) {
        *error = MR_make_string_const(""SSL_CTX_new failed"");
        return NULL;
    }

    if (strlen(certificate_file) > 0 &&
        SSL_CTX_load_verify_locations(ctx, certificate_file, NULL) != 1)
    {
        SSL_CTX_free(ctx);
        *error = MR_make_string_const(""SSL_CTX_load_verify_locations failed"");
        return NULL;
    }

    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

    bio = BIO_new_ssl_connect(ctx);
    if (bio == NULL) {
        SSL_CTX_free(ctx);
        *error = MR_make_string_const(""BIO_new_ssl_connect failed"");
        return NULL;
    }

    BIO_get_ssl(bio, &ssl);
    if (ssl == NULL) {
        BIO_free_all(bio);
        *error = MR_make_string_const(""BIO_get_ssl failed"");
        return NULL;
    }

	/* Don't want any retries. */
	SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);

    BIO_set_conn_hostname(bio, host);

    return bio;
}
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    print_errors(Stream::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    ERR_print_errors_fp(MR_file(* (MercuryFile *) Stream));
").

%-----------------------------------------------------------------------------%

bio_do_connect(Bio, Res, !IO) :-
    bio_do_connect_2(Bio, RC, !IO),
    ( RC =< 0 ->
        % We should not have to retry.
        Res = error("error connecting to server")
    ;
        Res = ok
    ).

:- pred bio_do_connect_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_do_connect_2(Sbio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = BIO_do_connect(Sbio);
").

%-----------------------------------------------------------------------------%

bio_do_handshake(Bio, Res, !IO) :-
    bio_do_handshake_2(Bio, RC, !IO),
    ( RC =< 0 ->
        Res = error("error establishing SSL connection")
    ;
        check_peer_certificate(Bio, Res, !IO)
    ).

:- pred bio_do_handshake_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_do_handshake_2(Bio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = BIO_do_handshake(Bio);
").

%-----------------------------------------------------------------------------%

    % This is probably not necessary because we set SSL_VERIFY_PEER
    % but it doesn't hurt to check?
    %
:- pred check_peer_certificate(bio::in, maybe_error::out, io::di, io::uo)
    is det.

check_peer_certificate(Bio, Res, !IO) :-
    check_peer_certificate_2(Bio, Ok, !IO),
    (
        Ok = yes,
        Res = ok
    ;
        Ok = no,
        Res = error("failed to verify peer certificate")
    ).

:- pred check_peer_certificate_2(bio::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    check_peer_certificate_2(Bio::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    SSL *ssl;
    X509 *peer_cert;

    BIO_get_ssl(Bio, &ssl);
    if (ssl == NULL) {
        Ok = MR_NO;
    } else {
        peer_cert = SSL_get_peer_certificate(ssl);
        if (peer_cert != NULL && SSL_get_verify_result(ssl) == X509_V_OK) {
            Ok = MR_YES;
        } else {
            Ok = MR_NO;
        }
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    bio_free_all(Bio::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    BIO_free_all(Bio);
").

%-----------------------------------------------------------------------------%

bio_get_fd(Bio, Res, !IO) :-
    bio_get_fd_2(Bio, Fd, !IO),
    ( Fd = -1 ->
        Res = error("BIO not initialised")
    ;
        Res = ok(Fd)
    ).

:- pred bio_get_fd_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_get_fd_2(Bio::in, Fd::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Fd = BIO_get_fd(Bio, NULL);
").

%-----------------------------------------------------------------------------%

bio_read_byte(Bio, Res, !IO) :-
    bio_read_byte_2(Bio, NumRead, Byte, !IO),
    ( NumRead > 0 ->
        Res = ok(Byte)
    ; NumRead = -2 ->
        Res = error(io.make_io_error("read not implemented for this BIO type"))
    ;
        % We should not have to retry.
        Res = eof
    ).

:- pred bio_read_byte_2(bio::in, int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_read_byte_2(Bio::in, NumRead::out, Byte::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    unsigned char buf[1];

    NumRead = BIO_read(Bio, buf, sizeof(buf));
    Byte = buf[0];
").

%-----------------------------------------------------------------------------%

bio_read_bytes(Bio, NumOctets, Res, !IO) :-
    bio_read_bytes_2(Bio, NumOctets, Error, NumRead, BinaryString, !IO),
    ( Error = 0 ->
        ( NumRead = NumOctets ->
            Res = ok(BinaryString)
        ;
            string.format("read %d bytes but expected %d bytes",
                [i(NumRead), i(NumOctets)], Message),
            Res = error(io.make_io_error(Message))
        )
    ; Error = -2 ->
        Res = error(io.make_io_error("read not implemented for this BIO type"))
    ;
        Res = eof
    ).

:- pred bio_read_bytes_2(bio::in, int::in, int::out, int::out,
    binary_string::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_read_bytes_2(Bio::in, NumOctets::in, Error::out, NumRead::out,
        BinaryString::uo, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    unsigned char *data;

    data = allocate_binary_string_buffer(NumOctets, MR_ALLOC_ID);
    NumRead = 0;
    Error = 0;

    while (NumRead < NumOctets) {
        const int remaining = NumOctets - NumRead;
        const int x = BIO_read(Bio, data + NumRead, NumOctets - NumRead);

        /* We set SSL_MODE_AUTO_RETRY so we should not need to retry. */
        if (x <= 0) {
            Error = x;
            break;
        }

        NumRead += x;
    }

    if (Error == 0 && NumRead == NumOctets) {
        BinaryString = make_binary_string(NumOctets, data, MR_ALLOC_ID);
    } else {
        BinaryString = NULL;
    }
").

%-----------------------------------------------------------------------------%

bio_write_string(Bio, String, Res, !IO) :-
    bio_write_binary_string(Bio, from_string(String), Res, !IO).

bio_write_binary_string(Bio, BinaryString, Res, !IO) :-
    Length = length(BinaryString),
    ( Length = 0 ->
        Res = ok
    ;
        bio_write_binary_string_2(Bio, BinaryString, NumWrite, !IO),
        ( NumWrite = Length ->
            Res = ok
        ; NumWrite > 0 ->
            Res = error("partial write")
        ;
            Res = error("error writing")
        )
    ).

:- pred bio_write_binary_string_2(bio::in, binary_string::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_write_binary_string_2(Bio::in, BinaryString::in, NumWritten::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    NumWritten = BIO_write(Bio, BinaryString->data, BinaryString->len);
").

%-----------------------------------------------------------------------------%

bio_flush(Bio, Res, !IO) :-
    bio_flush_2(Bio, RC, !IO),
    ( RC = -1 ->
        Res = error("error flushing")
    ;
        Res = ok
    ).

:- pred bio_flush_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_flush_2(Bio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    RC = BIO_flush(Bio);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
