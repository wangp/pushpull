%-----------------------------------------------------------------------------%

:- module openssl.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- type method
    --->    tlsv1_client_method.

:- type bio.

:- pred library_init(io::di, io::uo) is det.

:- pred setup(method::in, string::in, maybe_error(bio)::out, io::di, io::uo)
    is det.

:- pred bio_do_connect(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_do_handshake(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_free_all(bio::in, io::di, io::uo) is det.

:- pred bio_get_fd(bio::in, maybe_error(int)::out, io::di, io::uo) is det.

:- pred bio_read_byte(bio::in, io.result(int)::out, io::di, io::uo) is det.

:- pred bio_read_bytes(bio::in, int::in, io.res(list(int))::out,
    io::di, io::uo) is det.

:- pred bio_write_string(bio::in, string::in, maybe_error::out, io::di, io::uo)
    is det.

:- pred bio_flush(bio::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module string.

:- type method_ptr.

:- pragma foreign_type("C", method_ptr, "const SSL_METHOD *").

:- pragma foreign_type("C", bio, "BIO *").

:- pragma foreign_decl("C", local, "
    #include <openssl/ssl.h>
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

setup(Method, Host, Res, !IO) :-
    setup_2(method_ptr(Method), Host, Ok, Bio, Error, !IO),
    (
        Ok = yes,
        Res = ok(Bio)
    ;
        Ok = no,
        Res = error(Error)
    ).

:- pred setup_2(method_ptr::in, string::in, bool::out, bio::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setup_2(Method::in, Host::in, Ok::out, Bio::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Error = MR_make_string_const("""");
    Bio = do_setup(Method, Host, &Error);
    Ok = (Bio != NULL) ? MR_YES : MR_NO;
").

:- pragma foreign_decl("C", local, "
static BIO *
do_setup(const SSL_METHOD *method, const char *host, MR_String *error)
{
    SSL_CTX *ctx;
    SSL *ssl;
    BIO *bio;

    ctx = SSL_CTX_new(method);
    if (ctx == NULL) {
        *error = MR_make_string_const(""SSL_CTX_new failed"");
        return NULL;
    }

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
        Res = ok
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
    read_bytes_loop(Bio, NumOctets, Res0, [], RevBytes, !IO),
    (
        Res0 = ok,
        Res = ok(reverse(RevBytes))
    ;
        Res0 = error(Error),
        Res = error(Error)
    ).

:- pred read_bytes_loop(bio::in, int::in, io.res::out,
    list(int)::in, list(int)::out, io::di, io::uo) is det.

read_bytes_loop(Bio, NumOctets, Res, !Acc, !IO) :-
    ( NumOctets =< 0 ->
        Res = ok
    ;
        bio_read_byte(Bio, Res0, !IO),
        (
            Res0 = ok(Byte),
            cons(Byte, !Acc),
            read_bytes_loop(Bio, NumOctets - 1, Res, !Acc, !IO)
        ;
            Res0 = eof,
            Res = error(io.make_io_error("unexpected eof"))
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    ).

%-----------------------------------------------------------------------------%

bio_write_string(Bio, String, Res, !IO) :-
    string.count_code_units(String, Length),
    ( Length = 0 ->
        Res = ok
    ;
        bio_write_string_unsafe(Bio, String, Length, NumWrite, !IO),
        ( NumWrite = Length ->
            Res = ok
        ; NumWrite > 0 ->
            Res = error("partial write")
        ;
            Res = error("error writing")
        )
    ).

:- pred bio_write_string_unsafe(bio::in, string::in, int::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_write_string_unsafe(Bio::in, String::in, Length::in, NumWritten::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    NumWritten = BIO_write(Bio, String, Length);
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
