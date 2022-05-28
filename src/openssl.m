% pushpull - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module openssl.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module binary_string.

:- type bio.

:- type certificate_names
    --->    certificate_names(
                % CN field.
                common_name :: string,
                % SubjectAltNames extension; preferred.
                dns_names :: list(string)
            ).

:- pred library_init(io::di, io::uo) is det.

:- pred setup(string::in, int::in, maybe(string)::in, maybe_error(bio)::out,
    io::di, io::uo) is det.

:- pred bio_do_connect(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_do_handshake(bio::in, maybe_error::out, io::di, io::uo) is det.

:- pred bio_destroy(bio::in, io::di, io::uo) is det.

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
:- import_module string.

:- pragma foreign_type("C", bio, "BIO *").

:- pragma foreign_decl("C", local, "
    #include <openssl/ssl.h>
    #include <openssl/err.h>
    #include <openssl/x509v3.h>

    /* OpenSSL likes to use uninitialised memory. */
#if 0
    #include <valgrind/memcheck.h>
    #define VALGRIND_WORKAROUND_OPENSSL
#endif
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

setup(HostNameOnly, Port, MaybeCertificateFile, Res, !IO) :-
    (
        MaybeCertificateFile = yes(CertificateFileOrEmpty)
    ;
        MaybeCertificateFile = no,
        CertificateFileOrEmpty = ""
    ),
    HostAndPort = HostNameOnly ++ ":" ++ from_int(Port),
    setup_2(HostNameOnly, HostAndPort, CertificateFileOrEmpty, Ok, Bio, Error,
        !IO),
    (
        Ok = yes,
        Res = ok(Bio)
    ;
        Ok = no,
        make_error_message(Error, Message, !IO),
        Res = error(Message)
    ).

:- pred setup_2(string::in, string::in, string::in,
    bool::out, bio::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    setup_2(HostNameOnly::in, HostAndPort::in, CertificateFile::in,
        Ok::out, Bio::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Error = MR_make_string_const("""");
    Bio = do_setup(HostNameOnly, HostAndPort, CertificateFile, &Error);
    Ok = (Bio != NULL) ? MR_YES : MR_NO;
").

:- pragma foreign_decl("C", local, "
static BIO *
do_setup(const char *host_name_only, const char *host_and_port,
    const char *certificate_file, MR_String *error)
{
    SSL_CTX *ctx;
    X509_VERIFY_PARAM *param;
    SSL *ssl;
    BIO *bio;

    ctx = SSL_CTX_new(TLS_client_method());
    if (ctx == NULL) {
        *error = MR_make_string_const(""SSL_CTX_new failed"");
        return NULL;
    }

    /* Exclude everything below TLSv1 */
    SSL_CTX_set_min_proto_version(ctx, TLS1_VERSION);

    if (strlen(certificate_file) > 0) {
        if (SSL_CTX_load_verify_locations(ctx, certificate_file, NULL) != 1) {
            SSL_CTX_free(ctx);
            *error = MR_make_string_const(""SSL_CTX_load_verify_locations failed"");
            return NULL;
        }
    }

    /* Enable peer verification */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
    SSL_CTX_set_verify_depth(ctx, 4);

    /* Enable automatic hostname checks */
    /* OpenSSL 1.1.0 has a simpler interface but that is not yet in LibreSSL */
    param = SSL_CTX_get0_param(ctx);
    X509_VERIFY_PARAM_set_hostflags(param, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    if (X509_VERIFY_PARAM_set1_host(param, host_name_only, 0) != 1) {
        SSL_CTX_free(ctx);
        *error = MR_make_string_const(""X509_VERIFY_PARAM_set1_host failed"");
        return NULL;
    }

    bio = BIO_new_ssl_connect(ctx);

    SSL_CTX_free(ctx); /* decrement reference count */

    if (bio == NULL) {
        *error = MR_make_string_const(""BIO_new_ssl_connect failed"");
        return NULL;
    }

    if (BIO_set_conn_hostname(bio, host_and_port) != 1) {
        BIO_free_all(bio);
        *error = MR_make_string_const(""BIO_set_conn_hostname failed"");
        return NULL;
    }

    BIO_get_ssl(bio, &ssl);
    if (ssl == NULL) {
        BIO_free_all(bio);
        *error = MR_make_string_const(""BIO_get_ssl failed"");
        return NULL;
    }

    if (SSL_set_tlsext_host_name(ssl, host_name_only) != 1) {
        BIO_free_all(bio);
        *error = MR_make_string_const(""SSL_set_tlsext_host_name failed"");
        return NULL;
    }

    /* Don't want any retries. */
    SSL_set_mode(ssl, SSL_MODE_AUTO_RETRY);

    return bio;
}
").

%-----------------------------------------------------------------------------%

bio_do_connect(Bio, Res, !IO) :-
    bio_do_connect_2(Bio, RC, !IO),
    ( RC =< 0 ->
        % We should not have to retry.
        make_error_message("error connecting to server", Message, !IO),
        Res = error(Message)
    ;
        Res = ok
    ).

:- pred bio_do_connect_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_do_connect_2(Sbio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    ERR_clear_error();
    RC = BIO_do_connect(Sbio);
").

%-----------------------------------------------------------------------------%

bio_do_handshake(Bio, Res, !IO) :-
    bio_do_handshake_2(Bio, RC, !IO),
    ( RC =< 0 ->
        make_error_message("error establishing SSL connection", Message, !IO),
        Res = error(Message)
    ;
        Res = ok
    ).

:- pred bio_do_handshake_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_do_handshake_2(Bio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    ERR_clear_error();
    RC = BIO_do_handshake(Bio);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    bio_destroy(Bio::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    BIO_ssl_shutdown(Bio);
    BIO_free_all(Bio);
    ERR_clear_error();
").

%-----------------------------------------------------------------------------%

bio_get_fd(Bio, Res, !IO) :-
    bio_get_fd_2(Bio, Fd, !IO),
    ( Fd = -1 ->
        make_error_message("BIO_get_fd failed", Message, !IO),
        Res = error(Message)
    ;
        Res = ok(Fd)
    ).

:- pred bio_get_fd_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_get_fd_2(Bio::in, Fd::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    ERR_clear_error();
    Fd = BIO_get_fd(Bio, NULL);
").

%-----------------------------------------------------------------------------%

bio_read_byte(Bio, Res, !IO) :-
    bio_read_byte_2(Bio, NumRead, Byte, !IO),
    ( NumRead > 0 ->
        Res = ok(Byte)
    ; NumRead = -2 ->
        make_error_message("BIO_read failed", Message, !IO),
        Res = error(io.make_io_error(Message))
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

    ERR_clear_error();
    NumRead = BIO_read(Bio, buf, sizeof(buf));

#ifdef VALGRIND_WORKAROUND_OPENSSL
    if (NumRead > 0) {
        VALGRIND_MAKE_MEM_DEFINED(buf, sizeof(buf));
    }
#endif

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
        make_error_message("BIO_read failed", Message, !IO),
        Res = error(io.make_io_error(Message))
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
    MR_Bool system_heap;

    data = allocate_binary_string_buffer(NumOctets, &system_heap, MR_ALLOC_ID);
    NumRead = 0;
    Error = 0;

    ERR_clear_error();

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
        #ifdef VALGRIND_WORKAROUND_OPENSSL
            VALGRIND_MAKE_MEM_DEFINED(data, NumOctets);
        #endif

        BinaryString = make_binary_string(NumOctets, data, system_heap,
            MR_ALLOC_ID);
    } else {
        if (system_heap) {
            free(data);
        }
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
        ;
            ( NumWrite > 0 ->
                make_error_message("BIO_write incomplete write", Message, !IO)
            ;
                make_error_message("BIO_write failed", Message, !IO)
            ),
            Res = error(Message)
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
    ERR_clear_error();
    NumWritten = BIO_write(Bio, BinaryString->data, BinaryString->len);
").

%-----------------------------------------------------------------------------%

bio_flush(Bio, Res, !IO) :-
    bio_flush_2(Bio, RC, !IO),
    ( RC = -1 ->
        make_error_message("BIO_flush failed", Message, !IO),
        Res = error(Message)
    ;
        Res = ok
    ).

:- pred bio_flush_2(bio::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bio_flush_2(Bio::in, RC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    ERR_clear_error();
    RC = BIO_flush(Bio);
").

%-----------------------------------------------------------------------------%

:- pred make_error_message(string::in, string::out, io::di, io::uo) is det.

make_error_message(Prefix, Message, !IO) :-
    get_error_strings(Details, !IO),
    Message = string.join_list("; ", [Prefix | Details]).

:- pred get_error_strings(list(string)::out, io::di, io::uo) is det.

get_error_strings(Errors, !IO) :-
    get_error_string(Error, !IO),
    ( Error = "" ->
        Errors = []
    ;
        get_error_strings(ErrorsTail, !IO),
        Errors = [Error | ErrorsTail] % lcmc
    ).

:- pred get_error_string(string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_error_string(String::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    unsigned long e;
    char buf[128];

    e = ERR_get_error();
    if (e == 0) {
        String = MR_make_string_const("""");
    } else {
        ERR_error_string_n(e, buf, sizeof(buf));
        MR_make_aligned_string_copy_msg(String, buf, MR_ALLOC_ID);
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
