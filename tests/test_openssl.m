%-----------------------------------------------------------------------------%

:- module test_openssl.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module openssl.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [HostNameOnly, PortString],
        string.to_int(PortString, Port)
    then
        openssl.library_init(!IO),
        MaybeCertificateFile = yes("/etc/ssl/certs/ca-certificates.crt"),
        openssl.setup(HostNameOnly, Port, MaybeCertificateFile, ResSetup, !IO),
        (
            ResSetup = ok(Bio),
            io.write_string("connect...\n", !IO),
            bio_do_connect(Bio, ResConnect, !IO),
            (
                ResConnect = ok,
                io.write_string("handshake...\n", !IO),
                bio_do_handshake(Bio, ResHandshake, !IO),
                (
                    ResHandshake = ok,
                    io.write_string("connected\n", !IO),
                    send_get(Bio, HostNameOnly, ResSend, !IO),
                    (
                        ResSend = ok,
                        read_bytes(Bio, !IO)
                    ;
                        ResSend = error(Error),
                        report_error("bio_write_string", Error, !IO)
                    )
                ;
                    ResHandshake = error(Error),
                    report_error("bio_do_handshake", Error, !IO)
                )
            ;
                ResConnect = error(Error),
                report_error("bio_do_connect", Error, !IO)
            ),
            bio_destroy(Bio, !IO)
        ;
            ResSetup = error(Error),
            report_error("openssl.setup", Error, !IO)
        )
    else
        io.write_string("requires host and port (usually 443)\n", !IO)
    ).

:- pred send_get(bio::in, string::in, maybe_error::out, io::di, io::uo) is det.

send_get(Bio, HostNameOnly, Res, !IO) :-
    Request =
        "GET / HTTP/1.1\r\n" ++
        "Host: "++HostNameOnly++"\r\n" ++
        "Connection: close\r\n\r\n",
    bio_write_string(Bio, Request, Res, !IO).

:- pred read_bytes(bio::in, io::di, io::uo) is det.

read_bytes(Bio, !IO) :-
    bio_read_byte(Bio, ReadRes, !IO),
    (
        ReadRes = ok(Byte),
        io.write_byte(Byte, !IO),
        read_bytes(Bio, !IO)
    ;
        ReadRes = eof
    ;
        ReadRes = error(Error),
        report_error("bio_read_byte", io.error_message(Error), !IO)
    ).

:- pred report_error(string::in, string::in, io::di, io::uo) is det.

report_error(Context, Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Context, !IO),
    io.write_string(Stream, ": ", !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
