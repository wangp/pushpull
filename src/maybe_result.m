% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module maybe_result.
:- interface.

:- import_module maybe.

    % Like io.result
    %
:- type maybe_result
    --->    ok
    ;       eof
    ;       error(string).

    % Like io.result(T)
    %
:- type maybe_result(T)
    --->    ok(T)
    ;       eof
    ;       error(string).

:- inst maybe_result(I)
    --->    ok(I)
    ;       eof
    ;       error(ground).

:- inst eof_error
    --->    eof
    ;       error(ground).

:- typeclass convert(T, U) where [
    func convert(T) = U,
    mode convert(in(eof_error)) = out is det
].

:- instance convert(maybe_result, maybe_result(U)).
:- instance convert(maybe_result(T), maybe_result).
:- instance convert(maybe_result(T), maybe_result(U)).

:- func from_maybe_error(maybe_error) = maybe_result.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- instance convert(maybe_result, maybe_result(U)) where [
    convert(eof) = eof,
    convert(error(E)) = error(E)
].

:- instance convert(maybe_result(T), maybe_result) where [
    convert(eof) = eof,
    convert(error(E)) = error(E)
].

:- instance convert(maybe_result(T), maybe_result(U)) where [
    convert(eof) = eof,
    convert(error(E)) = error(E)
].

from_maybe_error(ok) = ok.
from_maybe_error(error(E)) = error(E).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
