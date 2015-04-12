% plugsink - a bidirectional IMAP/Maildir synchronisation tool.
% Copyright (C) 2015 Peter Wang

:- module path.
:- interface.

:- type dirname
    --->    dirname(dn :: string).

:- type basename
    --->    basename(bn :: string).

:- type path
    --->    path(path :: string).

:- func dirname / basename = path.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module dir.

dirname(Dir) / basename(Base) = path(Dir / Base).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
