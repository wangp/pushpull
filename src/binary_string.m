%-----------------------------------------------------------------------------%

:- module binary_string.
:- interface.

:- import_module bitmap.
:- import_module list.

:- type binary_string. % immutable like string

:- func length(binary_string) = int.

:- pred unsafe_byte(binary_string::in, int::in, int::out) is det.

:- func unsafe_pointer(binary_string) = c_pointer.

:- func from_string(string) = binary_string.

:- func from_bitmap(bitmap) = binary_string.

:- pred append_list(list(binary_string)::in, binary_string::out) is det.

:- pred sub_string_search_start(binary_string::in, binary_string::in, int::in,
    int::out) is semidet.

:- pred string_between(binary_string::in, int::in, int::in, string::out)
    is semidet.

:- pred replace_all(binary_string::in, binary_string::in, binary_string::in,
    binary_string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

:- pragma foreign_type("C", binary_string, "struct binary_string *").

:- pragma foreign_decl("C", "
struct binary_string {
    MR_Integer len;
    const unsigned char *data;
    void *ref;  /* prevent GC if data points into another structure */
};

unsigned char *
allocate_binary_string_buffer(MR_Integer len, MR_AllocSiteInfoPtr alloc_id);

struct binary_string *
make_binary_string(MR_Integer len, const unsigned char *data,
    MR_AllocSiteInfoPtr alloc_id);
").

:- pragma foreign_code("C", "
unsigned char *
allocate_binary_string_buffer(MR_Integer num_bytes,
    MR_AllocSiteInfoPtr alloc_id)
{
    MR_Word ptr;

    MR_offset_incr_hp_atomic_msg(ptr, 0, MR_bytes_to_words(num_bytes),
        alloc_id, ""binary_string.binary_string/0"");
    return (unsigned char *) ptr;
}

struct binary_string *
make_binary_string(MR_Integer len, const unsigned char *data,
    MR_AllocSiteInfoPtr alloc_id)
{
    struct binary_string *BinaryString;

    BinaryString = MR_GC_NEW_ATTRIB(struct binary_string, alloc_id);
    BinaryString->len = len;
    BinaryString->data = data;
    BinaryString->ref = NULL;
    return BinaryString;
}
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    length(BinaryString::in) = (Length::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Length = BinaryString->len;
").

:- pragma foreign_proc("C",
    unsafe_byte(BinaryString::in, Index::in, Byte::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Byte = BinaryString->data[Index];
").

:- pragma foreign_proc("C",
    unsafe_pointer(BinaryString::in) = (Data::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Data = (MR_Word) BinaryString->data;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_string(String::in) = (BinaryString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BinaryString = make_binary_string(strlen(String),
        (const unsigned char *) String, MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bitmap(Bitmap::in) = (BinaryString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BinaryString = make_binary_string(
        MR_bitmap_length_in_bytes(Bitmap->num_bits),
        Bitmap->elements, MR_ALLOC_ID);

    /* Keep a reference to the Bitmap to prevent GC. */
    BinaryString->ref = Bitmap;
").

%-----------------------------------------------------------------------------%

append_list(Inputs, Output) :-
    (
        Inputs = [],
        Output = from_string("")
    ;
        Inputs = [X],
        Output = X
    ;
        Inputs = [_, _ | _],
        sum_length(Inputs, 0, BufLen),
        allocate_buffer(BufLen, Buf0),
        unsafe_copy_to_buffer(Inputs, 0, Buf0, Buf),
        Output = from_buffer(BufLen, Buf)
    ).

:- pred sum_length(list(binary_string)::in, int::in, int::out) is det.

sum_length([], !Sum).
sum_length([X | Xs], Sum0, Sum) :-
    Sum1 = Sum0 + length(X),
    sum_length(Xs, Sum1, Sum).

:- pred allocate_buffer(int::in, c_pointer::uo) is det.

:- pragma foreign_proc("C",
    allocate_buffer(Length::in, Buf::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buf = (MR_Word) allocate_binary_string_buffer(Length, MR_ALLOC_ID);
").

:- pred unsafe_copy_to_buffer(list(binary_string)::in, int::in,
    c_pointer::di, c_pointer::uo) is det.

unsafe_copy_to_buffer([], _Offset, !Buf).
unsafe_copy_to_buffer([X | Xs], Offset0, !Buf) :-
    unsafe_copy(X, Offset0, !Buf),
    unsafe_copy_to_buffer(Xs, Offset0 + length(X), !Buf).

:- pred unsafe_copy(binary_string::in, int::in, c_pointer::di, c_pointer::uo)
    is det.

:- pragma foreign_proc("C",
    unsafe_copy(Input::in, Offset::in, Buf0::di, Buf::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    unsigned char *p = (unsigned char *) Buf0;
    memcpy(p + Offset, Input->data, Input->len);
    Buf = Buf0;
").

:- func from_buffer(int, c_pointer) = binary_string.

:- pragma foreign_proc("C",
    from_buffer(Length::in, Data::in) = (BinaryString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BinaryString = make_binary_string(Length, (unsigned const char *) Data,
        MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    sub_string_search_start(Haystack::in, Needle::in, BeginAt::in, Index::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const unsigned char *match;

    if ((MR_Unsigned) BeginAt > Haystack->len) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        match = memmem(Haystack->data + BeginAt, Haystack->len - BeginAt,
            Needle->data, Needle->len);
        if (match) {
            Index = match - Haystack->data;
            SUCCESS_INDICATOR = MR_TRUE;
        } else {
            SUCCESS_INDICATOR = MR_FALSE;
        }
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    string_between(BinaryString::in, Start::in, End::in, SubString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_Integer Count;

    Count = End - Start;
    MR_allocate_aligned_string_msg(SubString, Count, MR_ALLOC_ID);
    MR_memcpy(SubString, BinaryString->data + Start, Count);
    SubString[Count] = '\\0';

    if (strlen(SubString) == Count && MR_utf8_verify(SubString)) {
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%-----------------------------------------------------------------------------%

:- func unsafe_between(binary_string::in, int::in, int::in) =
    (binary_string::uo) is det.

:- pragma foreign_proc("C",
    unsafe_between(Str::in, Start::in, End::in) = (SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_Integer Count;
    unsigned char *Buf;

    Count = End - Start;
    Buf = allocate_binary_string_buffer(Count, MR_ALLOC_ID);
    MR_memcpy(Buf, Str->data + Start, Count);
    SubString = make_binary_string(Count, Buf, MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%

replace_all(Str, Pat, Subst, Result) :-
    PatLength = length(Pat),
    ( PatLength = 0 ->
        sorry($module, $pred, "length(Pat) = 0")
    ;
        ReversedChunks = replace_all_2(Str, Pat, Subst, PatLength, 0, []),
        list.reverse(ReversedChunks, Chunks),
        append_list(Chunks, Result)
    ).

:- func replace_all_2(binary_string, binary_string, binary_string, int, int,
    list(binary_string)) = list(binary_string).

replace_all_2(Str, Pat, Subst, PatLength, BeginAt, Result0) = Result :-
    ( sub_string_search_start(Str, Pat, BeginAt, Index) ->
        Initial = unsafe_between(Str, BeginAt, Index),
        Start = Index + PatLength,
        Result = replace_all_2(Str, Pat, Subst, PatLength, Start,
            [Subst, Initial | Result0])
    ;
        EndString = unsafe_between(Str, BeginAt, length(Str)),
        Result = [EndString | Result0]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
