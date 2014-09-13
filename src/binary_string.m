%-----------------------------------------------------------------------------%

:- module binary_string.
:- interface.

:- import_module bitmap.

:- type binary_string. % immutable like string

:- func length(binary_string) = int.

:- pred unsafe_byte(binary_string::in, int::in, int::out) is det.

:- func unsafe_pointer(binary_string) = c_pointer.

:- func from_string(string) = binary_string.

:- func from_bitmap(bitmap) = binary_string.

:- pred sub_string_search_start(binary_string::in, binary_string::in, int::in,
    int::out) is semidet.

:- pred string_between(binary_string::in, int::in, int::in, string::out)
    is semidet.

:- pred replace_all(binary_string::in, binary_string::in, binary_string::in,
    binary_string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module require.

:- pragma foreign_type("C", binary_string, "struct binary_string *").

:- pragma foreign_decl("C", "
struct binary_string {
    MR_Integer len;
    const unsigned char *data;  /* GC or system heap */
    void *ref;  /* prevent GC if data points into another structure */
};

unsigned char *
allocate_binary_string_buffer(MR_Integer len, MR_Bool *system_heap,
    MR_AllocSiteInfoPtr alloc_id);

struct binary_string *
make_binary_string(MR_Integer len, const unsigned char *data,
    MR_Bool system_heap, MR_AllocSiteInfoPtr alloc_id);

void
binary_string_free_data(void *BinaryString, void *data);
").

:- pragma foreign_code("C", "
unsigned char *
allocate_binary_string_buffer(MR_Integer num_bytes, MR_Bool *system_heap,
    MR_AllocSiteInfoPtr alloc_id)
{
    /*
    ** Use the system malloc for larger buffers as Boehm GC doesn't deal with
    ** large objects very well (internal fragmentation).
    */

    if (num_bytes < 0x1000) {
        MR_Word ptr;

        MR_offset_incr_hp_atomic_msg(ptr, 0, MR_bytes_to_words(num_bytes),
            alloc_id, ""binary_string.binary_string/0"");
        *system_heap = MR_NO;
        return (unsigned char *) ptr;
    } else {
        unsigned char *ptr;

        ptr = malloc(num_bytes);
        *system_heap = MR_YES;
        return ptr;
    }
}

struct binary_string *
make_binary_string(MR_Integer len, const unsigned char *data,
    MR_Bool system_heap, MR_AllocSiteInfoPtr alloc_id)
{
    struct binary_string *BinaryString;

    BinaryString = MR_GC_NEW_ATTRIB(struct binary_string, alloc_id);
    BinaryString->len = len;
    BinaryString->data = data;
    BinaryString->ref = NULL;

    if (system_heap) {
        MR_GC_register_finalizer(BinaryString, binary_string_free_data, NULL);
    }

    return BinaryString;
}

void
binary_string_free_data(void *obj, void *cd)
{
    struct binary_string *BinaryString = obj;
    void *data = (void *) BinaryString->data;
    (void) cd;

    BinaryString->data = NULL;
    free(data);
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
        (const unsigned char *) String, MR_NO, MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    from_bitmap(Bitmap::in) = (BinaryString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BinaryString = make_binary_string(
        MR_bitmap_length_in_bytes(Bitmap->num_bits),
        Bitmap->elements, MR_NO, MR_ALLOC_ID);

    /* Keep a reference to the Bitmap to prevent GC. */
    BinaryString->ref = Bitmap;
").

%-----------------------------------------------------------------------------%

:- type slice
    --->    slice(binary_string, int, int).

:- pred append_slices(list(binary_string.slice)::in, binary_string::out)
    is det.

append_slices(Inputs, Output) :-
    (
        Inputs = []
    ->
        Output = from_string("")
    ;
        Inputs = [Slice],
        Slice = slice(BinaryString, 0, length(BinaryString))
    ->
        Output = BinaryString
    ;
        sum_length(Inputs, 0, BufLen),
        allocate_buffer(BufLen, Buf0, SystemHeap),
        unsafe_copy_to_buffer(Inputs, Buf0, Buf, 0),
        Output = from_buffer(BufLen, Buf, SystemHeap)
    ).

:- pred sum_length(list(binary_string.slice)::in, int::in, int::out) is det.

sum_length([], !Sum).
sum_length([Slice | Slices], Sum0, Sum) :-
    Slice = slice(_, Start, End),
    Length = End - Start,
    sum_length(Slices, Sum0 + Length, Sum).

:- pred allocate_buffer(int::in, c_pointer::uo, bool::out) is det.

:- pragma foreign_proc("C",
    allocate_buffer(Length::in, Buf::uo, SystemHeap::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buf = (MR_Word) allocate_binary_string_buffer(Length, &SystemHeap,
        MR_ALLOC_ID);
").

:- pred unsafe_copy_to_buffer(list(binary_string.slice)::in,
    c_pointer::di, c_pointer::uo, int::in) is det.

unsafe_copy_to_buffer([], !Dest, _DestOffset).
unsafe_copy_to_buffer([Slice | Slices], !Dest, DestOffset0) :-
    Slice = slice(Input, InputStart, InputEnd),
    unsafe_copy(Input, InputStart, InputEnd, !Dest, DestOffset0, DestOffset1),
    unsafe_copy_to_buffer(Slices, !Dest, DestOffset1).

:- pred unsafe_copy(binary_string::in, int::in, int::in,
    c_pointer::di, c_pointer::uo, int::in, int::out) is det.

:- pragma foreign_proc("C",
    unsafe_copy(Input::in, InputStart::in, InputEnd::in,
        Dest0::di, Dest::uo, DestOffset0::in, DestOffset::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_Integer InputLength = InputEnd - InputStart;

    memcpy(
        ((unsigned char *) Dest0) + DestOffset0,
        Input->data + InputStart, InputLength);

    Dest = Dest0;
    DestOffset = DestOffset0 + InputLength;
").

:- func from_buffer(int, c_pointer, bool) = binary_string.

:- pragma foreign_proc("C",
    from_buffer(Length::in, Data::in, SystemHeap::in) = (BinaryString::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BinaryString = make_binary_string(Length, (unsigned const char *) Data,
        SystemHeap, MR_ALLOC_ID);
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

replace_all(Str, Pat, Subst, Result) :-
    PatLength = length(Pat),
    ( PatLength = 0 ->
        sorry($module, $pred, "length(Pat) = 0")
    ;
        SubstSlice = slice(Subst, 0, length(Subst)),
        replace_all_2(Str, Pat, SubstSlice, PatLength, 0, [], RevSlices),
        list.reverse(RevSlices, Slices),
        append_slices(Slices, Result)
    ).

:- pred replace_all_2(binary_string::in, binary_string::in,
    binary_string.slice::in, int::in, int::in,
    list(binary_string.slice)::in, list(binary_string.slice)::out) is det.

replace_all_2(Str, Pat, Subst, PatLength, BeginAt, RevSlices0, RevSlices) :-
    ( sub_string_search_start(Str, Pat, BeginAt, Index) ->
        Initial = slice(Str, BeginAt, Index),
        replace_all_2(Str, Pat, Subst, PatLength, Index + PatLength,
            [Subst, Initial | RevSlices0], RevSlices)
    ;
        EndString = slice(Str, BeginAt, length(Str)),
        RevSlices = [EndString | RevSlices0]
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
