







open Ctypes
open Foreign

let so_ext = if Sys.win32 || Sys.cygwin then "dll" else "so"
let c_lib_name = Format.sprintf "libturkixirlang.%s" so_ext
let c_lib = Dl.dlopen ~filename:c_lib_name ~flags:[Dl.RTLD_NOW]

exception BadTypeError of string

exception OutOfBoundsError of string

exception InvalidInput of string

exception InvalidSymbolError of string

exception InvalidUnitNameError of string

exception NativeException of string

exception PreconditionFailure of string

exception PropertyError of string

exception TemplateArgsError of string

exception TemplateFormatError of string

exception TemplateInstantiationError of string

exception StaleReferenceError of string

exception UnknownCharset of string





exception SyntaxError

module Exception = struct

  type t = {
    kind : int;
    information : string;
  }

  let c_struct : t structure typ = structure "exception"
  let kind = field c_struct "kind" int
  let information = field c_struct "information" string
  let () = seal c_struct

  let wrap c_value_ptr =
    if is_null c_value_ptr then
      None
    else
      let c_value = !@ c_value_ptr in
      Some {
        kind = getf c_value kind;
        information = getf c_value information;
      }

  let unwrap value =
    match value with
    | None ->
        from_voidp c_struct null
    | Some value ->
        let c_value = make c_struct in
        setf c_value kind value.kind;
        setf c_value information value.information;
        allocate c_struct c_value

  let c_type = view (ptr c_struct) ~read:wrap ~write:unwrap

end

let get_last_exception = foreign ~from:c_lib
  "turkixir_get_last_exception"
  (void @-> returning Exception.c_type)

(* When declaring an imported function with foreign, use raisable instead of
 returning, to check get_last_exception before returning *)
let raisable typ =
  let read value =
    match get_last_exception () with
    | None -> value
    | Some exc ->
        (match exc.kind with
         | 0 ->
             raise (BadTypeError exc.information)
         | 1 ->
             raise (OutOfBoundsError exc.information)
         | 2 ->
             raise (InvalidInput exc.information)
         | 3 ->
             raise (InvalidSymbolError exc.information)
         | 4 ->
             raise (InvalidUnitNameError exc.information)
         | 5 ->
             raise (NativeException exc.information)
         | 6 ->
             raise (PreconditionFailure exc.information)
         | 7 ->
             raise (PropertyError exc.information)
         | 8 ->
             raise (TemplateArgsError exc.information)
         | 9 ->
             raise (TemplateFormatError exc.information)
         | 10 ->
             raise (TemplateInstantiationError exc.information)
         | 11 ->
             raise (StaleReferenceError exc.information)
         | 12 ->
             raise (UnknownCharset exc.information)
         | _ -> assert false)
  in
  let write value = value in
  let new_typ = view typ ~read ~write in
  returning new_typ

(* Module used to encode/decode UTF32 strings *)

(* Camomile needs to know the location of its standard library to work,
   so we use the following heuristic:
   - if the directory chosen at build time exists, we assume the installation
     is ok
   - otherwise we look for a directory 'share/camomile' next to the binary
   - otherwise we fail
*)

module type CamomileConfig = module type of CamomileLibrary.DefaultConfig

module CamomileDefaultConfig : CamomileConfig = CamomileLibrary.DefaultConfig

let ( ^/ ) = Filename.concat

let build_camomile_config root_path = (module struct
  let share_dir = root_path ^/ "share" ^/ "camomile"

  let datadir = share_dir ^/ "database"

  let localedir = share_dir ^/ "locales"

  let charmapdir = share_dir ^/ "charmaps"

  let unimapdir = share_dir ^/ "mappings"

  end : CamomileConfig)

module CamomileShareConfig =
  (val build_camomile_config
    (Filename.dirname Sys.executable_name ^/ Filename.parent_dir_name)
    : CamomileConfig)

(* In case we are building through an opam-installed env, find
   Camomile's stdlib through the appropriate opam env variable *)
module CamomileOpamConfig =
  (val
    let opam_dir = try Sys.getenv "OPAM_SWITCH_PREFIX" with _ -> "DUMMY" in
    build_camomile_config opam_dir : CamomileConfig)

let camomile_config =
  if Sys.file_exists CamomileDefaultConfig.datadir then
    (module CamomileDefaultConfig : CamomileConfig )
  else if Sys.file_exists CamomileShareConfig.datadir then
    (module CamomileShareConfig : CamomileConfig )
  else if Sys.file_exists CamomileOpamConfig.datadir then
    (module CamomileOpamConfig : CamomileConfig)
  else failwith "no camomile library found"

module CamomileConfig = (val camomile_config)

module Camomile = CamomileLibrary.Make (CamomileConfig)

module Text = struct
  type t = string

  let c_struct : t structure typ = structure "text"

  let chars = field c_struct "chars" (ptr uint32_t)

  let length = field c_struct "length" size_t

  let is_allocated = field c_struct "is_allocated" bool

  let () = seal c_struct

  let destroy_text = foreign ~from:c_lib "turkixir_destroy_text"
    (ptr c_struct @-> raisable void)

  module UCS4Encoding = Camomile.CharEncoding.Make (Camomile.UCS4)

  let wrap (c_value : t structure) : t =
    let open Unsigned.Size_t in
    let open Camomile in
    let length = to_int (getf c_value length) in
    let chars = getf c_value chars in
    let f i =
      UChar.chr_of_uint (Unsigned.UInt32.to_int !@ (chars +@ i))
    in
    let result = UCS4.init length f in
    (* Now that the value is fully transformed to an ocaml value, we can
      free it by calling destroy_text *)
    destroy_text (addr c_value) ;
    UCS4Encoding.encode CharEncoding.utf8 result

  let unwrap (value : t) : t structure =
    let open Unsigned in
    let open Camomile in
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let struct_length = Size_t.of_int (UCS4.length text) in
    let struct_chars = allocate_n uint32_t ~count:(UCS4.length text) in
    let i = ref 0 in
    let f c =
      struct_chars +@ !i <-@ (UInt32.of_int (UChar.code c));
      i := !i + 1
    in
    UCS4.iter f text ;
    let c_value = make c_struct in
    setf c_value length struct_length ;
    setf c_value chars struct_chars ;
    setf c_value is_allocated false ;
    (* We don't need to care about calling destroy_text here since we
     manually allocated the pointer, ctypes will take care of freeing the
     memory *)
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module Character = struct
  (* Characters are encoded as strings because ocaml char are not unicode
   characters *)
  type t = string

  module UCharEncoding = Camomile.CharEncoding.Make (Camomile.UText)

  let of_int i =
    let open Camomile in
    let uchar = UChar.chr i in
    UCharEncoding.encode CharEncoding.utf8 (UText.init 1 (fun _ -> uchar))

  let of_int32 i =
    of_int (Unsigned.UInt32.to_int i)

  let wrap (c_value : Unsigned.UInt32.t) : t =
    of_int32 c_value

  let unwrap (value : string) : Unsigned.UInt32.t =
    let open Camomile in
    let text = UCharEncoding.decode CharEncoding.utf8 value in
    let uchar = UText.get text 0 in
    Unsigned.UInt32.of_int (UChar.code uchar)

  let c_type = view uint32_t ~read:wrap ~write:unwrap
end

module StringType = struct
  type t = string

  let c_struct : t structure typ = structure "string"
  let length_field = field c_struct "length" int
  let _ = field c_struct "ref_count" int
  (* Langkit strings are encoded in UTF-32 (native endianity). *)
  let content_field = field c_struct "content" uint32_t
  let () = seal c_struct

  let buffer_ptr_type = ptr uint32_t
  let c_type = ptr c_struct

  let create = foreign ~from:c_lib "turkixir_create_string"
    (buffer_ptr_type @-> int @-> raisable c_type)
  let dec_ref = foreign ~from:c_lib "turkixir_string_dec_ref"
    (c_type @-> raisable void)

  module UCharEncoding = Camomile.CharEncoding.Make (Camomile.UText)

  let wrap c_value_ptr =
    let open Text in
    let open Camomile in
    let c_value = !@ c_value_ptr in
    let length = getf c_value length_field in
    let content = c_value @. content_field in
    (* We use Camomile to encode utf32 strings to an ocaml string *)
    let f i = UChar.chr_of_uint (Unsigned.UInt32.to_int !@(content +@ i)) in
    let result =
      UCS4Encoding.encode CharEncoding.utf8 (UCS4.init length f)
    in
    dec_ref c_value_ptr;
    result

  let unwrap value =
    let open Text in
    let open Camomile in

    (* Create a buffer to contain the UTF-32 encoded string. *)
    let text = UCS4Encoding.decode CharEncoding.utf8 value in
    let length = UCS4.length text in
    let buffer = allocate_n uint32_t ~count:length in
    let i = ref 0 in
    let f c =
      buffer +@ !i <-@ (Unsigned.UInt32.of_int (UChar.code c));
      i := !i + 1
    in
    UCS4.iter f text ;

    (* ctypes is supposed to take care of freeing "buffer" before returning. *)
    create buffer length
end

module BigInteger = struct

  type t = Z.t

  let c_type = ptr void

  let create = foreign ~from:c_lib "turkixir_create_big_integer"
    (ptr Text.c_type @-> raisable c_type)

  let text = foreign ~from:c_lib "turkixir_big_integer_text"
    (c_type @-> ptr Text.c_type @-> raisable void)

  let decref = foreign ~from:c_lib "turkixir_big_integer_decref"
    (c_type @-> raisable void)

  let wrap (c_value : unit ptr) : t =
    let c_text_ptr = allocate_n Text.c_type ~count:1 in
    text c_value c_text_ptr;
    decref c_value;
    Z.of_string (!@ c_text_ptr)

  let unwrap (value : t) : unit ptr =
    create (allocate Text.c_type (Z.to_string value))
end

module AnalysisUnitKind = struct
  type t =
  | UnitSpecification
  | UnitBody

  let name () = "AnalysisUnitKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> UnitSpecification
    | 1 -> UnitBody
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | UnitSpecification -> 0
    | UnitBody -> 1

   let c_type = view int ~read:wrap ~write:unwrap
end

module LookupKind = struct
  type t =
  | Recursive
  | Flat
  | Minimal

  let name () = "LookupKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> Recursive
    | 1 -> Flat
    | 2 -> Minimal
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | Recursive -> 0
    | Flat -> 1
    | Minimal -> 2

   let c_type = view int ~read:wrap ~write:unwrap
end

module DesignatedEnvKind = struct
  type t =
  | None
  | CurrentEnv
  | NamedEnv
  | DirectEnv

  let name () = "DesignatedEnvKind"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> None
    | 1 -> CurrentEnv
    | 2 -> NamedEnv
    | 3 -> DirectEnv
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | None -> 0
    | CurrentEnv -> 1
    | NamedEnv -> 2
    | DirectEnv -> 3

   let c_type = view int ~read:wrap ~write:unwrap
end

module GrammarRule = struct
  type t =
  | NameRule
  | NumberRule
  | StringRule
  | CatStringRule
  | NlRule
  | MainRuleRule
  | DecoratorRule
  | DecoratorsRule
  | DecoratedRule
  | FuncDefRule
  | ParametersRule
  | VarargslistRule
  | FpdefRule
  | NameListRule
  | StmtRule
  | SimpleStmtRule
  | SmallStmtRule
  | ExprStmtRule
  | PrintStmtRule
  | DelStmtRule
  | PassStmtRule
  | FlowStmtRule
  | BreakStmtRule
  | ContinueStmtRule
  | ReturnStmtRule
  | YieldStmtRule
  | RaiseStmtRule
  | ImportStmtRule
  | ImportNameRule
  | DotRule
  | ImportFromRule
  | AsNameRule
  | DottedAsNameRule
  | ImportAsNamesRule
  | DottedAsNamesRule
  | DottedNameRule
  | GlobalStmtRule
  | ExecStmtRule
  | AssertStmtRule
  | CompoundStmtRule
  | ElsePartRule
  | IfStmtRule
  | WhileStmtRule
  | ForStmtRule
  | TryStmtRule
  | WithStmtRule
  | WithItemRule
  | SuiteRule
  | TestRule
  | OrTestRule
  | AndTestRule
  | NotTestRule
  | ComparisonRule
  | ExprRule
  | XorExprRule
  | AndExprRule
  | ShiftExprRule
  | ArithExprRule
  | TermRule
  | FactorRule
  | PowerRule
  | AtomExprRule
  | DictAssocRule
  | YieldExprRule
  | AtomRule
  | SetLitRule
  | LambdefRule
  | SubscriptListRule
  | SubscriptRule
  | ExprListRule
  | TestListRule
  | EmptyTestListRule
  | ClassDefRule
  | ArgListRule
  | ListIterRule
  | ListForRule
  | ListIfRule
  | CompIterRule
  | CompForRule
  | CompIfRule

  let name () = "GrammarRule"

  let wrap (c_value : int) : t =
    match c_value with
    | 0 -> NameRule
    | 1 -> NumberRule
    | 2 -> StringRule
    | 3 -> CatStringRule
    | 4 -> NlRule
    | 5 -> MainRuleRule
    | 6 -> DecoratorRule
    | 7 -> DecoratorsRule
    | 8 -> DecoratedRule
    | 9 -> FuncDefRule
    | 10 -> ParametersRule
    | 11 -> VarargslistRule
    | 12 -> FpdefRule
    | 13 -> NameListRule
    | 14 -> StmtRule
    | 15 -> SimpleStmtRule
    | 16 -> SmallStmtRule
    | 17 -> ExprStmtRule
    | 18 -> PrintStmtRule
    | 19 -> DelStmtRule
    | 20 -> PassStmtRule
    | 21 -> FlowStmtRule
    | 22 -> BreakStmtRule
    | 23 -> ContinueStmtRule
    | 24 -> ReturnStmtRule
    | 25 -> YieldStmtRule
    | 26 -> RaiseStmtRule
    | 27 -> ImportStmtRule
    | 28 -> ImportNameRule
    | 29 -> DotRule
    | 30 -> ImportFromRule
    | 31 -> AsNameRule
    | 32 -> DottedAsNameRule
    | 33 -> ImportAsNamesRule
    | 34 -> DottedAsNamesRule
    | 35 -> DottedNameRule
    | 36 -> GlobalStmtRule
    | 37 -> ExecStmtRule
    | 38 -> AssertStmtRule
    | 39 -> CompoundStmtRule
    | 40 -> ElsePartRule
    | 41 -> IfStmtRule
    | 42 -> WhileStmtRule
    | 43 -> ForStmtRule
    | 44 -> TryStmtRule
    | 45 -> WithStmtRule
    | 46 -> WithItemRule
    | 47 -> SuiteRule
    | 48 -> TestRule
    | 49 -> OrTestRule
    | 50 -> AndTestRule
    | 51 -> NotTestRule
    | 52 -> ComparisonRule
    | 53 -> ExprRule
    | 54 -> XorExprRule
    | 55 -> AndExprRule
    | 56 -> ShiftExprRule
    | 57 -> ArithExprRule
    | 58 -> TermRule
    | 59 -> FactorRule
    | 60 -> PowerRule
    | 61 -> AtomExprRule
    | 62 -> DictAssocRule
    | 63 -> YieldExprRule
    | 64 -> AtomRule
    | 65 -> SetLitRule
    | 66 -> LambdefRule
    | 67 -> SubscriptListRule
    | 68 -> SubscriptRule
    | 69 -> ExprListRule
    | 70 -> TestListRule
    | 71 -> EmptyTestListRule
    | 72 -> ClassDefRule
    | 73 -> ArgListRule
    | 74 -> ListIterRule
    | 75 -> ListForRule
    | 76 -> ListIfRule
    | 77 -> CompIterRule
    | 78 -> CompForRule
    | 79 -> CompIfRule
    | _ -> assert false

  let unwrap (value : t) : int =
    match value with
    | NameRule -> 0
    | NumberRule -> 1
    | StringRule -> 2
    | CatStringRule -> 3
    | NlRule -> 4
    | MainRuleRule -> 5
    | DecoratorRule -> 6
    | DecoratorsRule -> 7
    | DecoratedRule -> 8
    | FuncDefRule -> 9
    | ParametersRule -> 10
    | VarargslistRule -> 11
    | FpdefRule -> 12
    | NameListRule -> 13
    | StmtRule -> 14
    | SimpleStmtRule -> 15
    | SmallStmtRule -> 16
    | ExprStmtRule -> 17
    | PrintStmtRule -> 18
    | DelStmtRule -> 19
    | PassStmtRule -> 20
    | FlowStmtRule -> 21
    | BreakStmtRule -> 22
    | ContinueStmtRule -> 23
    | ReturnStmtRule -> 24
    | YieldStmtRule -> 25
    | RaiseStmtRule -> 26
    | ImportStmtRule -> 27
    | ImportNameRule -> 28
    | DotRule -> 29
    | ImportFromRule -> 30
    | AsNameRule -> 31
    | DottedAsNameRule -> 32
    | ImportAsNamesRule -> 33
    | DottedAsNamesRule -> 34
    | DottedNameRule -> 35
    | GlobalStmtRule -> 36
    | ExecStmtRule -> 37
    | AssertStmtRule -> 38
    | CompoundStmtRule -> 39
    | ElsePartRule -> 40
    | IfStmtRule -> 41
    | WhileStmtRule -> 42
    | ForStmtRule -> 43
    | TryStmtRule -> 44
    | WithStmtRule -> 45
    | WithItemRule -> 46
    | SuiteRule -> 47
    | TestRule -> 48
    | OrTestRule -> 49
    | AndTestRule -> 50
    | NotTestRule -> 51
    | ComparisonRule -> 52
    | ExprRule -> 53
    | XorExprRule -> 54
    | AndExprRule -> 55
    | ShiftExprRule -> 56
    | ArithExprRule -> 57
    | TermRule -> 58
    | FactorRule -> 59
    | PowerRule -> 60
    | AtomExprRule -> 61
    | DictAssocRule -> 62
    | YieldExprRule -> 63
    | AtomRule -> 64
    | SetLitRule -> 65
    | LambdefRule -> 66
    | SubscriptListRule -> 67
    | SubscriptRule -> 68
    | ExprListRule -> 69
    | TestListRule -> 70
    | EmptyTestListRule -> 71
    | ClassDefRule -> 72
    | ArgListRule -> 73
    | ListIterRule -> 74
    | ListForRule -> 75
    | ListIfRule -> 76
    | CompIterRule -> 77
    | CompForRule -> 78
    | CompIfRule -> 79

   let c_type = view int ~read:wrap ~write:unwrap
end


let free = foreign ~from:c_lib
  "turkixir_free"
  (ptr void @-> returning void)

(** Assuming char_ptr is a valid char*, convert it to a native Ocaml
  * string and free the C pointer.
  *)
let unwrap_str char_ptr =
  let str = Ctypes.coerce (ptr char) string char_ptr in
  free (Ctypes.coerce (ptr char) (ptr void) char_ptr);
  str


let default_grammar_rule = GrammarRule.MainRuleRule

module Sloc = struct
  type t = {
    line : int;
    column : int;
  }

  let c_struct : t structure typ = structure "sloc"
  let line = field c_struct "line" uint32_t
  let column = field c_struct "column" uint16_t
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    line = Unsigned.UInt32.to_int (getf c_value line);
    column = Unsigned.UInt16.to_int (getf c_value column);
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value line (Unsigned.UInt32.of_int (value.line));
    setf c_value column (Unsigned.UInt16.of_int (value.column));
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module SlocRange = struct
  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  let c_struct : t structure typ = structure "sloc_range"
  let loc_start = field c_struct "loc_start" Sloc.c_type
  let loc_end = field c_struct "loc_end" Sloc.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    loc_start = getf c_value loc_start;
    loc_end = getf c_value loc_end;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value loc_start value.loc_start;
    setf c_value loc_end value.loc_end;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap

  let pp fmt sloc_range =
    Format.fprintf fmt "<SlocRange %d:%d-%d:%d>"
      sloc_range.loc_start.line
      sloc_range.loc_start.column
      sloc_range.loc_end.line
      sloc_range.loc_end.column
end

module Diagnostic = struct
  type t = {
    sloc_range : SlocRange.t;
    message : string
  }

  let c_struct : t structure typ = structure "diagnostic"
  let sloc_range = field c_struct "sloc_range" SlocRange.c_type
  let message = field c_struct "message" Text.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    sloc_range = getf c_value sloc_range;
    message = getf c_value message;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value sloc_range value.sloc_range;
    setf c_value message value.message;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap
end

module TokenData = struct
  type t = unit ptr
end

module Token = struct
  (* We don't have access to AnalysisContextStruct at this point. We don't need
     to do anything with the context value except pass it around, so map it as
     an opaque pointer instead. *)
  type dummy_context = unit ptr

  type t = {
    context : dummy_context;
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  let c_struct : t structure typ = structure "token"
  let context = field c_struct "context" (ptr void)
  let token_data = field c_struct "token_data" (ptr void)
  let token_index = field c_struct "token_index" int
  let trivia_index = field c_struct "trivia_index" int
  let kind = field c_struct "kind" int
  let text = field c_struct "text" Text.c_type
  let sloc_range = field c_struct "sloc_range" SlocRange.c_type
  let () = seal c_struct

  let wrap (c_value : t structure) : t = {
    context = getf c_value context;
    token_data = getf c_value token_data;
    token_index = getf c_value token_index;
    trivia_index = getf c_value trivia_index;
    kind = getf c_value kind;
    text = getf c_value text;
    sloc_range = getf c_value sloc_range;
  }

  let unwrap (value : t) : t structure =
    let c_value = make c_struct in
    setf c_value context value.context;
    setf c_value token_data value.token_data;
    setf c_value token_index value.token_index;
    setf c_value trivia_index value.trivia_index;
    setf c_value kind value.kind;
    setf c_value text value.text;
    setf c_value sloc_range value.sloc_range;
    c_value

  let c_type = view c_struct ~read:wrap ~write:unwrap

  let _token_kind_name = foreign ~from:c_lib
    "turkixir_token_kind_name"
    (int @-> raisable (ptr char))

  let token_kind_name kind =
    unwrap_str (_token_kind_name kind)

  let kind_name token = token_kind_name token.kind

  let token_range_text = foreign ~from:c_lib
    "turkixir_token_range_text"
    (ptr c_type @-> ptr c_type @-> ptr Text.c_type @-> raisable int)

  let token_next = foreign ~from:c_lib
    "turkixir_token_next"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let token_previous = foreign ~from:c_lib
    "turkixir_token_previous"
    (ptr c_type @-> ptr c_type @-> raisable void)

  let is_equivalent = foreign ~from:c_lib
    "turkixir_token_is_equivalent"
    (ptr c_type @-> ptr c_type @-> raisable bool)

  let pp fmt token =
    let pp_text fmt = function
      | "" -> Format.pp_print_string fmt ""
      | _ as text -> Format.fprintf fmt " %S" text
    in
    Format.fprintf fmt "<Token %s%a at %a>"
      (kind_name token)
      pp_text token.text
      SlocRange.pp token.sloc_range

  let text_range token_first token_last =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    let res =
      token_range_text
        (allocate c_type token_first)
        (allocate c_type token_last)
        c_result_ptr
    in
    if res = 0 then
      raise (Invalid_argument
        (Format.asprintf "%a and %a come from different units"
          pp token_first
          pp token_last));
    !@ c_result_ptr

  let next token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_next (allocate c_type token) c_next_token_ptr ;
    !@ c_next_token_ptr

  let previous token =
    let c_next_token_ptr = allocate_n c_type ~count:1 in
    token_previous (allocate c_type token) c_next_token_ptr ;
    !@ c_next_token_ptr

  let is_trivia token =
    token.trivia_index != 0

  let index token =
    match token.trivia_index with
    | 0 ->
        token.token_index - 1
    | _ ->
        token.trivia_index - 1

  let compare one other =
    let open Stdlib in
    let compare_token_data = compare one.token_data other.token_data in
    if compare_token_data = 0 then
      let compare_token_index = compare one.token_index other.token_index in
      if compare_token_index = 0 then
        compare one.trivia_index other.trivia_index
      else
        compare_token_index
    else
      compare_token_data

  let equal one other =
    compare one other = 0

  let hash token =
    Hashtbl.hash
      (token.token_data
       , token.token_index
       , token.trivia_index)

  let is_equivalent one other =
    is_equivalent (allocate c_type one) (allocate c_type other)

end

module UnitProvider = struct
  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it. *)
  type t = unit ptr ptr

  let c_type = ptr void

  let null = allocate c_type null

  

end

module BareNode = struct
  type t = unit ptr
end

module Rebindings = struct
  type t = unit ptr
end



      
module EntityInfoStruct = struct
  type t

   
  let c_type : t structure typ = structure "entity_info"
  let rebindings =
    field c_type "rebindings" (ptr void)
  let from_rebound =
    field c_type "from_rebound" bool
  let () = seal c_type

end

         
module EntityStruct = struct
  type t

   
  let c_type : t structure typ = structure "turkixir_node"
  let node =
    field c_type "node" (ptr void)
  let info =
    field c_type "info" EntityInfoStruct.c_type
  let () = seal c_type

end

      
module AnalysisUnitStruct : sig
  type t = unit ptr

  val c_type : t typ

  val unit_root : t -> EntityStruct.t structure ptr -> unit

  val unit_diagnostic_count : t -> int

  val unit_diagnostic : t -> int -> Diagnostic.t ptr -> int

  val unit_filename : t -> char ptr

  val unit_reparse_from_file : t -> string -> int

  val unit_reparse_from_buffer :
    t -> string -> string -> Unsigned.size_t -> int

  val unit_first_token : t -> Token.t ptr -> unit

  val unit_last_token : t -> Token.t ptr -> unit

  val unit_token_count : t -> int

  val unit_trivia_count : t -> int
end = struct
  (* Module defining the c structure of an analysis unit *)

  type t = unit ptr
  let c_type = ptr void

  let unit_root = foreign ~from:c_lib "turkixir_unit_root"
    (c_type @-> ptr EntityStruct.c_type @-> raisable void)

  let unit_diagnostic_count = foreign ~from:c_lib
    "turkixir_unit_diagnostic_count"
    (c_type @-> raisable int)

  let unit_diagnostic = foreign ~from:c_lib
    "turkixir_unit_diagnostic"
    (c_type @-> int @-> ptr Diagnostic.c_type @-> raisable int)

  let unit_filename = foreign ~from:c_lib
    "turkixir_unit_filename"
    (c_type @-> raisable (ptr char))

  let unit_reparse_from_file = foreign ~from:c_lib
    "turkixir_unit_reparse_from_file"
    (c_type
     @-> string
     @-> raisable int)

  let unit_reparse_from_buffer = foreign ~from:c_lib
    "turkixir_unit_reparse_from_buffer"
    (c_type
     @-> string
     @-> string
     @-> size_t
     @-> raisable int)

  let unit_first_token = foreign ~from:c_lib
    "turkixir_unit_first_token"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_last_token = foreign ~from:c_lib
    "turkixir_unit_last_token"
    (c_type
     @-> ptr Token.c_type
     @-> raisable void)

  let unit_token_count = foreign ~from:c_lib
    "turkixir_unit_token_count"
    (c_type @-> raisable int)

  let unit_trivia_count = foreign ~from:c_lib
    "turkixir_unit_trivia_count"
    (c_type @-> raisable int)
end


      
module AnalysisContextStruct : sig
  type t

  val c_type : t typ

  val create_analysis_context :
    string -> unit ptr -> unit ptr -> unit ptr -> bool -> int -> t

  val get_analysis_unit_from_file :
    t -> string -> string -> bool -> GrammarRule.t -> AnalysisUnitStruct.t

  val get_analysis_unit_from_buffer :
    t
    -> string (* Filename *)
    -> string (* Charset *)
    -> string (* Buffer *)
    -> Unsigned.size_t (* Buffer size *)
    -> GrammarRule.t
    -> AnalysisUnitStruct.t
end = struct
  (* Module defining the c structure of an analysis context *)

  (* The real C type of a context is a void*. But we use a pointer to this
     type, to be able to allocate a value of t and attach a finalizer to it.
     See wrap function *)
  type t = unit ptr ptr

  let unwrap (value : t) : unit ptr = !@value

  let context_decref =
    let f =
      foreign ~from:c_lib "turkixir_context_decref"
        (ptr void @-> raisable void)
    in
    fun ctx -> f (unwrap ctx)

  let wrap (c_value : unit ptr) : t =
    (* To deallocate cleanly the context, we need to call context_decref.
       Allocate a value and attach a finalizer to it *)
    allocate ~finalise:context_decref (ptr void) c_value

  let c_type = view (ptr void) ~read:wrap ~write:unwrap

  let create_analysis_context =
    foreign ~from:c_lib "turkixir_create_analysis_context"
      (string @-> ptr void @-> UnitProvider.c_type @-> ptr void @-> bool @-> int
      @-> raisable c_type)

  let get_analysis_unit_from_file =
    foreign ~from:c_lib "turkixir_get_analysis_unit_from_file"
      ( c_type @-> string @-> string @-> bool @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

  let get_analysis_unit_from_buffer =
    foreign ~from:c_lib "turkixir_get_analysis_unit_from_buffer"
      ( c_type @-> string (* Filename *) @-> string (* Charset *)
      @-> string (* Buffer *) @-> size_t (* Buffer size *)
      @-> GrammarRule.c_type
      @-> raisable AnalysisUnitStruct.c_type )

end

      
module Symbol : sig
  type t = string

  val c_type : t structure typ

  val wrap : (t structure) -> t

  val unwrap : AnalysisContextStruct.t -> t -> (t structure)

  val symbol_text : t structure ptr -> string ptr -> unit

  val context_symbol :
    AnalysisContextStruct.t -> string ptr -> t structure ptr -> int
end = struct
  type t = string

  let c_type : t structure typ = structure "symbol"
  let data = field c_type "data" (ptr void)
  let bounds = field c_type "bounds" (ptr void)
  let () = seal c_type

  let symbol_text = foreign ~from:c_lib "turkixir_symbol_text"
    (ptr c_type @-> ptr Text.c_type @-> raisable void)

  let wrap (c_value : t structure) : t =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    symbol_text (addr c_value) c_result_ptr;
    !@ c_result_ptr

  let context_symbol = foreign ~from:c_lib "turkixir_context_symbol"
    (AnalysisContextStruct.c_type
     @-> ptr Text.c_type
     @-> ptr c_type
     @-> raisable int)

  let unwrap (ctx : AnalysisContextStruct.t) (value : t) : t structure =
    let result = make c_type in
    let code =
      context_symbol ctx (allocate Text.c_type value) (addr result)
    in
    if code = 0 then
      raise (InvalidSymbolError value) ;
    result
end

         
module TurkixirNodeArrayStruct = struct
  

  type t

  let c_struct : t structure typ = structure "turkixir_node_array"
  let n = field c_struct "n" int
  let _ = field c_struct "ref_count" int
  let items = field c_struct "items"
    EntityStruct.c_type
  let () = seal c_struct

  let c_type = ptr c_struct

  let create = foreign ~from:c_lib "turkixir_turkixir_node_array_create"
    (int @-> raisable c_type)
  let dec_ref = foreign ~from:c_lib "turkixir_turkixir_node_array_dec_ref"
    (c_type @-> raisable void)

end


module CFunctions = struct
  let node_kind = foreign ~from:c_lib "turkixir_node_kind"
    (ptr EntityStruct.c_type @-> raisable int)

  let image = foreign ~from:c_lib "turkixir_node_image"
    (ptr EntityStruct.c_type
     @-> ptr Text.c_type
     @-> raisable void)

  let node_sloc_range = foreign ~from:c_lib
    "turkixir_node_sloc_range"
    (ptr EntityStruct.c_type
     @-> ptr SlocRange.c_type
     @-> raisable void)

  let lookup_in_node = foreign ~from:c_lib
    "turkixir_lookup_in_node"
    (ptr EntityStruct.c_type
     @-> ptr Sloc.c_type
     @-> ptr EntityStruct.c_type
     @-> raisable void)

  let entity_image = foreign ~from:c_lib
    "turkixir_entity_image"
    (ptr EntityStruct.c_type
     @-> ptr Text.c_type
     @-> raisable void)

  let node_is_token_node = foreign ~from:c_lib
    "turkixir_node_is_token_node"
    (ptr EntityStruct.c_type
     @-> raisable bool)

  let turkixir_node_parent = foreign ~from:c_lib
    "turkixir_turkixir_node_parent"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let turkixir_node_parents = foreign ~from:c_lib
    "turkixir_turkixir_node_parents"
    (ptr EntityStruct.c_type
        
    @-> bool
    @-> ptr TurkixirNodeArrayStruct.c_type
    @-> raisable int)

  let turkixir_node_children = foreign ~from:c_lib
    "turkixir_turkixir_node_children"
    (ptr EntityStruct.c_type
    @-> ptr TurkixirNodeArrayStruct.c_type
    @-> raisable int)

  let turkixir_node_token_start = foreign ~from:c_lib
    "turkixir_turkixir_node_token_start"
    (ptr EntityStruct.c_type
    @-> ptr Token.c_type
    @-> raisable int)

  let turkixir_node_token_end = foreign ~from:c_lib
    "turkixir_turkixir_node_token_end"
    (ptr EntityStruct.c_type
    @-> ptr Token.c_type
    @-> raisable int)

  let turkixir_node_child_index = foreign ~from:c_lib
    "turkixir_turkixir_node_child_index"
    (ptr EntityStruct.c_type
    @-> ptr int
    @-> raisable int)

  let turkixir_node_previous_sibling = foreign ~from:c_lib
    "turkixir_turkixir_node_previous_sibling"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let turkixir_node_next_sibling = foreign ~from:c_lib
    "turkixir_turkixir_node_next_sibling"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let turkixir_node_unit = foreign ~from:c_lib
    "turkixir_turkixir_node_unit"
    (ptr EntityStruct.c_type
    @-> ptr AnalysisUnitStruct.c_type
    @-> raisable int)

  let turkixir_node_is_ghost = foreign ~from:c_lib
    "turkixir_turkixir_node_is_ghost"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let turkixir_node_full_sloc_image = foreign ~from:c_lib
    "turkixir_turkixir_node_full_sloc_image"
    (ptr EntityStruct.c_type
    @-> ptr StringType.c_type
    @-> raisable int)

  let arg_assoc_f_name = foreign ~from:c_lib
    "turkixir_arg_assoc_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let arg_assoc_f_expr = foreign ~from:c_lib
    "turkixir_arg_assoc_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let arg_gen_f_expr = foreign ~from:c_lib
    "turkixir_arg_gen_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let arg_gen_f_comprehension = foreign ~from:c_lib
    "turkixir_arg_gen_f_comprehension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let kw_args_f_expr = foreign ~from:c_lib
    "turkixir_kw_args_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let var_args_f_expr = foreign ~from:c_lib
    "turkixir_var_args_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let as_name_node_f_imported = foreign ~from:c_lib
    "turkixir_as_name_node_f_imported"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let as_name_node_f_as_name = foreign ~from:c_lib
    "turkixir_as_name_node_f_as_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_if_f_test = foreign ~from:c_lib
    "turkixir_comp_if_f_test"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_if_f_comp = foreign ~from:c_lib
    "turkixir_comp_if_f_comp"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_for_f_exprs = foreign ~from:c_lib
    "turkixir_comp_for_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_for_f_target = foreign ~from:c_lib
    "turkixir_comp_for_f_target"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_for_f_comp = foreign ~from:c_lib
    "turkixir_comp_for_f_comp"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_forl_f_exprs = foreign ~from:c_lib
    "turkixir_comp_forl_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_forl_f_target = foreign ~from:c_lib
    "turkixir_comp_forl_f_target"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_forl_f_comp = foreign ~from:c_lib
    "turkixir_comp_forl_f_comp"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let decorator_f_dec_name = foreign ~from:c_lib
    "turkixir_decorator_f_dec_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let decorator_f_arg_list = foreign ~from:c_lib
    "turkixir_decorator_f_arg_list"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dict_assoc_f_key = foreign ~from:c_lib
    "turkixir_dict_assoc_f_key"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dict_assoc_f_value = foreign ~from:c_lib
    "turkixir_dict_assoc_f_value"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let else_part_f_statements = foreign ~from:c_lib
    "turkixir_else_part_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let except_part_f_as_name = foreign ~from:c_lib
    "turkixir_except_part_f_as_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let except_part_f_statements = foreign ~from:c_lib
    "turkixir_except_part_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let and_expr_f_left = foreign ~from:c_lib
    "turkixir_and_expr_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let and_expr_f_right = foreign ~from:c_lib
    "turkixir_and_expr_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let and_op_f_left = foreign ~from:c_lib
    "turkixir_and_op_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let and_op_f_right = foreign ~from:c_lib
    "turkixir_and_op_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let bin_op_f_left = foreign ~from:c_lib
    "turkixir_bin_op_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let bin_op_f_op = foreign ~from:c_lib
    "turkixir_bin_op_f_op"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let bin_op_f_right = foreign ~from:c_lib
    "turkixir_bin_op_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let call_expr_f_prefix = foreign ~from:c_lib
    "turkixir_call_expr_f_prefix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let call_expr_f_suffix = foreign ~from:c_lib
    "turkixir_call_expr_f_suffix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_op_f_left = foreign ~from:c_lib
    "turkixir_comp_op_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_op_f_op = foreign ~from:c_lib
    "turkixir_comp_op_f_op"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let comp_op_f_right = foreign ~from:c_lib
    "turkixir_comp_op_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let concat_string_lit_f_first_str = foreign ~from:c_lib
    "turkixir_concat_string_lit_f_first_str"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let concat_string_lit_f_subsequent_str = foreign ~from:c_lib
    "turkixir_concat_string_lit_f_subsequent_str"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dict_comp_f_assoc = foreign ~from:c_lib
    "turkixir_dict_comp_f_assoc"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dict_comp_f_comprehension = foreign ~from:c_lib
    "turkixir_dict_comp_f_comprehension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dict_lit_f_assocs = foreign ~from:c_lib
    "turkixir_dict_lit_f_assocs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let factor_f_op = foreign ~from:c_lib
    "turkixir_factor_f_op"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let factor_f_expr = foreign ~from:c_lib
    "turkixir_factor_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_expr_f_expr = foreign ~from:c_lib
    "turkixir_if_expr_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_expr_f_cond = foreign ~from:c_lib
    "turkixir_if_expr_f_cond"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_expr_f_else_expr = foreign ~from:c_lib
    "turkixir_if_expr_f_else_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let inline_eval_f_exprs = foreign ~from:c_lib
    "turkixir_inline_eval_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let lambda_def_f_args = foreign ~from:c_lib
    "turkixir_lambda_def_f_args"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let lambda_def_f_expr = foreign ~from:c_lib
    "turkixir_lambda_def_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let list_comp_f_expr = foreign ~from:c_lib
    "turkixir_list_comp_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let list_comp_f_comprehension = foreign ~from:c_lib
    "turkixir_list_comp_f_comprehension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let list_gen_f_expr = foreign ~from:c_lib
    "turkixir_list_gen_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let list_gen_f_comprehension = foreign ~from:c_lib
    "turkixir_list_gen_f_comprehension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let list_lit_f_exprs = foreign ~from:c_lib
    "turkixir_list_lit_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dotted_name_f_prefix = foreign ~from:c_lib
    "turkixir_dotted_name_f_prefix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let dotted_name_f_suffix = foreign ~from:c_lib
    "turkixir_dotted_name_f_suffix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let not_op_f_expr = foreign ~from:c_lib
    "turkixir_not_op_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let or_expr_f_left = foreign ~from:c_lib
    "turkixir_or_expr_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let or_expr_f_right = foreign ~from:c_lib
    "turkixir_or_expr_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let or_op_f_left = foreign ~from:c_lib
    "turkixir_or_op_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let or_op_f_right = foreign ~from:c_lib
    "turkixir_or_op_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let power_f_left = foreign ~from:c_lib
    "turkixir_power_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let power_f_right = foreign ~from:c_lib
    "turkixir_power_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let set_comp_f_expr = foreign ~from:c_lib
    "turkixir_set_comp_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let set_comp_f_comprehension = foreign ~from:c_lib
    "turkixir_set_comp_f_comprehension"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let set_lit_f_exprs = foreign ~from:c_lib
    "turkixir_set_lit_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let slice_expr_f_first = foreign ~from:c_lib
    "turkixir_slice_expr_f_first"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let slice_expr_f_last = foreign ~from:c_lib
    "turkixir_slice_expr_f_last"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let ext_slice_expr_f_stride = foreign ~from:c_lib
    "turkixir_ext_slice_expr_f_stride"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let subscript_expr_f_prefix = foreign ~from:c_lib
    "turkixir_subscript_expr_f_prefix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let subscript_expr_f_suffix = foreign ~from:c_lib
    "turkixir_subscript_expr_f_suffix"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let tuple_lit_f_exprs = foreign ~from:c_lib
    "turkixir_tuple_lit_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let xor_expr_f_left = foreign ~from:c_lib
    "turkixir_xor_expr_f_left"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let xor_expr_f_right = foreign ~from:c_lib
    "turkixir_xor_expr_f_right"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let yield_expr_f_exprs = foreign ~from:c_lib
    "turkixir_yield_expr_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let file_node_f_statements = foreign ~from:c_lib
    "turkixir_file_node_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let kw_args_flag_p_as_bool = foreign ~from:c_lib
    "turkixir_kw_args_flag_p_as_bool"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)

  let params_f_single_params = foreign ~from:c_lib
    "turkixir_params_f_single_params"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let rel_name_f_dots = foreign ~from:c_lib
    "turkixir_rel_name_f_dots"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let rel_name_f_name = foreign ~from:c_lib
    "turkixir_rel_name_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let single_param_f_is_varargs = foreign ~from:c_lib
    "turkixir_single_param_f_is_varargs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let single_param_f_is_kwargs = foreign ~from:c_lib
    "turkixir_single_param_f_is_kwargs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let single_param_f_name = foreign ~from:c_lib
    "turkixir_single_param_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let single_param_f_default_value = foreign ~from:c_lib
    "turkixir_single_param_f_default_value"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let assert_stmt_f_test_expr = foreign ~from:c_lib
    "turkixir_assert_stmt_f_test_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let assert_stmt_f_msg = foreign ~from:c_lib
    "turkixir_assert_stmt_f_msg"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let assign_stmt_f_l_value = foreign ~from:c_lib
    "turkixir_assign_stmt_f_l_value"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let assign_stmt_f_r_values = foreign ~from:c_lib
    "turkixir_assign_stmt_f_r_values"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let aug_assign_stmt_f_l_value = foreign ~from:c_lib
    "turkixir_aug_assign_stmt_f_l_value"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let aug_assign_stmt_f_op = foreign ~from:c_lib
    "turkixir_aug_assign_stmt_f_op"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let aug_assign_stmt_f_r_value = foreign ~from:c_lib
    "turkixir_aug_assign_stmt_f_r_value"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let decorated_f_decorators = foreign ~from:c_lib
    "turkixir_decorated_f_decorators"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let decorated_f_defn = foreign ~from:c_lib
    "turkixir_decorated_f_defn"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let class_def_f_name = foreign ~from:c_lib
    "turkixir_class_def_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let class_def_f_bases = foreign ~from:c_lib
    "turkixir_class_def_f_bases"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let class_def_f_statements = foreign ~from:c_lib
    "turkixir_class_def_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let func_def_f_name = foreign ~from:c_lib
    "turkixir_func_def_f_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let func_def_f_parameters = foreign ~from:c_lib
    "turkixir_func_def_f_parameters"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let func_def_f_body = foreign ~from:c_lib
    "turkixir_func_def_f_body"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let del_stmt_f_exprs = foreign ~from:c_lib
    "turkixir_del_stmt_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let elif_branch_f_cond_test = foreign ~from:c_lib
    "turkixir_elif_branch_f_cond_test"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let elif_branch_f_statements = foreign ~from:c_lib
    "turkixir_elif_branch_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let exec_stmt_f_expr = foreign ~from:c_lib
    "turkixir_exec_stmt_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let exec_stmt_f_in_list = foreign ~from:c_lib
    "turkixir_exec_stmt_f_in_list"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let for_stmt_f_bindings = foreign ~from:c_lib
    "turkixir_for_stmt_f_bindings"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let for_stmt_f_expr = foreign ~from:c_lib
    "turkixir_for_stmt_f_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let for_stmt_f_statements = foreign ~from:c_lib
    "turkixir_for_stmt_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let for_stmt_f_else_part = foreign ~from:c_lib
    "turkixir_for_stmt_f_else_part"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let global_stmt_f_names = foreign ~from:c_lib
    "turkixir_global_stmt_f_names"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_stmt_f_cond_test = foreign ~from:c_lib
    "turkixir_if_stmt_f_cond_test"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_stmt_f_statements = foreign ~from:c_lib
    "turkixir_if_stmt_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_stmt_f_elif_branchs = foreign ~from:c_lib
    "turkixir_if_stmt_f_elif_branchs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let if_stmt_f_else_part = foreign ~from:c_lib
    "turkixir_if_stmt_f_else_part"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let import_from_f_rel_name = foreign ~from:c_lib
    "turkixir_import_from_f_rel_name"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let import_from_f_imported = foreign ~from:c_lib
    "turkixir_import_from_f_imported"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let import_name_f_imported_names = foreign ~from:c_lib
    "turkixir_import_name_f_imported_names"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let print_stmt_f_exprs = foreign ~from:c_lib
    "turkixir_print_stmt_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let raise_stmt_f_exprs = foreign ~from:c_lib
    "turkixir_raise_stmt_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let return_stmt_f_exprs = foreign ~from:c_lib
    "turkixir_return_stmt_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let stream_print_stmt_f_stream_expr = foreign ~from:c_lib
    "turkixir_stream_print_stmt_f_stream_expr"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let stream_print_stmt_f_exprs = foreign ~from:c_lib
    "turkixir_stream_print_stmt_f_exprs"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let try_stmt_f_statements = foreign ~from:c_lib
    "turkixir_try_stmt_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let try_stmt_f_except_parts = foreign ~from:c_lib
    "turkixir_try_stmt_f_except_parts"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let try_stmt_f_else_part = foreign ~from:c_lib
    "turkixir_try_stmt_f_else_part"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let try_stmt_f_finally_part = foreign ~from:c_lib
    "turkixir_try_stmt_f_finally_part"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let while_stmt_f_cond_test = foreign ~from:c_lib
    "turkixir_while_stmt_f_cond_test"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let while_stmt_f_statements = foreign ~from:c_lib
    "turkixir_while_stmt_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let while_stmt_f_else_part = foreign ~from:c_lib
    "turkixir_while_stmt_f_else_part"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let with_stmt_f_bindings = foreign ~from:c_lib
    "turkixir_with_stmt_f_bindings"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let with_stmt_f_statements = foreign ~from:c_lib
    "turkixir_with_stmt_f_statements"
    (ptr EntityStruct.c_type
    @-> ptr EntityStruct.c_type
    @-> raisable int)

  let var_args_flag_p_as_bool = foreign ~from:c_lib
    "turkixir_var_args_flag_p_as_bool"
    (ptr EntityStruct.c_type
    @-> ptr bool
    @-> raisable int)


end

type analysis_context = {
  c_value : AnalysisContextStruct.t;
  unit_provider : UnitProvider.t
}

and analysis_unit = {
  c_value : AnalysisUnitStruct.t;
  context : analysis_context
}

and entity = EntityStruct.t structure


  and entity_info = {
      rebindings :
         Rebindings.t;
      from_rebound :
         bool;
  }





  
   
  (**
    * arg
    * as_name_node
    * comp_if
    * comp_op_kind
    * comprehension
    * decorator
    * dict_assoc
    * else_part
    * except_part
    * expr
    * file_node
    * import_star
    * kw_args_flag
    * nl
    * op
    * params
    * rel_name
    * single_param
    * stmt
    * turkixir_node_base_list
    * var_args_flag
    *)
  and turkixir_node = [
    | `ArgAssoc
        of arg_assoc_fields
    | `ArgGen
        of arg_gen_fields
    | `KwArgs
        of kw_args_fields
    | `VarArgs
        of var_args_fields
    | `AsNameNode
        of as_name_node_fields
    | `CompIf
        of comp_if_fields
    | `CompOpKindDiamond
        of comp_op_kind_diamond_fields
    | `CompOpKindEq
        of comp_op_kind_eq_fields
    | `CompOpKindGt
        of comp_op_kind_gt_fields
    | `CompOpKindGte
        of comp_op_kind_gte_fields
    | `CompOpKindIn
        of comp_op_kind_in_fields
    | `CompOpKindIs
        of comp_op_kind_is_fields
    | `CompOpKindIsnot
        of comp_op_kind_isnot_fields
    | `CompOpKindLt
        of comp_op_kind_lt_fields
    | `CompOpKindLte
        of comp_op_kind_lte_fields
    | `CompOpKindNoteq
        of comp_op_kind_noteq_fields
    | `CompOpKindNotin
        of comp_op_kind_notin_fields
    | `CompFor
        of comp_for_fields
    | `CompForL
        of comp_forl_fields
    | `Decorator
        of decorator_fields
    | `DictAssoc
        of dict_assoc_fields
    | `ElsePart
        of else_part_fields
    | `ExceptPart
        of except_part_fields
    | `AndExpr
        of and_expr_fields
    | `AndOp
        of and_op_fields
    | `ArithExpr
        of arith_expr_fields
    | `ShiftExpr
        of shift_expr_fields
    | `Term
        of term_fields
    | `CallExpr
        of call_expr_fields
    | `CompOp
        of comp_op_fields
    | `ConcatStringLit
        of concat_string_lit_fields
    | `DictComp
        of dict_comp_fields
    | `DictLit
        of dict_lit_fields
    | `Dot
        of dot_fields
    | `EllipsisExpr
        of ellipsis_expr_fields
    | `Factor
        of factor_fields
    | `IfExpr
        of if_expr_fields
    | `InlineEval
        of inline_eval_fields
    | `LambdaDef
        of lambda_def_fields
    | `ListComp
        of list_comp_fields
    | `ListGen
        of list_gen_fields
    | `ListLit
        of list_lit_fields
    | `DottedName
        of dotted_name_fields
    | `Id
        of id_fields
    | `NotOp
        of not_op_fields
    | `NumberLit
        of number_lit_fields
    | `OrExpr
        of or_expr_fields
    | `OrOp
        of or_op_fields
    | `Power
        of power_fields
    | `SetComp
        of set_comp_fields
    | `SetLit
        of set_lit_fields
    | `SliceExpr
        of slice_expr_fields
    | `ExtSliceExpr
        of ext_slice_expr_fields
    | `StringLit
        of string_lit_fields
    | `SubscriptExpr
        of subscript_expr_fields
    | `TupleLit
        of tuple_lit_fields
    | `XorExpr
        of xor_expr_fields
    | `YieldExpr
        of yield_expr_fields
    | `FileNode
        of file_node_fields
    | `ImportStar
        of import_star_fields
    | `KwArgsFlagAbsent
        of kw_args_flag_absent_fields
    | `KwArgsFlagPresent
        of kw_args_flag_present_fields
    | `NL
        of nl_fields
    | `Op
        of op_fields
    | `Params
        of params_fields
    | `RelName
        of rel_name_fields
    | `SingleParam
        of single_param_fields
    | `AssertStmt
        of assert_stmt_fields
    | `AssignStmt
        of assign_stmt_fields
    | `AugAssignStmt
        of aug_assign_stmt_fields
    | `BreakStmt
        of break_stmt_fields
    | `ContinueStmt
        of continue_stmt_fields
    | `Decorated
        of decorated_fields
    | `ClassDef
        of class_def_fields
    | `FuncDef
        of func_def_fields
    | `DelStmt
        of del_stmt_fields
    | `ElifBranch
        of elif_branch_fields
    | `ExecStmt
        of exec_stmt_fields
    | `ForStmt
        of for_stmt_fields
    | `GlobalStmt
        of global_stmt_fields
    | `IfStmt
        of if_stmt_fields
    | `ImportFrom
        of import_from_fields
    | `ImportName
        of import_name_fields
    | `PassStmt
        of pass_stmt_fields
    | `PrintStmt
        of print_stmt_fields
    | `RaiseStmt
        of raise_stmt_fields
    | `ReturnStmt
        of return_stmt_fields
    | `StreamPrintStmt
        of stream_print_stmt_fields
    | `TryStmt
        of try_stmt_fields
    | `WhileStmt
        of while_stmt_fields
    | `WithStmt
        of with_stmt_fields
    | `ArgList
        of arg_list_fields
    | `AsNameNodeList
        of as_name_node_list_fields
    | `DecoratorList
        of decorator_list_fields
    | `DictAssocList
        of dict_assoc_list_fields
    | `DotList
        of dot_list_fields
    | `ElifBranchList
        of elif_branch_list_fields
    | `ExceptPartList
        of except_part_list_fields
    | `ExprList
        of expr_list_fields
    | `IdList
        of id_list_fields
    | `NLList
        of nl_list_fields
    | `SingleParamList
        of single_param_list_fields
    | `StringLitList
        of string_lit_list_fields
    | `TurkixirNodeList
        of turkixir_node_list_fields
    | `VarArgsFlagAbsent
        of var_args_flag_absent_fields
    | `VarArgsFlagPresent
        of var_args_flag_present_fields
  ]

  
   
  and arg = [
    | `ArgAssoc
        of arg_assoc_fields
    | `ArgGen
        of arg_gen_fields
    | `KwArgs
        of kw_args_fields
    | `VarArgs
        of var_args_fields
  ]

  
   
  and arg_assoc = [
    | `ArgAssoc
        of arg_assoc_fields
  ]
  and arg_assoc_fields = 
  {
         
    f_name: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and arg_gen = [
    | `ArgGen
        of arg_gen_fields
  ]
  and arg_gen_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and kw_args = [
    | `KwArgs
        of kw_args_fields
  ]
  and kw_args_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and var_args = [
    | `VarArgs
        of var_args_fields
  ]
  and var_args_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and as_name_node = [
    | `AsNameNode
        of as_name_node_fields
  ]
  and as_name_node_fields = 
  {
         
    f_imported: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_as_name: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_if = [
    | `CompIf
        of comp_if_fields
  ]
  and comp_if_fields = 
  {
         
    f_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comp: [
      | `CompFor
          of comp_for_fields
      | `CompForL
          of comp_forl_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind = [
    | `CompOpKindDiamond
        of comp_op_kind_diamond_fields
    | `CompOpKindEq
        of comp_op_kind_eq_fields
    | `CompOpKindGt
        of comp_op_kind_gt_fields
    | `CompOpKindGte
        of comp_op_kind_gte_fields
    | `CompOpKindIn
        of comp_op_kind_in_fields
    | `CompOpKindIs
        of comp_op_kind_is_fields
    | `CompOpKindIsnot
        of comp_op_kind_isnot_fields
    | `CompOpKindLt
        of comp_op_kind_lt_fields
    | `CompOpKindLte
        of comp_op_kind_lte_fields
    | `CompOpKindNoteq
        of comp_op_kind_noteq_fields
    | `CompOpKindNotin
        of comp_op_kind_notin_fields
  ]

  
   
  and comp_op_kind_diamond = [
    | `CompOpKindDiamond
        of comp_op_kind_diamond_fields
  ]
  and comp_op_kind_diamond_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_eq = [
    | `CompOpKindEq
        of comp_op_kind_eq_fields
  ]
  and comp_op_kind_eq_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_gt = [
    | `CompOpKindGt
        of comp_op_kind_gt_fields
  ]
  and comp_op_kind_gt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_gte = [
    | `CompOpKindGte
        of comp_op_kind_gte_fields
  ]
  and comp_op_kind_gte_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_in = [
    | `CompOpKindIn
        of comp_op_kind_in_fields
  ]
  and comp_op_kind_in_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_is = [
    | `CompOpKindIs
        of comp_op_kind_is_fields
  ]
  and comp_op_kind_is_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_isnot = [
    | `CompOpKindIsnot
        of comp_op_kind_isnot_fields
  ]
  and comp_op_kind_isnot_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_lt = [
    | `CompOpKindLt
        of comp_op_kind_lt_fields
  ]
  and comp_op_kind_lt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_lte = [
    | `CompOpKindLte
        of comp_op_kind_lte_fields
  ]
  and comp_op_kind_lte_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_noteq = [
    | `CompOpKindNoteq
        of comp_op_kind_noteq_fields
  ]
  and comp_op_kind_noteq_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op_kind_notin = [
    | `CompOpKindNotin
        of comp_op_kind_notin_fields
  ]
  and comp_op_kind_notin_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and comprehension = [
    | `CompFor
        of comp_for_fields
    | `CompForL
        of comp_forl_fields
  ]

  
   
  and comp_for = [
    | `CompFor
        of comp_for_fields
  ]
  and comp_for_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
         
    f_target: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comp: [
      | `CompFor
          of comp_for_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_forl = [
    | `CompForL
        of comp_forl_fields
  ]
  and comp_forl_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
         
    f_target: expr_list
    Lazy.t;
         
    f_comp: [
      | `CompForL
          of comp_forl_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and decorator = [
    | `Decorator
        of decorator_fields
  ]
  and decorator_fields = 
  {
         
    f_dec_name: name
    Lazy.t;
         
    f_arg_list: arg_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dict_assoc = [
    | `DictAssoc
        of dict_assoc_fields
  ]
  and dict_assoc_fields = 
  {
         
    f_key: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_value: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and else_part = [
    | `ElsePart
        of else_part_fields
  ]
  and else_part_fields = 
  {
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and except_part = [
    | `ExceptPart
        of except_part_fields
  ]
  and except_part_fields = 
  {
         
    f_as_name: as_name_node
    option
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * and_expr
    * and_op
    * bin_op
    * call_expr
    * comp_op
    * concat_string_lit
    * dict_comp
    * dict_lit
    * dot
    * ellipsis_expr
    * factor
    * if_expr
    * inline_eval
    * lambda_def
    * list_comp
    * list_gen
    * list_lit
    * name
    * not_op
    * number_lit
    * or_expr
    * or_op
    * power
    * set_comp
    * set_lit
    * slice_expr
    * string_lit
    * subscript_expr
    * tuple_lit
    * xor_expr
    * yield_expr
    *)
  and expr = [
    | `AndExpr
        of and_expr_fields
    | `AndOp
        of and_op_fields
    | `ArithExpr
        of arith_expr_fields
    | `ShiftExpr
        of shift_expr_fields
    | `Term
        of term_fields
    | `CallExpr
        of call_expr_fields
    | `CompOp
        of comp_op_fields
    | `ConcatStringLit
        of concat_string_lit_fields
    | `DictComp
        of dict_comp_fields
    | `DictLit
        of dict_lit_fields
    | `Dot
        of dot_fields
    | `EllipsisExpr
        of ellipsis_expr_fields
    | `Factor
        of factor_fields
    | `IfExpr
        of if_expr_fields
    | `InlineEval
        of inline_eval_fields
    | `LambdaDef
        of lambda_def_fields
    | `ListComp
        of list_comp_fields
    | `ListGen
        of list_gen_fields
    | `ListLit
        of list_lit_fields
    | `DottedName
        of dotted_name_fields
    | `Id
        of id_fields
    | `NotOp
        of not_op_fields
    | `NumberLit
        of number_lit_fields
    | `OrExpr
        of or_expr_fields
    | `OrOp
        of or_op_fields
    | `Power
        of power_fields
    | `SetComp
        of set_comp_fields
    | `SetLit
        of set_lit_fields
    | `SliceExpr
        of slice_expr_fields
    | `ExtSliceExpr
        of ext_slice_expr_fields
    | `StringLit
        of string_lit_fields
    | `SubscriptExpr
        of subscript_expr_fields
    | `TupleLit
        of tuple_lit_fields
    | `XorExpr
        of xor_expr_fields
    | `YieldExpr
        of yield_expr_fields
  ]

  
   
  and and_expr = [
    | `AndExpr
        of and_expr_fields
  ]
  and and_expr_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and and_op = [
    | `AndOp
        of and_op_fields
  ]
  and and_op_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and bin_op = [
    | `ArithExpr
        of arith_expr_fields
    | `ShiftExpr
        of shift_expr_fields
    | `Term
        of term_fields
  ]

  
   
  and arith_expr = [
    | `ArithExpr
        of arith_expr_fields
  ]
  and arith_expr_fields = 
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and shift_expr = [
    | `ShiftExpr
        of shift_expr_fields
  ]
  and shift_expr_fields = 
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and term = [
    | `Term
        of term_fields
  ]
  and term_fields = 
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and call_expr = [
    | `CallExpr
        of call_expr_fields
  ]
  and call_expr_fields = 
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: arg_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and comp_op = [
    | `CompOp
        of comp_op_fields
  ]
  and comp_op_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: comp_op_kind
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and concat_string_lit = [
    | `ConcatStringLit
        of concat_string_lit_fields
  ]
  and concat_string_lit_fields = 
  {
         
    f_first_str: string_lit
    Lazy.t;
         
    f_subsequent_str: string_lit_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dict_comp = [
    | `DictComp
        of dict_comp_fields
  ]
  and dict_comp_fields = 
  {
         
    f_assoc: dict_assoc
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dict_lit = [
    | `DictLit
        of dict_lit_fields
  ]
  and dict_lit_fields = 
  {
         
    f_assocs: dict_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dot = [
    | `Dot
        of dot_fields
  ]
  and dot_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and ellipsis_expr = [
    | `EllipsisExpr
        of ellipsis_expr_fields
  ]
  and ellipsis_expr_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and factor = [
    | `Factor
        of factor_fields
  ]
  and factor_fields = 
  {
         
    f_op: op
    Lazy.t;
         
    f_expr: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and if_expr = [
    | `IfExpr
        of if_expr_fields
  ]
  and if_expr_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_cond: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and inline_eval = [
    | `InlineEval
        of inline_eval_fields
  ]
  and inline_eval_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and lambda_def = [
    | `LambdaDef
        of lambda_def_fields
  ]
  and lambda_def_fields = 
  {
         
    f_args: params
    Lazy.t;
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and list_comp = [
    | `ListComp
        of list_comp_fields
  ]
  and list_comp_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_forl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and list_gen = [
    | `ListGen
        of list_gen_fields
  ]
  and list_gen_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_forl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and list_lit = [
    | `ListLit
        of list_lit_fields
  ]
  and list_lit_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and name = [
    | `DottedName
        of dotted_name_fields
    | `Id
        of id_fields
  ]

  
   
  and dotted_name = [
    | `DottedName
        of dotted_name_fields
  ]
  and dotted_name_fields = 
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: id
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and id = [
    | `Id
        of id_fields
  ]
  and id_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and not_op = [
    | `NotOp
        of not_op_fields
  ]
  and not_op_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and number_lit = [
    | `NumberLit
        of number_lit_fields
  ]
  and number_lit_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and or_expr = [
    | `OrExpr
        of or_expr_fields
  ]
  and or_expr_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and or_op = [
    | `OrOp
        of or_op_fields
  ]
  and or_op_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and power = [
    | `Power
        of power_fields
  ]
  and power_fields = 
  {
         
    f_left: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and set_comp = [
    | `SetComp
        of set_comp_fields
  ]
  and set_comp_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and set_lit = [
    | `SetLit
        of set_lit_fields
  ]
  and set_lit_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and slice_expr = [
    | `SliceExpr
        of slice_expr_fields
    | `ExtSliceExpr
        of ext_slice_expr_fields
  ]
  and slice_expr_fields = 
  {
         
    f_first: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_last: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and ext_slice_expr = [
    | `ExtSliceExpr
        of ext_slice_expr_fields
  ]
  and ext_slice_expr_fields = 
  {
         
    f_first: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_last: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_stride: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and string_lit = [
    | `StringLit
        of string_lit_fields
  ]
  and string_lit_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and subscript_expr = [
    | `SubscriptExpr
        of subscript_expr_fields
  ]
  and subscript_expr_fields = 
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and tuple_lit = [
    | `TupleLit
        of tuple_lit_fields
  ]
  and tuple_lit_fields = 
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and xor_expr = [
    | `XorExpr
        of xor_expr_fields
  ]
  and xor_expr_fields = 
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and yield_expr = [
    | `YieldExpr
        of yield_expr_fields
  ]
  and yield_expr_fields = 
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and file_node = [
    | `FileNode
        of file_node_fields
  ]
  and file_node_fields = 
  {
         
    f_statements: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and import_star = [
    | `ImportStar
        of import_star_fields
  ]
  and import_star_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and kw_args_flag = [
    | `KwArgsFlagAbsent
        of kw_args_flag_absent_fields
    | `KwArgsFlagPresent
        of kw_args_flag_present_fields
  ]

  
   
  and kw_args_flag_absent = [
    | `KwArgsFlagAbsent
        of kw_args_flag_absent_fields
  ]
  and kw_args_flag_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and kw_args_flag_present = [
    | `KwArgsFlagPresent
        of kw_args_flag_present_fields
  ]
  and kw_args_flag_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and nl = [
    | `NL
        of nl_fields
  ]
  and nl_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and op = [
    | `Op
        of op_fields
  ]
  and op_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and params = [
    | `Params
        of params_fields
  ]
  and params_fields = 
  {
         
    f_single_params: single_param_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and rel_name = [
    | `RelName
        of rel_name_fields
  ]
  and rel_name_fields = 
  {
         
    f_dots: dot_list
    Lazy.t;
         
    f_name: name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_param = [
    | `SingleParam
        of single_param_fields
  ]
  and single_param_fields = 
  {
         
    f_is_varargs: var_args_flag
    Lazy.t;
         
    f_is_kwargs: kw_args_flag
    Lazy.t;
         
    f_name: [
      | `Id
          of id_fields
      | `IdList
          of id_list_fields
    ]
    Lazy.t;
         
    f_default_value: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  (**
    * assert_stmt
    * assign_stmt
    * aug_assign_stmt
    * break_stmt
    * continue_stmt
    * decorated
    * def_stmt
    * del_stmt
    * elif_branch
    * exec_stmt
    * for_stmt
    * global_stmt
    * if_stmt
    * import_from
    * import_name
    * pass_stmt
    * print_stmt
    * raise_stmt
    * return_stmt
    * stream_print_stmt
    * try_stmt
    * while_stmt
    * with_stmt
    *)
  and stmt = [
    | `AssertStmt
        of assert_stmt_fields
    | `AssignStmt
        of assign_stmt_fields
    | `AugAssignStmt
        of aug_assign_stmt_fields
    | `BreakStmt
        of break_stmt_fields
    | `ContinueStmt
        of continue_stmt_fields
    | `Decorated
        of decorated_fields
    | `ClassDef
        of class_def_fields
    | `FuncDef
        of func_def_fields
    | `DelStmt
        of del_stmt_fields
    | `ElifBranch
        of elif_branch_fields
    | `ExecStmt
        of exec_stmt_fields
    | `ForStmt
        of for_stmt_fields
    | `GlobalStmt
        of global_stmt_fields
    | `IfStmt
        of if_stmt_fields
    | `ImportFrom
        of import_from_fields
    | `ImportName
        of import_name_fields
    | `PassStmt
        of pass_stmt_fields
    | `PrintStmt
        of print_stmt_fields
    | `RaiseStmt
        of raise_stmt_fields
    | `ReturnStmt
        of return_stmt_fields
    | `StreamPrintStmt
        of stream_print_stmt_fields
    | `TryStmt
        of try_stmt_fields
    | `WhileStmt
        of while_stmt_fields
    | `WithStmt
        of with_stmt_fields
  ]

  
   
  and assert_stmt = [
    | `AssertStmt
        of assert_stmt_fields
  ]
  and assert_stmt_fields = 
  {
         
    f_test_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_msg: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and assign_stmt = [
    | `AssignStmt
        of assign_stmt_fields
  ]
  and assign_stmt_fields = 
  {
         
    f_l_value: expr_list
    Lazy.t;
         
    f_r_values: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and aug_assign_stmt = [
    | `AugAssignStmt
        of aug_assign_stmt_fields
  ]
  and aug_assign_stmt_fields = 
  {
         
    f_l_value: expr_list
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_r_value: [
      | `ExprList
          of expr_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and break_stmt = [
    | `BreakStmt
        of break_stmt_fields
  ]
  and break_stmt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and continue_stmt = [
    | `ContinueStmt
        of continue_stmt_fields
  ]
  and continue_stmt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and decorated = [
    | `Decorated
        of decorated_fields
  ]
  and decorated_fields = 
  {
         
    f_decorators: decorator_list
    Lazy.t;
         
    f_defn: def_stmt
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and def_stmt = [
    | `ClassDef
        of class_def_fields
    | `FuncDef
        of func_def_fields
  ]

  
   
  and class_def = [
    | `ClassDef
        of class_def_fields
  ]
  and class_def_fields = 
  {
         
    f_name: id
    Lazy.t;
         
    f_bases: expr_list
    option
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and func_def = [
    | `FuncDef
        of func_def_fields
  ]
  and func_def_fields = 
  {
         
    f_name: id
    Lazy.t;
         
    f_parameters: params
    option
    Lazy.t;
         
    f_body: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and del_stmt = [
    | `DelStmt
        of del_stmt_fields
  ]
  and del_stmt_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elif_branch = [
    | `ElifBranch
        of elif_branch_fields
  ]
  and elif_branch_fields = 
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and exec_stmt = [
    | `ExecStmt
        of exec_stmt_fields
  ]
  and exec_stmt_fields = 
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_in_list: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and for_stmt = [
    | `ForStmt
        of for_stmt_fields
  ]
  and for_stmt_fields = 
  {
         
    f_bindings: expr_list
    Lazy.t;
         
    f_expr: expr_list
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and global_stmt = [
    | `GlobalStmt
        of global_stmt_fields
  ]
  and global_stmt_fields = 
  {
         
    f_names: id_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and if_stmt = [
    | `IfStmt
        of if_stmt_fields
  ]
  and if_stmt_fields = 
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_elif_branchs: elif_branch_list
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and import_from = [
    | `ImportFrom
        of import_from_fields
  ]
  and import_from_fields = 
  {
         
    f_rel_name: [
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `RelName
          of rel_name_fields
    ]
    Lazy.t;
         
    f_imported: [
      | `ImportStar
          of import_star_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and import_name = [
    | `ImportName
        of import_name_fields
  ]
  and import_name_fields = 
  {
         
    f_imported_names: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and pass_stmt = [
    | `PassStmt
        of pass_stmt_fields
  ]
  and pass_stmt_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and print_stmt = [
    | `PrintStmt
        of print_stmt_fields
  ]
  and print_stmt_fields = 
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and raise_stmt = [
    | `RaiseStmt
        of raise_stmt_fields
  ]
  and raise_stmt_fields = 
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and return_stmt = [
    | `ReturnStmt
        of return_stmt_fields
  ]
  and return_stmt_fields = 
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and stream_print_stmt = [
    | `StreamPrintStmt
        of stream_print_stmt_fields
  ]
  and stream_print_stmt_fields = 
  {
         
    f_stream_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and try_stmt = [
    | `TryStmt
        of try_stmt_fields
  ]
  and try_stmt_fields = 
  {
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_except_parts: except_part_list
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
         
    f_finally_part: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and while_stmt = [
    | `WhileStmt
        of while_stmt_fields
  ]
  and while_stmt_fields = 
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and with_stmt = [
    | `WithStmt
        of with_stmt_fields
  ]
  and with_stmt_fields = 
  {
         
    f_bindings: as_name_node_list
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and turkixir_node_base_list = [
    | `ArgList
        of arg_list_fields
    | `AsNameNodeList
        of as_name_node_list_fields
    | `DecoratorList
        of decorator_list_fields
    | `DictAssocList
        of dict_assoc_list_fields
    | `DotList
        of dot_list_fields
    | `ElifBranchList
        of elif_branch_list_fields
    | `ExceptPartList
        of except_part_list_fields
    | `ExprList
        of expr_list_fields
    | `IdList
        of id_list_fields
    | `NLList
        of nl_list_fields
    | `SingleParamList
        of single_param_list_fields
    | `StringLitList
        of string_lit_list_fields
    | `TurkixirNodeList
        of turkixir_node_list_fields
  ]

  
   
  and arg_list = [
    | `ArgList
        of arg_list_fields
  ]
  and arg_list_fields = 
  {
    list : arg list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and as_name_node_list = [
    | `AsNameNodeList
        of as_name_node_list_fields
  ]
  and as_name_node_list_fields = 
  {
    list : as_name_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and decorator_list = [
    | `DecoratorList
        of decorator_list_fields
  ]
  and decorator_list_fields = 
  {
    list : decorator list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dict_assoc_list = [
    | `DictAssocList
        of dict_assoc_list_fields
  ]
  and dict_assoc_list_fields = 
  {
    list : dict_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and dot_list = [
    | `DotList
        of dot_list_fields
  ]
  and dot_list_fields = 
  {
    list : dot list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and elif_branch_list = [
    | `ElifBranchList
        of elif_branch_list_fields
  ]
  and elif_branch_list_fields = 
  {
    list : elif_branch list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and except_part_list = [
    | `ExceptPartList
        of except_part_list_fields
  ]
  and except_part_list_fields = 
  {
    list : except_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and expr_list = [
    | `ExprList
        of expr_list_fields
  ]
  and expr_list_fields = 
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and id_list = [
    | `IdList
        of id_list_fields
  ]
  and id_list_fields = 
  {
    list : id list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and nl_list = [
    | `NLList
        of nl_list_fields
  ]
  and nl_list_fields = 
  {
    list : nl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and single_param_list = [
    | `SingleParamList
        of single_param_list_fields
  ]
  and single_param_list_fields = 
  {
    list : single_param list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and string_lit_list = [
    | `StringLitList
        of string_lit_list_fields
  ]
  and string_lit_list_fields = 
  {
    list : string_lit list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and turkixir_node_list = [
    | `TurkixirNodeList
        of turkixir_node_list_fields
  ]
  and turkixir_node_list_fields = 
  {
    list : turkixir_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  
   
  and var_args_flag = [
    | `VarArgsFlagAbsent
        of var_args_flag_absent_fields
    | `VarArgsFlagPresent
        of var_args_flag_present_fields
  ]

  
   
  and var_args_flag_absent = [
    | `VarArgsFlagAbsent
        of var_args_flag_absent_fields
  ]
  and var_args_flag_absent_fields = 
  {
    c_value : entity;
    context : analysis_context
  }


  
   
  and var_args_flag_present = [
    | `VarArgsFlagPresent
        of var_args_flag_present_fields
  ]
  and var_args_flag_present_fields = 
  {
    c_value : entity;
    context : analysis_context
  }




let rec unwrap_turkixir_node value =
  (* This is the unique unwrap function that can be called for any node. *)
  match (value :> turkixir_node) with
  | `ArgAssoc fields -> fields.c_value
  | `ArgGen fields -> fields.c_value
  | `KwArgs fields -> fields.c_value
  | `VarArgs fields -> fields.c_value
  | `AsNameNode fields -> fields.c_value
  | `CompIf fields -> fields.c_value
  | `CompOpKindDiamond fields -> fields.c_value
  | `CompOpKindEq fields -> fields.c_value
  | `CompOpKindGt fields -> fields.c_value
  | `CompOpKindGte fields -> fields.c_value
  | `CompOpKindIn fields -> fields.c_value
  | `CompOpKindIs fields -> fields.c_value
  | `CompOpKindIsnot fields -> fields.c_value
  | `CompOpKindLt fields -> fields.c_value
  | `CompOpKindLte fields -> fields.c_value
  | `CompOpKindNoteq fields -> fields.c_value
  | `CompOpKindNotin fields -> fields.c_value
  | `CompFor fields -> fields.c_value
  | `CompForL fields -> fields.c_value
  | `Decorator fields -> fields.c_value
  | `DictAssoc fields -> fields.c_value
  | `ElsePart fields -> fields.c_value
  | `ExceptPart fields -> fields.c_value
  | `AndExpr fields -> fields.c_value
  | `AndOp fields -> fields.c_value
  | `ArithExpr fields -> fields.c_value
  | `ShiftExpr fields -> fields.c_value
  | `Term fields -> fields.c_value
  | `CallExpr fields -> fields.c_value
  | `CompOp fields -> fields.c_value
  | `ConcatStringLit fields -> fields.c_value
  | `DictComp fields -> fields.c_value
  | `DictLit fields -> fields.c_value
  | `Dot fields -> fields.c_value
  | `EllipsisExpr fields -> fields.c_value
  | `Factor fields -> fields.c_value
  | `IfExpr fields -> fields.c_value
  | `InlineEval fields -> fields.c_value
  | `LambdaDef fields -> fields.c_value
  | `ListComp fields -> fields.c_value
  | `ListGen fields -> fields.c_value
  | `ListLit fields -> fields.c_value
  | `DottedName fields -> fields.c_value
  | `Id fields -> fields.c_value
  | `NotOp fields -> fields.c_value
  | `NumberLit fields -> fields.c_value
  | `OrExpr fields -> fields.c_value
  | `OrOp fields -> fields.c_value
  | `Power fields -> fields.c_value
  | `SetComp fields -> fields.c_value
  | `SetLit fields -> fields.c_value
  | `SliceExpr fields -> fields.c_value
  | `ExtSliceExpr fields -> fields.c_value
  | `StringLit fields -> fields.c_value
  | `SubscriptExpr fields -> fields.c_value
  | `TupleLit fields -> fields.c_value
  | `XorExpr fields -> fields.c_value
  | `YieldExpr fields -> fields.c_value
  | `FileNode fields -> fields.c_value
  | `ImportStar fields -> fields.c_value
  | `KwArgsFlagAbsent fields -> fields.c_value
  | `KwArgsFlagPresent fields -> fields.c_value
  | `NL fields -> fields.c_value
  | `Op fields -> fields.c_value
  | `Params fields -> fields.c_value
  | `RelName fields -> fields.c_value
  | `SingleParam fields -> fields.c_value
  | `AssertStmt fields -> fields.c_value
  | `AssignStmt fields -> fields.c_value
  | `AugAssignStmt fields -> fields.c_value
  | `BreakStmt fields -> fields.c_value
  | `ContinueStmt fields -> fields.c_value
  | `Decorated fields -> fields.c_value
  | `ClassDef fields -> fields.c_value
  | `FuncDef fields -> fields.c_value
  | `DelStmt fields -> fields.c_value
  | `ElifBranch fields -> fields.c_value
  | `ExecStmt fields -> fields.c_value
  | `ForStmt fields -> fields.c_value
  | `GlobalStmt fields -> fields.c_value
  | `IfStmt fields -> fields.c_value
  | `ImportFrom fields -> fields.c_value
  | `ImportName fields -> fields.c_value
  | `PassStmt fields -> fields.c_value
  | `PrintStmt fields -> fields.c_value
  | `RaiseStmt fields -> fields.c_value
  | `ReturnStmt fields -> fields.c_value
  | `StreamPrintStmt fields -> fields.c_value
  | `TryStmt fields -> fields.c_value
  | `WhileStmt fields -> fields.c_value
  | `WithStmt fields -> fields.c_value
  | `ArgList fields -> fields.c_value
  | `AsNameNodeList fields -> fields.c_value
  | `DecoratorList fields -> fields.c_value
  | `DictAssocList fields -> fields.c_value
  | `DotList fields -> fields.c_value
  | `ElifBranchList fields -> fields.c_value
  | `ExceptPartList fields -> fields.c_value
  | `ExprList fields -> fields.c_value
  | `IdList fields -> fields.c_value
  | `NLList fields -> fields.c_value
  | `SingleParamList fields -> fields.c_value
  | `StringLitList fields -> fields.c_value
  | `TurkixirNodeList fields -> fields.c_value
  | `VarArgsFlagAbsent fields -> fields.c_value
  | `VarArgsFlagPresent fields -> fields.c_value


   

  and unwrap_entity_info value =
    let c_value = make EntityInfoStruct.c_type in
    setf c_value
      EntityInfoStruct.rebindings
      (value.rebindings);
    setf c_value
      EntityInfoStruct.from_rebound
      (value.from_rebound);
    c_value




   




and unwrap_analysis_unit
  (unit : analysis_unit) = unit.c_value

let rec wrap_turkixir_node context c_value =
  (* Top level wrap function that dispatch to wrap function of concrete types
     depending on the node kind *)
  if is_null (getf c_value EntityStruct.node) then
    raise SyntaxError
  else
    let kind = CFunctions.node_kind (addr c_value) in
    match kind with
    | 1 ->
        (wrap_arg_assoc context (c_value)
         :> turkixir_node)
    | 2 ->
        (wrap_arg_gen context (c_value)
         :> turkixir_node)
    | 3 ->
        (wrap_kw_args context (c_value)
         :> turkixir_node)
    | 4 ->
        (wrap_var_args context (c_value)
         :> turkixir_node)
    | 5 ->
        (wrap_as_name_node context (c_value)
         :> turkixir_node)
    | 6 ->
        (wrap_comp_if context (c_value)
         :> turkixir_node)
    | 7 ->
        (wrap_comp_op_kind_diamond context (c_value)
         :> turkixir_node)
    | 8 ->
        (wrap_comp_op_kind_eq context (c_value)
         :> turkixir_node)
    | 9 ->
        (wrap_comp_op_kind_gt context (c_value)
         :> turkixir_node)
    | 10 ->
        (wrap_comp_op_kind_gte context (c_value)
         :> turkixir_node)
    | 11 ->
        (wrap_comp_op_kind_in context (c_value)
         :> turkixir_node)
    | 12 ->
        (wrap_comp_op_kind_is context (c_value)
         :> turkixir_node)
    | 13 ->
        (wrap_comp_op_kind_isnot context (c_value)
         :> turkixir_node)
    | 14 ->
        (wrap_comp_op_kind_lt context (c_value)
         :> turkixir_node)
    | 15 ->
        (wrap_comp_op_kind_lte context (c_value)
         :> turkixir_node)
    | 16 ->
        (wrap_comp_op_kind_noteq context (c_value)
         :> turkixir_node)
    | 17 ->
        (wrap_comp_op_kind_notin context (c_value)
         :> turkixir_node)
    | 18 ->
        (wrap_comp_for context (c_value)
         :> turkixir_node)
    | 19 ->
        (wrap_comp_forl context (c_value)
         :> turkixir_node)
    | 20 ->
        (wrap_decorator context (c_value)
         :> turkixir_node)
    | 21 ->
        (wrap_dict_assoc context (c_value)
         :> turkixir_node)
    | 22 ->
        (wrap_else_part context (c_value)
         :> turkixir_node)
    | 23 ->
        (wrap_except_part context (c_value)
         :> turkixir_node)
    | 24 ->
        (wrap_and_expr context (c_value)
         :> turkixir_node)
    | 25 ->
        (wrap_and_op context (c_value)
         :> turkixir_node)
    | 26 ->
        (wrap_arith_expr context (c_value)
         :> turkixir_node)
    | 27 ->
        (wrap_shift_expr context (c_value)
         :> turkixir_node)
    | 28 ->
        (wrap_term context (c_value)
         :> turkixir_node)
    | 29 ->
        (wrap_call_expr context (c_value)
         :> turkixir_node)
    | 30 ->
        (wrap_comp_op context (c_value)
         :> turkixir_node)
    | 31 ->
        (wrap_concat_string_lit context (c_value)
         :> turkixir_node)
    | 32 ->
        (wrap_dict_comp context (c_value)
         :> turkixir_node)
    | 33 ->
        (wrap_dict_lit context (c_value)
         :> turkixir_node)
    | 34 ->
        (wrap_dot context (c_value)
         :> turkixir_node)
    | 35 ->
        (wrap_ellipsis_expr context (c_value)
         :> turkixir_node)
    | 36 ->
        (wrap_factor context (c_value)
         :> turkixir_node)
    | 37 ->
        (wrap_if_expr context (c_value)
         :> turkixir_node)
    | 38 ->
        (wrap_inline_eval context (c_value)
         :> turkixir_node)
    | 39 ->
        (wrap_lambda_def context (c_value)
         :> turkixir_node)
    | 40 ->
        (wrap_list_comp context (c_value)
         :> turkixir_node)
    | 41 ->
        (wrap_list_gen context (c_value)
         :> turkixir_node)
    | 42 ->
        (wrap_list_lit context (c_value)
         :> turkixir_node)
    | 43 ->
        (wrap_dotted_name context (c_value)
         :> turkixir_node)
    | 44 ->
        (wrap_id context (c_value)
         :> turkixir_node)
    | 45 ->
        (wrap_not_op context (c_value)
         :> turkixir_node)
    | 46 ->
        (wrap_number_lit context (c_value)
         :> turkixir_node)
    | 47 ->
        (wrap_or_expr context (c_value)
         :> turkixir_node)
    | 48 ->
        (wrap_or_op context (c_value)
         :> turkixir_node)
    | 49 ->
        (wrap_power context (c_value)
         :> turkixir_node)
    | 50 ->
        (wrap_set_comp context (c_value)
         :> turkixir_node)
    | 51 ->
        (wrap_set_lit context (c_value)
         :> turkixir_node)
    | 52 ->
        (wrap_slice_expr context (c_value)
         :> turkixir_node)
    | 53 ->
        (wrap_ext_slice_expr context (c_value)
         :> turkixir_node)
    | 54 ->
        (wrap_string_lit context (c_value)
         :> turkixir_node)
    | 55 ->
        (wrap_subscript_expr context (c_value)
         :> turkixir_node)
    | 56 ->
        (wrap_tuple_lit context (c_value)
         :> turkixir_node)
    | 57 ->
        (wrap_xor_expr context (c_value)
         :> turkixir_node)
    | 58 ->
        (wrap_yield_expr context (c_value)
         :> turkixir_node)
    | 59 ->
        (wrap_file_node context (c_value)
         :> turkixir_node)
    | 60 ->
        (wrap_import_star context (c_value)
         :> turkixir_node)
    | 61 ->
        (wrap_kw_args_flag_absent context (c_value)
         :> turkixir_node)
    | 62 ->
        (wrap_kw_args_flag_present context (c_value)
         :> turkixir_node)
    | 63 ->
        (wrap_nl context (c_value)
         :> turkixir_node)
    | 64 ->
        (wrap_op context (c_value)
         :> turkixir_node)
    | 65 ->
        (wrap_params context (c_value)
         :> turkixir_node)
    | 66 ->
        (wrap_rel_name context (c_value)
         :> turkixir_node)
    | 67 ->
        (wrap_single_param context (c_value)
         :> turkixir_node)
    | 68 ->
        (wrap_assert_stmt context (c_value)
         :> turkixir_node)
    | 69 ->
        (wrap_assign_stmt context (c_value)
         :> turkixir_node)
    | 70 ->
        (wrap_aug_assign_stmt context (c_value)
         :> turkixir_node)
    | 71 ->
        (wrap_break_stmt context (c_value)
         :> turkixir_node)
    | 72 ->
        (wrap_continue_stmt context (c_value)
         :> turkixir_node)
    | 73 ->
        (wrap_decorated context (c_value)
         :> turkixir_node)
    | 74 ->
        (wrap_class_def context (c_value)
         :> turkixir_node)
    | 75 ->
        (wrap_func_def context (c_value)
         :> turkixir_node)
    | 76 ->
        (wrap_del_stmt context (c_value)
         :> turkixir_node)
    | 77 ->
        (wrap_elif_branch context (c_value)
         :> turkixir_node)
    | 78 ->
        (wrap_exec_stmt context (c_value)
         :> turkixir_node)
    | 79 ->
        (wrap_for_stmt context (c_value)
         :> turkixir_node)
    | 80 ->
        (wrap_global_stmt context (c_value)
         :> turkixir_node)
    | 81 ->
        (wrap_if_stmt context (c_value)
         :> turkixir_node)
    | 82 ->
        (wrap_import_from context (c_value)
         :> turkixir_node)
    | 83 ->
        (wrap_import_name context (c_value)
         :> turkixir_node)
    | 84 ->
        (wrap_pass_stmt context (c_value)
         :> turkixir_node)
    | 85 ->
        (wrap_print_stmt context (c_value)
         :> turkixir_node)
    | 86 ->
        (wrap_raise_stmt context (c_value)
         :> turkixir_node)
    | 87 ->
        (wrap_return_stmt context (c_value)
         :> turkixir_node)
    | 88 ->
        (wrap_stream_print_stmt context (c_value)
         :> turkixir_node)
    | 89 ->
        (wrap_try_stmt context (c_value)
         :> turkixir_node)
    | 90 ->
        (wrap_while_stmt context (c_value)
         :> turkixir_node)
    | 91 ->
        (wrap_with_stmt context (c_value)
         :> turkixir_node)
    | 92 ->
        (wrap_arg_list context (c_value)
         :> turkixir_node)
    | 93 ->
        (wrap_as_name_node_list context (c_value)
         :> turkixir_node)
    | 94 ->
        (wrap_decorator_list context (c_value)
         :> turkixir_node)
    | 95 ->
        (wrap_dict_assoc_list context (c_value)
         :> turkixir_node)
    | 96 ->
        (wrap_dot_list context (c_value)
         :> turkixir_node)
    | 97 ->
        (wrap_elif_branch_list context (c_value)
         :> turkixir_node)
    | 98 ->
        (wrap_except_part_list context (c_value)
         :> turkixir_node)
    | 99 ->
        (wrap_expr_list context (c_value)
         :> turkixir_node)
    | 100 ->
        (wrap_id_list context (c_value)
         :> turkixir_node)
    | 101 ->
        (wrap_nl_list context (c_value)
         :> turkixir_node)
    | 102 ->
        (wrap_single_param_list context (c_value)
         :> turkixir_node)
    | 103 ->
        (wrap_string_lit_list context (c_value)
         :> turkixir_node)
    | 104 ->
        (wrap_turkixir_node_list context (c_value)
         :> turkixir_node)
    | 105 ->
        (wrap_var_args_flag_absent context (c_value)
         :> turkixir_node)
    | 106 ->
        (wrap_var_args_flag_present context (c_value)
         :> turkixir_node)
    | _ -> assert false

      

  and wrap_arg context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `ArgAssoc _
      | `ArgGen _
      | `KwArgs _
      | `VarArgs _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_arg_assoc context c_value
   : arg_assoc =
    let f_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.arg_assoc_f_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.arg_assoc_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ArgAssoc {
        f_name
          = Lazy.from_fun f_name ;
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_arg_gen context c_value
   : arg_gen =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.arg_gen_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comprehension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.arg_gen_f_comprehension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_for context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ArgGen {
        f_expr
          = Lazy.from_fun f_expr ;
        f_comprehension
          = Lazy.from_fun f_comprehension ;
        c_value = c_value;
        context = context
      }

      


  and wrap_kw_args context c_value
   : kw_args =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.kw_args_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `KwArgs {
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_var_args context c_value
   : var_args =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.var_args_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `VarArgs {
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_as_name_node context c_value
   : as_name_node =
    let f_imported () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.as_name_node_f_imported
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_as_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.as_name_node_f_as_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AsNameNode {
        f_imported
          = Lazy.from_fun f_imported ;
        f_as_name
          = Lazy.from_fun f_as_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_if context c_value
   : comp_if =
    let f_test () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_if_f_test
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comp () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_if_f_comp
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_turkixir_node context (field_c_value))
      in
         

      match node with
            
      | Some `CompFor _
      | Some `CompForL _
      | Some `CompIf _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompIf {
        f_test
          = Lazy.from_fun f_test ;
        f_comp
          = Lazy.from_fun f_comp ;
        c_value = c_value;
        context = context
      }

      

  and wrap_comp_op_kind context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `CompOpKindDiamond _
      | `CompOpKindEq _
      | `CompOpKindGt _
      | `CompOpKindGte _
      | `CompOpKindIn _
      | `CompOpKindIs _
      | `CompOpKindIsnot _
      | `CompOpKindLt _
      | `CompOpKindLte _
      | `CompOpKindNoteq _
      | `CompOpKindNotin _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_comp_op_kind_diamond context c_value
   : comp_op_kind_diamond =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindDiamond {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_eq context c_value
   : comp_op_kind_eq =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindEq {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_gt context c_value
   : comp_op_kind_gt =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindGt {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_gte context c_value
   : comp_op_kind_gte =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindGte {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_in context c_value
   : comp_op_kind_in =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindIn {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_is context c_value
   : comp_op_kind_is =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindIs {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_isnot context c_value
   : comp_op_kind_isnot =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindIsnot {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_lt context c_value
   : comp_op_kind_lt =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindLt {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_lte context c_value
   : comp_op_kind_lte =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindLte {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_noteq context c_value
   : comp_op_kind_noteq =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindNoteq {
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op_kind_notin context c_value
   : comp_op_kind_notin =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOpKindNotin {
        c_value = c_value;
        context = context
      }

      

  and wrap_comprehension context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `CompFor _
      | `CompForL _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_comp_for context c_value
   : comp_for =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_for_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_target () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_for_f_target
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comp () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_for_f_comp
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_turkixir_node context (field_c_value))
      in
         

      match node with
            
      | Some `CompFor _
      | Some `CompIf _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompFor {
        f_exprs
          = Lazy.from_fun f_exprs ;
        f_target
          = Lazy.from_fun f_target ;
        f_comp
          = Lazy.from_fun f_comp ;
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_forl context c_value
   : comp_forl =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_forl_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_target () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_forl_f_target
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_comp () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_forl_f_comp
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_turkixir_node context (field_c_value))
      in
         

      match node with
            
      | Some `CompForL _
      | Some `CompIf _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompForL {
        f_exprs
          = Lazy.from_fun f_exprs ;
        f_target
          = Lazy.from_fun f_target ;
        f_comp
          = Lazy.from_fun f_comp ;
        c_value = c_value;
        context = context
      }

      


  and wrap_decorator context c_value
   : decorator =
    let f_dec_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.decorator_f_dec_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_name context (field_c_value)
      in
         

      node
    in
    let f_arg_list () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.decorator_f_arg_list
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_arg_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Decorator {
        f_dec_name
          = Lazy.from_fun f_dec_name ;
        f_arg_list
          = Lazy.from_fun f_arg_list ;
        c_value = c_value;
        context = context
      }

      


  and wrap_dict_assoc context c_value
   : dict_assoc =
    let f_key () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dict_assoc_f_key
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_value () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dict_assoc_f_value
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DictAssoc {
        f_key
          = Lazy.from_fun f_key ;
        f_value
          = Lazy.from_fun f_value ;
        c_value = c_value;
        context = context
      }

      


  and wrap_else_part context c_value
   : else_part =
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.else_part_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ElsePart {
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      


  and wrap_except_part context c_value
   : except_part =
    let f_as_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.except_part_f_as_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_as_name_node context (field_c_value))
      in
         

      node
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.except_part_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ExceptPart {
        f_as_name
          = Lazy.from_fun f_as_name ;
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      

  and wrap_expr context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `ShiftExpr _
      | `Term _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `Dot _
      | `EllipsisExpr _
      | `Factor _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `DottedName _
      | `Id _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `SliceExpr _
      | `ExtSliceExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_and_expr context c_value
   : and_expr =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.and_expr_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.and_expr_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AndExpr {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_and_op context c_value
   : and_op =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.and_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.and_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AndOp {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      

  and wrap_bin_op context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `ArithExpr _
      | `ShiftExpr _
      | `Term _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_arith_expr context c_value
   : arith_expr =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_op context (field_c_value)
      in
         

      node
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ArithExpr {
        f_left
          = Lazy.from_fun f_left ;
        f_op
          = Lazy.from_fun f_op ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_shift_expr context c_value
   : shift_expr =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_op context (field_c_value)
      in
         

      node
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ShiftExpr {
        f_left
          = Lazy.from_fun f_left ;
        f_op
          = Lazy.from_fun f_op ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_term context c_value
   : term =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_op context (field_c_value)
      in
         

      node
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.bin_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Term {
        f_left
          = Lazy.from_fun f_left ;
        f_op
          = Lazy.from_fun f_op ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_call_expr context c_value
   : call_expr =
    let f_prefix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.call_expr_f_prefix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_suffix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.call_expr_f_suffix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_arg_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CallExpr {
        f_prefix
          = Lazy.from_fun f_prefix ;
        f_suffix
          = Lazy.from_fun f_suffix ;
        c_value = c_value;
        context = context
      }

      


  and wrap_comp_op context c_value
   : comp_op =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_op_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_op_kind context (field_c_value)
      in
         

      node
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.comp_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `CompOp {
        f_left
          = Lazy.from_fun f_left ;
        f_op
          = Lazy.from_fun f_op ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_concat_string_lit context c_value
   : concat_string_lit =
    let f_first_str () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.concat_string_lit_f_first_str
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_lit context (field_c_value)
      in
         

      node
    in
    let f_subsequent_str () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.concat_string_lit_f_subsequent_str
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_string_lit_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ConcatStringLit {
        f_first_str
          = Lazy.from_fun f_first_str ;
        f_subsequent_str
          = Lazy.from_fun f_subsequent_str ;
        c_value = c_value;
        context = context
      }

      


  and wrap_dict_comp context c_value
   : dict_comp =
    let f_assoc () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dict_comp_f_assoc
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_dict_assoc context (field_c_value)
      in
         

      node
    in
    let f_comprehension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dict_comp_f_comprehension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_for context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DictComp {
        f_assoc
          = Lazy.from_fun f_assoc ;
        f_comprehension
          = Lazy.from_fun f_comprehension ;
        c_value = c_value;
        context = context
      }

      


  and wrap_dict_lit context c_value
   : dict_lit =
    let f_assocs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dict_lit_f_assocs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_dict_assoc_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DictLit {
        f_assocs
          = Lazy.from_fun f_assocs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_dot context c_value
   : dot =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Dot {
        c_value = c_value;
        context = context
      }

      


  and wrap_ellipsis_expr context c_value
   : ellipsis_expr =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `EllipsisExpr {
        c_value = c_value;
        context = context
      }

      


  and wrap_factor context c_value
   : factor =
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.factor_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_op context (field_c_value)
      in
         

      node
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.factor_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Factor {
        f_op
          = Lazy.from_fun f_op ;
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_if_expr context c_value
   : if_expr =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_expr_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_cond () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_expr_f_cond
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_else_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_expr_f_else_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `IfExpr {
        f_expr
          = Lazy.from_fun f_expr ;
        f_cond
          = Lazy.from_fun f_cond ;
        f_else_expr
          = Lazy.from_fun f_else_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_inline_eval context c_value
   : inline_eval =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.inline_eval_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `InlineEval {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_lambda_def context c_value
   : lambda_def =
    let f_args () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.lambda_def_f_args
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_params context (field_c_value)
      in
         

      node
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.lambda_def_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `LambdaDef {
        f_args
          = Lazy.from_fun f_args ;
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_list_comp context c_value
   : list_comp =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.list_comp_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comprehension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.list_comp_f_comprehension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_forl context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ListComp {
        f_expr
          = Lazy.from_fun f_expr ;
        f_comprehension
          = Lazy.from_fun f_comprehension ;
        c_value = c_value;
        context = context
      }

      


  and wrap_list_gen context c_value
   : list_gen =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.list_gen_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comprehension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.list_gen_f_comprehension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_forl context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ListGen {
        f_expr
          = Lazy.from_fun f_expr ;
        f_comprehension
          = Lazy.from_fun f_comprehension ;
        c_value = c_value;
        context = context
      }

      


  and wrap_list_lit context c_value
   : list_lit =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.list_lit_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ListLit {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      

  and wrap_name context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `DottedName _
      | `Id _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_dotted_name context c_value
   : dotted_name =
    let f_prefix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dotted_name_f_prefix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_suffix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.dotted_name_f_suffix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_id context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DottedName {
        f_prefix
          = Lazy.from_fun f_prefix ;
        f_suffix
          = Lazy.from_fun f_suffix ;
        c_value = c_value;
        context = context
      }

      


  and wrap_id context c_value
   : id =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Id {
        c_value = c_value;
        context = context
      }

      


  and wrap_not_op context c_value
   : not_op =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.not_op_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `NotOp {
        f_expr
          = Lazy.from_fun f_expr ;
        c_value = c_value;
        context = context
      }

      


  and wrap_number_lit context c_value
   : number_lit =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `NumberLit {
        c_value = c_value;
        context = context
      }

      


  and wrap_or_expr context c_value
   : or_expr =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.or_expr_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.or_expr_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `OrExpr {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_or_op context c_value
   : or_op =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.or_op_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.or_op_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `OrOp {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_power context c_value
   : power =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.power_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.power_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Power {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_set_comp context c_value
   : set_comp =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.set_comp_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_comprehension () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.set_comp_f_comprehension
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_comp_for context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SetComp {
        f_expr
          = Lazy.from_fun f_expr ;
        f_comprehension
          = Lazy.from_fun f_comprehension ;
        c_value = c_value;
        context = context
      }

      


  and wrap_set_lit context c_value
   : set_lit =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.set_lit_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SetLit {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_slice_expr context c_value
   : slice_expr =
    let f_first () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.slice_expr_f_first
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    let f_last () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.slice_expr_f_last
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SliceExpr {
        f_first
          = Lazy.from_fun f_first ;
        f_last
          = Lazy.from_fun f_last ;
        c_value = c_value;
        context = context
      }

      


  and wrap_ext_slice_expr context c_value
   : ext_slice_expr =
    let f_first () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.slice_expr_f_first
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    let f_last () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.slice_expr_f_last
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    let f_stride () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.ext_slice_expr_f_stride
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ExtSliceExpr {
        f_first
          = Lazy.from_fun f_first ;
        f_last
          = Lazy.from_fun f_last ;
        f_stride
          = Lazy.from_fun f_stride ;
        c_value = c_value;
        context = context
      }

      


  and wrap_string_lit context c_value
   : string_lit =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `StringLit {
        c_value = c_value;
        context = context
      }

      


  and wrap_subscript_expr context c_value
   : subscript_expr =
    let f_prefix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.subscript_expr_f_prefix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `SetComp _
      | `SetLit _
      | `StringLit _
      | `SubscriptExpr _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_suffix () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.subscript_expr_f_suffix
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SubscriptExpr {
        f_prefix
          = Lazy.from_fun f_prefix ;
        f_suffix
          = Lazy.from_fun f_suffix ;
        c_value = c_value;
        context = context
      }

      


  and wrap_tuple_lit context c_value
   : tuple_lit =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.tuple_lit_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `TupleLit {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_xor_expr context c_value
   : xor_expr =
    let f_left () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.xor_expr_f_left
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_right () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.xor_expr_f_right
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `XorExpr {
        f_left
          = Lazy.from_fun f_left ;
        f_right
          = Lazy.from_fun f_right ;
        c_value = c_value;
        context = context
      }

      


  and wrap_yield_expr context c_value
   : yield_expr =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.yield_expr_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `YieldExpr {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_file_node context c_value
   : file_node =
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.file_node_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `FileNode {
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      


  and wrap_import_star context c_value
   : import_star =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ImportStar {
        c_value = c_value;
        context = context
      }

      

  and wrap_kw_args_flag context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `KwArgsFlagAbsent _
      | `KwArgsFlagPresent _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_kw_args_flag_absent context c_value
   : kw_args_flag_absent =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `KwArgsFlagAbsent {
        c_value = c_value;
        context = context
      }

      


  and wrap_kw_args_flag_present context c_value
   : kw_args_flag_present =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `KwArgsFlagPresent {
        c_value = c_value;
        context = context
      }

      


  and wrap_nl context c_value
   : nl =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `NL {
        c_value = c_value;
        context = context
      }

      


  and wrap_op context c_value
   : op =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Op {
        c_value = c_value;
        context = context
      }

      


  and wrap_params context c_value
   : params =
    let f_single_params () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.params_f_single_params
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_single_param_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Params {
        f_single_params
          = Lazy.from_fun f_single_params ;
        c_value = c_value;
        context = context
      }

      


  and wrap_rel_name context c_value
   : rel_name =
    let f_dots () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.rel_name_f_dots
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_dot_list context (field_c_value)
      in
         

      node
    in
    let f_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.rel_name_f_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_name context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `RelName {
        f_dots
          = Lazy.from_fun f_dots ;
        f_name
          = Lazy.from_fun f_name ;
        c_value = c_value;
        context = context
      }

      


  and wrap_single_param context c_value
   : single_param =
    let f_is_varargs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.single_param_f_is_varargs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_var_args_flag context (field_c_value)
      in
         

      node
    in
    let f_is_kwargs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.single_param_f_is_kwargs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_kw_args_flag context (field_c_value)
      in
         

      node
    in
    let f_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.single_param_f_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `Id _
      | `IdList _
      as e -> e
      | _ -> assert false
    in
    let f_default_value () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.single_param_f_default_value
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SingleParam {
        f_is_varargs
          = Lazy.from_fun f_is_varargs ;
        f_is_kwargs
          = Lazy.from_fun f_is_kwargs ;
        f_name
          = Lazy.from_fun f_name ;
        f_default_value
          = Lazy.from_fun f_default_value ;
        c_value = c_value;
        context = context
      }

      

  and wrap_stmt context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `Decorated _
      | `ClassDef _
      | `FuncDef _
      | `DelStmt _
      | `ElifBranch _
      | `ExecStmt _
      | `ForStmt _
      | `GlobalStmt _
      | `IfStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TryStmt _
      | `WhileStmt _
      | `WithStmt _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_assert_stmt context c_value
   : assert_stmt =
    let f_test_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.assert_stmt_f_test_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_msg () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.assert_stmt_f_msg
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr context (field_c_value))
      in
         

      match node with
            
      | Some `AndExpr _
      | Some `AndOp _
      | Some `ArithExpr _
      | Some `CallExpr _
      | Some `CompOp _
      | Some `ConcatStringLit _
      | Some `DictComp _
      | Some `DictLit _
      | Some `DottedName _
      | Some `Factor _
      | Some `Id _
      | Some `IfExpr _
      | Some `InlineEval _
      | Some `LambdaDef _
      | Some `ListComp _
      | Some `ListGen _
      | Some `ListLit _
      | Some `NotOp _
      | Some `NumberLit _
      | Some `OrExpr _
      | Some `OrOp _
      | Some `Power _
      | Some `SetComp _
      | Some `SetLit _
      | Some `ShiftExpr _
      | Some `StringLit _
      | Some `SubscriptExpr _
      | Some `Term _
      | Some `TupleLit _
      | Some `XorExpr _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AssertStmt {
        f_test_expr
          = Lazy.from_fun f_test_expr ;
        f_msg
          = Lazy.from_fun f_msg ;
        c_value = c_value;
        context = context
      }

      


  and wrap_assign_stmt context c_value
   : assign_stmt =
    let f_l_value () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.assign_stmt_f_l_value
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_r_values () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.assign_stmt_f_r_values
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AssignStmt {
        f_l_value
          = Lazy.from_fun f_l_value ;
        f_r_values
          = Lazy.from_fun f_r_values ;
        c_value = c_value;
        context = context
      }

      


  and wrap_aug_assign_stmt context c_value
   : aug_assign_stmt =
    let f_l_value () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.aug_assign_stmt_f_l_value
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_op () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.aug_assign_stmt_f_op
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_op context (field_c_value)
      in
         

      node
    in
    let f_r_value () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.aug_assign_stmt_f_r_value
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `ExprList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AugAssignStmt {
        f_l_value
          = Lazy.from_fun f_l_value ;
        f_op
          = Lazy.from_fun f_op ;
        f_r_value
          = Lazy.from_fun f_r_value ;
        c_value = c_value;
        context = context
      }

      


  and wrap_break_stmt context c_value
   : break_stmt =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `BreakStmt {
        c_value = c_value;
        context = context
      }

      


  and wrap_continue_stmt context c_value
   : continue_stmt =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ContinueStmt {
        c_value = c_value;
        context = context
      }

      


  and wrap_decorated context c_value
   : decorated =
    let f_decorators () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.decorated_f_decorators
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_decorator_list context (field_c_value)
      in
         

      node
    in
    let f_defn () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.decorated_f_defn
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_def_stmt context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `Decorated {
        f_decorators
          = Lazy.from_fun f_decorators ;
        f_defn
          = Lazy.from_fun f_defn ;
        c_value = c_value;
        context = context
      }

      

  and wrap_def_stmt context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `ClassDef _
      | `FuncDef _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_class_def context c_value
   : class_def =
    let f_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.class_def_f_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_id context (field_c_value)
      in
         

      node
    in
    let f_bases () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.class_def_f_bases
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.class_def_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ClassDef {
        f_name
          = Lazy.from_fun f_name ;
        f_bases
          = Lazy.from_fun f_bases ;
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      


  and wrap_func_def context c_value
   : func_def =
    let f_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.func_def_f_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_id context (field_c_value)
      in
         

      node
    in
    let f_parameters () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.func_def_f_parameters
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_params context (field_c_value))
      in
         

      node
    in
    let f_body () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.func_def_f_body
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `FuncDef {
        f_name
          = Lazy.from_fun f_name ;
        f_parameters
          = Lazy.from_fun f_parameters ;
        f_body
          = Lazy.from_fun f_body ;
        c_value = c_value;
        context = context
      }

      


  and wrap_del_stmt context c_value
   : del_stmt =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.del_stmt_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DelStmt {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_elif_branch context c_value
   : elif_branch =
    let f_cond_test () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.elif_branch_f_cond_test
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.elif_branch_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ElifBranch {
        f_cond_test
          = Lazy.from_fun f_cond_test ;
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      


  and wrap_exec_stmt context c_value
   : exec_stmt =
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.exec_stmt_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `ArithExpr _
      | `CallExpr _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `InlineEval _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NumberLit _
      | `OrExpr _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_in_list () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.exec_stmt_f_in_list
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ExecStmt {
        f_expr
          = Lazy.from_fun f_expr ;
        f_in_list
          = Lazy.from_fun f_in_list ;
        c_value = c_value;
        context = context
      }

      


  and wrap_for_stmt context c_value
   : for_stmt =
    let f_bindings () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.for_stmt_f_bindings
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.for_stmt_f_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.for_stmt_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_else_part () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.for_stmt_f_else_part
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_else_part context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ForStmt {
        f_bindings
          = Lazy.from_fun f_bindings ;
        f_expr
          = Lazy.from_fun f_expr ;
        f_statements
          = Lazy.from_fun f_statements ;
        f_else_part
          = Lazy.from_fun f_else_part ;
        c_value = c_value;
        context = context
      }

      


  and wrap_global_stmt context c_value
   : global_stmt =
    let f_names () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.global_stmt_f_names
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_id_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `GlobalStmt {
        f_names
          = Lazy.from_fun f_names ;
        c_value = c_value;
        context = context
      }

      


  and wrap_if_stmt context c_value
   : if_stmt =
    let f_cond_test () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_stmt_f_cond_test
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_stmt_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_elif_branchs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_stmt_f_elif_branchs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_elif_branch_list context (field_c_value)
      in
         

      node
    in
    let f_else_part () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.if_stmt_f_else_part
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_else_part context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `IfStmt {
        f_cond_test
          = Lazy.from_fun f_cond_test ;
        f_statements
          = Lazy.from_fun f_statements ;
        f_elif_branchs
          = Lazy.from_fun f_elif_branchs ;
        f_else_part
          = Lazy.from_fun f_else_part ;
        c_value = c_value;
        context = context
      }

      


  and wrap_import_from context c_value
   : import_from =
    let f_rel_name () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.import_from_f_rel_name
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `DottedName _
      | `Id _
      | `RelName _
      as e -> e
      | _ -> assert false
    in
    let f_imported () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.import_from_f_imported
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `ImportStar _
      | `TurkixirNodeList _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ImportFrom {
        f_rel_name
          = Lazy.from_fun f_rel_name ;
        f_imported
          = Lazy.from_fun f_imported ;
        c_value = c_value;
        context = context
      }

      


  and wrap_import_name context c_value
   : import_name =
    let f_imported_names () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.import_name_f_imported_names
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ImportName {
        f_imported_names
          = Lazy.from_fun f_imported_names ;
        c_value = c_value;
        context = context
      }

      


  and wrap_pass_stmt context c_value
   : pass_stmt =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `PassStmt {
        c_value = c_value;
        context = context
      }

      


  and wrap_print_stmt context c_value
   : print_stmt =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.print_stmt_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `PrintStmt {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_raise_stmt context c_value
   : raise_stmt =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.raise_stmt_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `RaiseStmt {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_return_stmt context c_value
   : return_stmt =
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.return_stmt_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_expr_list context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ReturnStmt {
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_stream_print_stmt context c_value
   : stream_print_stmt =
    let f_stream_expr () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.stream_print_stmt_f_stream_expr
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_exprs () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.stream_print_stmt_f_exprs
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr_list context (field_c_value)
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `StreamPrintStmt {
        f_stream_expr
          = Lazy.from_fun f_stream_expr ;
        f_exprs
          = Lazy.from_fun f_exprs ;
        c_value = c_value;
        context = context
      }

      


  and wrap_try_stmt context c_value
   : try_stmt =
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.try_stmt_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_except_parts () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.try_stmt_f_except_parts
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_except_part_list context (field_c_value)
      in
         

      node
    in
    let f_else_part () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.try_stmt_f_else_part
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_else_part context (field_c_value))
      in
         

      node
    in
    let f_finally_part () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.try_stmt_f_finally_part
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_turkixir_node context (field_c_value))
      in
         

      match node with
            
      | Some `AssertStmt _
      | Some `AssignStmt _
      | Some `AugAssignStmt _
      | Some `BreakStmt _
      | Some `ContinueStmt _
      | Some `DelStmt _
      | Some `ExecStmt _
      | Some `ExprList _
      | Some `GlobalStmt _
      | Some `ImportFrom _
      | Some `ImportName _
      | Some `PassStmt _
      | Some `PrintStmt _
      | Some `RaiseStmt _
      | Some `ReturnStmt _
      | Some `StreamPrintStmt _
      | Some `TurkixirNodeList _
      | Some `YieldExpr _
      | None as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `TryStmt {
        f_statements
          = Lazy.from_fun f_statements ;
        f_except_parts
          = Lazy.from_fun f_except_parts ;
        f_else_part
          = Lazy.from_fun f_else_part ;
        f_finally_part
          = Lazy.from_fun f_finally_part ;
        c_value = c_value;
        context = context
      }

      


  and wrap_while_stmt context c_value
   : while_stmt =
    let f_cond_test () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.while_stmt_f_cond_test
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_expr context (field_c_value)
      in
         

      match node with
            
      | `AndExpr _
      | `AndOp _
      | `ArithExpr _
      | `CallExpr _
      | `CompOp _
      | `ConcatStringLit _
      | `DictComp _
      | `DictLit _
      | `DottedName _
      | `Factor _
      | `Id _
      | `IfExpr _
      | `InlineEval _
      | `LambdaDef _
      | `ListComp _
      | `ListGen _
      | `ListLit _
      | `NotOp _
      | `NumberLit _
      | `OrExpr _
      | `OrOp _
      | `Power _
      | `SetComp _
      | `SetLit _
      | `ShiftExpr _
      | `StringLit _
      | `SubscriptExpr _
      | `Term _
      | `TupleLit _
      | `XorExpr _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.while_stmt_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    let f_else_part () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.while_stmt_f_else_part
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         if is_null (getf field_c_value EntityStruct.node) then None else Some (wrap_else_part context (field_c_value))
      in
         

      node
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `WhileStmt {
        f_cond_test
          = Lazy.from_fun f_cond_test ;
        f_statements
          = Lazy.from_fun f_statements ;
        f_else_part
          = Lazy.from_fun f_else_part ;
        c_value = c_value;
        context = context
      }

      


  and wrap_with_stmt context c_value
   : with_stmt =
    let f_bindings () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.with_stmt_f_bindings
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_as_name_node_list context (field_c_value)
      in
         

      node
    in
    let f_statements () =
      let field_c_value = make EntityStruct.c_type in
      let _ : int = CFunctions.with_stmt_f_statements
        (addr c_value)
        (addr field_c_value)
      in
      let node =
         wrap_turkixir_node context (field_c_value)
      in
         

      match node with
            
      | `AssertStmt _
      | `AssignStmt _
      | `AugAssignStmt _
      | `BreakStmt _
      | `ContinueStmt _
      | `DelStmt _
      | `ExecStmt _
      | `ExprList _
      | `GlobalStmt _
      | `ImportFrom _
      | `ImportName _
      | `PassStmt _
      | `PrintStmt _
      | `RaiseStmt _
      | `ReturnStmt _
      | `StreamPrintStmt _
      | `TurkixirNodeList _
      | `YieldExpr _
      as e -> e
      | _ -> assert false
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `WithStmt {
        f_bindings
          = Lazy.from_fun f_bindings ;
        f_statements
          = Lazy.from_fun f_statements ;
        c_value = c_value;
        context = context
      }

      

  and wrap_turkixir_node_base_list context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `ArgList _
      | `AsNameNodeList _
      | `DecoratorList _
      | `DictAssocList _
      | `DotList _
      | `ElifBranchList _
      | `ExceptPartList _
      | `ExprList _
      | `IdList _
      | `NLList _
      | `SingleParamList _
      | `StringLitList _
      | `TurkixirNodeList _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_arg_list context c_value
   : arg_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_arg context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ArgList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_as_name_node_list context c_value
   : as_name_node_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_as_name_node context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `AsNameNodeList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_decorator_list context c_value
   : decorator_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_decorator context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DecoratorList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_dict_assoc_list context c_value
   : dict_assoc_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_dict_assoc context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DictAssocList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_dot_list context c_value
   : dot_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_dot context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `DotList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_elif_branch_list context c_value
   : elif_branch_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_elif_branch context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ElifBranchList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_except_part_list context c_value
   : except_part_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_except_part context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ExceptPartList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_expr_list context c_value
   : expr_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_expr context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `ExprList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_id_list context c_value
   : id_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_id context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `IdList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_nl_list context c_value
   : nl_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_nl context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `NLList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_single_param_list context c_value
   : single_param_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_single_param context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `SingleParamList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_string_lit_list context c_value
   : string_lit_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_string_lit context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `StringLitList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      


  and wrap_turkixir_node_list context c_value
   : turkixir_node_list =
    let list () =
      let c_value_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      let _ : int =
        CFunctions.turkixir_node_children
          (addr c_value)
          (c_value_ptr)
      in
      let c_value = !@(!@(c_value_ptr)) in
      let length = getf c_value TurkixirNodeArrayStruct.n in
      let items = c_value @. TurkixirNodeArrayStruct.items in
      let f i =
        let fresh = allocate EntityStruct.c_type !@(items +@ i) in
        (* This can raise a SyntaxError, which is expected *)
        wrap_turkixir_node context ((!@ fresh))
      in
      let result = List.init length f in
      TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
      result
    in
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `TurkixirNodeList {
        list = Lazy.from_fun list;
        c_value = c_value;
        context = context
      }

      

  and wrap_var_args_flag context c_value =
    (* This is an abstract node, call the root wrap function and filter to get
     the desired type *)
    match wrap_turkixir_node context (c_value) with
      | `VarArgsFlagAbsent _
      | `VarArgsFlagPresent _
      as e -> e
      | _ ->
          (* This should not happen if the types are correct *)
          assert false

      


  and wrap_var_args_flag_absent context c_value
   : var_args_flag_absent =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `VarArgsFlagAbsent {
        c_value = c_value;
        context = context
      }

      


  and wrap_var_args_flag_present context c_value
   : var_args_flag_present =
    if is_null (getf c_value EntityStruct.node) then
      raise SyntaxError
    else
      `VarArgsFlagPresent {
        c_value = c_value;
        context = context
      }



   
  and wrap_entity_info c_value = {
    rebindings = (getf c_value EntityInfoStruct.rebindings);
    from_rebound = (getf c_value EntityInfoStruct.from_rebound);
  }



   


and wrap_analysis_unit context c_value
   : analysis_unit = {
 c_value=c_value;
 context=context;
}

module Entity = struct
  type t = entity

  let info value =
    wrap_entity_info (getf value EntityStruct.info)

  let compare e1 e2 =
    let open Stdlib in
    let compare_node =
      compare (getf e1 EntityStruct.node) (getf e2 EntityStruct.node)
    in
    if compare_node = 0 then
      compare
        (getf (getf e1 EntityStruct.info) EntityInfoStruct.rebindings)
        (getf (getf e2 EntityStruct.info) EntityInfoStruct.rebindings)
    else
      compare_node

  let equal e1 e2 =
    compare e1 e2 = 0

  let hash e =
    Hashtbl.hash
      ( getf e EntityStruct.node
      , getf (getf e EntityStruct.info) EntityInfoStruct.rebindings )
end

module AnalysisUnit = struct
  type t = analysis_unit

  let root (unit : t) =
    let c_value = make EntityStruct.c_type in
    AnalysisUnitStruct.unit_root
      (unwrap_analysis_unit (unit))
      (addr c_value);
    if is_null (getf c_value EntityStruct.node) then None else Some (wrap_turkixir_node unit.context (c_value))

  let diagnostics (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let length = AnalysisUnitStruct.unit_diagnostic_count c_unit in
    let f i =
      let diag = allocate_n Diagnostic.c_type ~count:1 in
      let _ : int = AnalysisUnitStruct.unit_diagnostic c_unit i diag in
      !@ diag
    in
    List.init length f

  let filename (unit : t) =
    unwrap_str( AnalysisUnitStruct.unit_filename
      (unwrap_analysis_unit (unit)))

  let reparse ?charset:(charset="") ?buffer (unit : t) =
    match buffer with
    | None ->
        ignore
          (AnalysisUnitStruct.unit_reparse_from_file unit.c_value charset)
    | Some buffer ->
        ignore (AnalysisUnitStruct.unit_reparse_from_buffer unit.c_value
          charset buffer (Unsigned.Size_t.of_int (String.length buffer)))

  let first_token (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_first_token c_unit result_ptr ;
    !@ result_ptr

  let last_token (unit : t) =
    let c_unit = unwrap_analysis_unit (unit) in
    let result_ptr = allocate_n Token.c_type ~count:1 in
    AnalysisUnitStruct.unit_last_token c_unit result_ptr ;
    !@ result_ptr

  let token_count (unit : t) =
    AnalysisUnitStruct.unit_token_count
      (unwrap_analysis_unit (unit))

  let trivia_count (unit : t) =
    AnalysisUnitStruct.unit_trivia_count
      (unwrap_analysis_unit (unit))

  
  let fold_tokens f init node =
    let tok_start = first_token node in
    let tok_end = last_token node in
    let rec aux acc tok_curr =
      let new_acc = f acc tok_curr in
      if Token.equal tok_curr tok_end then
        new_acc
      else
        aux new_acc (Token.next tok_curr)
    in
    aux init tok_start

  let iter_tokens f node =
    let tok_start = first_token node in
    let tok_end = last_token node in
    let rec aux tok_curr =
      f tok_curr;
      if not (Token.equal tok_curr tok_end) then
        aux (Token.next tok_curr)
    in
    aux tok_start

  let map_tokens f node =
    let tok_start = first_token node in
    let tok_end = last_token node in
    let rec aux tok_curr =
      let value = f tok_curr in
      if Token.equal tok_curr tok_end then
        [value]
      else
        value :: aux (Token.next tok_curr)
    in
    aux tok_start

  let tokens node =
    map_tokens (fun x -> x) node

end

module AnalysisContext = struct
  type t = analysis_context

  let create
    ?charset:(charset="")
    ?with_trivia:(with_trivia=true)
    ?tab_stop:(tab_stop=8)
    ?unit_provider:(unit_provider=UnitProvider.null) () : t =
    if tab_stop < 1 then
      raise (Invalid_argument "Invalid tab_stop (positive integer expected)") ;
    let c_context =
       AnalysisContextStruct.create_analysis_context
         charset
         Ctypes.null (* TODO: bind the file readers API to OCaml *)
         (!@unit_provider)
         Ctypes.null (* TODO: bind the event handlers API to OCaml *)
         with_trivia
         tab_stop
    in
    { c_value= c_context
      ; unit_provider= unit_provider }

  let get_from_file
    ?charset:(charset="")
    ?reparse:(reparse=false)
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename : AnalysisUnit.t =

    wrap_analysis_unit ctx (AnalysisContextStruct.get_analysis_unit_from_file ctx.c_value filename charset reparse grammar_rule)

  let get_from_buffer
    ?charset:(charset="")
    ?grammar_rule:(grammar_rule=default_grammar_rule)
    (ctx : t)
    filename
    buffer : AnalysisUnit.t =

    wrap_analysis_unit ctx (AnalysisContextStruct.get_analysis_unit_from_buffer ctx.c_value filename charset buffer (Unsigned.Size_t.of_int (String.length buffer)) grammar_rule)
end

   
module TurkixirNodeArray : sig
   
  type t = turkixir_node list

  val wrap : analysis_context -> TurkixirNodeArrayStruct.t structure ptr -> t

  val unwrap : t -> TurkixirNodeArrayStruct.t structure ptr

end = struct
   
  type t = turkixir_node list

  let wrap (context : analysis_context) c_value_ptr =
    let c_value = !@ c_value_ptr in
    let length = getf c_value TurkixirNodeArrayStruct.n in
    let items = c_value @. TurkixirNodeArrayStruct.items in
    let f i =
      (* we want to allocate a fresh value for a record, otherwize, the c value
       * will still point to the memory at array location *)
      let fresh =
        allocate EntityStruct.c_type (!@ (items +@ i))
      in
      wrap_turkixir_node context (!@ fresh)
    in
    let result = List.init length f in
    TurkixirNodeArrayStruct.dec_ref c_value_ptr;
    result

  let unwrap value =
    let result = TurkixirNodeArrayStruct.create (List.length value) in
    let items = result |-> TurkixirNodeArrayStruct.items in
    let f i v =
      items +@ i <-@
        unwrap_turkixir_node (v)
    in
    List.iteri f value;
    result


end


let context node =
  (* Given any node, extract the context field *)
  match (node :> turkixir_node) with
  | `ArgAssoc fields -> fields.context
  | `ArgGen fields -> fields.context
  | `KwArgs fields -> fields.context
  | `VarArgs fields -> fields.context
  | `AsNameNode fields -> fields.context
  | `CompIf fields -> fields.context
  | `CompOpKindDiamond fields -> fields.context
  | `CompOpKindEq fields -> fields.context
  | `CompOpKindGt fields -> fields.context
  | `CompOpKindGte fields -> fields.context
  | `CompOpKindIn fields -> fields.context
  | `CompOpKindIs fields -> fields.context
  | `CompOpKindIsnot fields -> fields.context
  | `CompOpKindLt fields -> fields.context
  | `CompOpKindLte fields -> fields.context
  | `CompOpKindNoteq fields -> fields.context
  | `CompOpKindNotin fields -> fields.context
  | `CompFor fields -> fields.context
  | `CompForL fields -> fields.context
  | `Decorator fields -> fields.context
  | `DictAssoc fields -> fields.context
  | `ElsePart fields -> fields.context
  | `ExceptPart fields -> fields.context
  | `AndExpr fields -> fields.context
  | `AndOp fields -> fields.context
  | `ArithExpr fields -> fields.context
  | `ShiftExpr fields -> fields.context
  | `Term fields -> fields.context
  | `CallExpr fields -> fields.context
  | `CompOp fields -> fields.context
  | `ConcatStringLit fields -> fields.context
  | `DictComp fields -> fields.context
  | `DictLit fields -> fields.context
  | `Dot fields -> fields.context
  | `EllipsisExpr fields -> fields.context
  | `Factor fields -> fields.context
  | `IfExpr fields -> fields.context
  | `InlineEval fields -> fields.context
  | `LambdaDef fields -> fields.context
  | `ListComp fields -> fields.context
  | `ListGen fields -> fields.context
  | `ListLit fields -> fields.context
  | `DottedName fields -> fields.context
  | `Id fields -> fields.context
  | `NotOp fields -> fields.context
  | `NumberLit fields -> fields.context
  | `OrExpr fields -> fields.context
  | `OrOp fields -> fields.context
  | `Power fields -> fields.context
  | `SetComp fields -> fields.context
  | `SetLit fields -> fields.context
  | `SliceExpr fields -> fields.context
  | `ExtSliceExpr fields -> fields.context
  | `StringLit fields -> fields.context
  | `SubscriptExpr fields -> fields.context
  | `TupleLit fields -> fields.context
  | `XorExpr fields -> fields.context
  | `YieldExpr fields -> fields.context
  | `FileNode fields -> fields.context
  | `ImportStar fields -> fields.context
  | `KwArgsFlagAbsent fields -> fields.context
  | `KwArgsFlagPresent fields -> fields.context
  | `NL fields -> fields.context
  | `Op fields -> fields.context
  | `Params fields -> fields.context
  | `RelName fields -> fields.context
  | `SingleParam fields -> fields.context
  | `AssertStmt fields -> fields.context
  | `AssignStmt fields -> fields.context
  | `AugAssignStmt fields -> fields.context
  | `BreakStmt fields -> fields.context
  | `ContinueStmt fields -> fields.context
  | `Decorated fields -> fields.context
  | `ClassDef fields -> fields.context
  | `FuncDef fields -> fields.context
  | `DelStmt fields -> fields.context
  | `ElifBranch fields -> fields.context
  | `ExecStmt fields -> fields.context
  | `ForStmt fields -> fields.context
  | `GlobalStmt fields -> fields.context
  | `IfStmt fields -> fields.context
  | `ImportFrom fields -> fields.context
  | `ImportName fields -> fields.context
  | `PassStmt fields -> fields.context
  | `PrintStmt fields -> fields.context
  | `RaiseStmt fields -> fields.context
  | `ReturnStmt fields -> fields.context
  | `StreamPrintStmt fields -> fields.context
  | `TryStmt fields -> fields.context
  | `WhileStmt fields -> fields.context
  | `WithStmt fields -> fields.context
  | `ArgList fields -> fields.context
  | `AsNameNodeList fields -> fields.context
  | `DecoratorList fields -> fields.context
  | `DictAssocList fields -> fields.context
  | `DotList fields -> fields.context
  | `ElifBranchList fields -> fields.context
  | `ExceptPartList fields -> fields.context
  | `ExprList fields -> fields.context
  | `IdList fields -> fields.context
  | `NLList fields -> fields.context
  | `SingleParamList fields -> fields.context
  | `StringLitList fields -> fields.context
  | `TurkixirNodeList fields -> fields.context
  | `VarArgsFlagAbsent fields -> fields.context
  | `VarArgsFlagPresent fields -> fields.context

type _ node =
  | TurkixirNode :
      turkixir_node node
  | Arg :
      arg node
  | ArgAssoc :
      arg_assoc node
  | ArgGen :
      arg_gen node
  | KwArgs :
      kw_args node
  | VarArgs :
      var_args node
  | AsNameNode :
      as_name_node node
  | CompIf :
      comp_if node
  | CompOpKind :
      comp_op_kind node
  | CompOpKindDiamond :
      comp_op_kind_diamond node
  | CompOpKindEq :
      comp_op_kind_eq node
  | CompOpKindGt :
      comp_op_kind_gt node
  | CompOpKindGte :
      comp_op_kind_gte node
  | CompOpKindIn :
      comp_op_kind_in node
  | CompOpKindIs :
      comp_op_kind_is node
  | CompOpKindIsnot :
      comp_op_kind_isnot node
  | CompOpKindLt :
      comp_op_kind_lt node
  | CompOpKindLte :
      comp_op_kind_lte node
  | CompOpKindNoteq :
      comp_op_kind_noteq node
  | CompOpKindNotin :
      comp_op_kind_notin node
  | Comprehension :
      comprehension node
  | CompFor :
      comp_for node
  | CompForL :
      comp_forl node
  | Decorator :
      decorator node
  | DictAssoc :
      dict_assoc node
  | ElsePart :
      else_part node
  | ExceptPart :
      except_part node
  | Expr :
      expr node
  | AndExpr :
      and_expr node
  | AndOp :
      and_op node
  | BinOp :
      bin_op node
  | ArithExpr :
      arith_expr node
  | ShiftExpr :
      shift_expr node
  | Term :
      term node
  | CallExpr :
      call_expr node
  | CompOp :
      comp_op node
  | ConcatStringLit :
      concat_string_lit node
  | DictComp :
      dict_comp node
  | DictLit :
      dict_lit node
  | Dot :
      dot node
  | EllipsisExpr :
      ellipsis_expr node
  | Factor :
      factor node
  | IfExpr :
      if_expr node
  | InlineEval :
      inline_eval node
  | LambdaDef :
      lambda_def node
  | ListComp :
      list_comp node
  | ListGen :
      list_gen node
  | ListLit :
      list_lit node
  | Name :
      name node
  | DottedName :
      dotted_name node
  | Id :
      id node
  | NotOp :
      not_op node
  | NumberLit :
      number_lit node
  | OrExpr :
      or_expr node
  | OrOp :
      or_op node
  | Power :
      power node
  | SetComp :
      set_comp node
  | SetLit :
      set_lit node
  | SliceExpr :
      slice_expr node
  | ExtSliceExpr :
      ext_slice_expr node
  | StringLit :
      string_lit node
  | SubscriptExpr :
      subscript_expr node
  | TupleLit :
      tuple_lit node
  | XorExpr :
      xor_expr node
  | YieldExpr :
      yield_expr node
  | FileNode :
      file_node node
  | ImportStar :
      import_star node
  | KwArgsFlag :
      kw_args_flag node
  | KwArgsFlagAbsent :
      kw_args_flag_absent node
  | KwArgsFlagPresent :
      kw_args_flag_present node
  | NL :
      nl node
  | Op :
      op node
  | Params :
      params node
  | RelName :
      rel_name node
  | SingleParam :
      single_param node
  | Stmt :
      stmt node
  | AssertStmt :
      assert_stmt node
  | AssignStmt :
      assign_stmt node
  | AugAssignStmt :
      aug_assign_stmt node
  | BreakStmt :
      break_stmt node
  | ContinueStmt :
      continue_stmt node
  | Decorated :
      decorated node
  | DefStmt :
      def_stmt node
  | ClassDef :
      class_def node
  | FuncDef :
      func_def node
  | DelStmt :
      del_stmt node
  | ElifBranch :
      elif_branch node
  | ExecStmt :
      exec_stmt node
  | ForStmt :
      for_stmt node
  | GlobalStmt :
      global_stmt node
  | IfStmt :
      if_stmt node
  | ImportFrom :
      import_from node
  | ImportName :
      import_name node
  | PassStmt :
      pass_stmt node
  | PrintStmt :
      print_stmt node
  | RaiseStmt :
      raise_stmt node
  | ReturnStmt :
      return_stmt node
  | StreamPrintStmt :
      stream_print_stmt node
  | TryStmt :
      try_stmt node
  | WhileStmt :
      while_stmt node
  | WithStmt :
      with_stmt node
  | TurkixirNodeBaseList :
      turkixir_node_base_list node
  | ArgList :
      arg_list node
  | AsNameNodeList :
      as_name_node_list node
  | DecoratorList :
      decorator_list node
  | DictAssocList :
      dict_assoc_list node
  | DotList :
      dot_list node
  | ElifBranchList :
      elif_branch_list node
  | ExceptPartList :
      except_part_list node
  | ExprList :
      expr_list node
  | IdList :
      id_list node
  | NLList :
      nl_list node
  | SingleParamList :
      single_param_list node
  | StringLitList :
      string_lit_list node
  | TurkixirNodeList :
      turkixir_node_list node
  | VarArgsFlag :
      var_args_flag node
  | VarArgsFlagAbsent :
      var_args_flag_absent node
  | VarArgsFlagPresent :
      var_args_flag_present node

module VarArgsFlagPresent = struct
  type t =
    [
      | `VarArgsFlagPresent of
          var_args_flag_present_fields
    ]

  type fields = var_args_flag_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module VarArgsFlagAbsent = struct
  type t =
    [
      | `VarArgsFlagAbsent of
          var_args_flag_absent_fields
    ]

  type fields = var_args_flag_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module VarArgsFlag = struct
  type t =
    [
      | VarArgsFlagAbsent.t
      | VarArgsFlagPresent.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))

let p_as_bool
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.var_args_flag_p_as_bool
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr





end

module TurkixirNodeList = struct
  type t =
    [
      | `TurkixirNodeList of
          turkixir_node_list_fields
    ]

  type fields = turkixir_node_list_fields =
    
  {
    list : turkixir_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> turkixir_node_list) with
    | `TurkixirNodeList fields ->
        Lazy.force fields.list



end

module StringLitList = struct
  type t =
    [
      | `StringLitList of
          string_lit_list_fields
    ]

  type fields = string_lit_list_fields =
    
  {
    list : string_lit list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> string_lit_list) with
    | `StringLitList fields ->
        Lazy.force fields.list



end

module SingleParamList = struct
  type t =
    [
      | `SingleParamList of
          single_param_list_fields
    ]

  type fields = single_param_list_fields =
    
  {
    list : single_param list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> single_param_list) with
    | `SingleParamList fields ->
        Lazy.force fields.list



end

module NLList = struct
  type t =
    [
      | `NLList of
          nl_list_fields
    ]

  type fields = nl_list_fields =
    
  {
    list : nl list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> nl_list) with
    | `NLList fields ->
        Lazy.force fields.list



end

module IdList = struct
  type t =
    [
      | `IdList of
          id_list_fields
    ]

  type fields = id_list_fields =
    
  {
    list : id list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> id_list) with
    | `IdList fields ->
        Lazy.force fields.list



end

module ExprList = struct
  type t =
    [
      | `ExprList of
          expr_list_fields
    ]

  type fields = expr_list_fields =
    
  {
    list : expr list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> expr_list) with
    | `ExprList fields ->
        Lazy.force fields.list



end

module ExceptPartList = struct
  type t =
    [
      | `ExceptPartList of
          except_part_list_fields
    ]

  type fields = except_part_list_fields =
    
  {
    list : except_part list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> except_part_list) with
    | `ExceptPartList fields ->
        Lazy.force fields.list



end

module ElifBranchList = struct
  type t =
    [
      | `ElifBranchList of
          elif_branch_list_fields
    ]

  type fields = elif_branch_list_fields =
    
  {
    list : elif_branch list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> elif_branch_list) with
    | `ElifBranchList fields ->
        Lazy.force fields.list



end

module DotList = struct
  type t =
    [
      | `DotList of
          dot_list_fields
    ]

  type fields = dot_list_fields =
    
  {
    list : dot list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> dot_list) with
    | `DotList fields ->
        Lazy.force fields.list



end

module DictAssocList = struct
  type t =
    [
      | `DictAssocList of
          dict_assoc_list_fields
    ]

  type fields = dict_assoc_list_fields =
    
  {
    list : dict_assoc list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> dict_assoc_list) with
    | `DictAssocList fields ->
        Lazy.force fields.list



end

module DecoratorList = struct
  type t =
    [
      | `DecoratorList of
          decorator_list_fields
    ]

  type fields = decorator_list_fields =
    
  {
    list : decorator list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> decorator_list) with
    | `DecoratorList fields ->
        Lazy.force fields.list



end

module AsNameNodeList = struct
  type t =
    [
      | `AsNameNodeList of
          as_name_node_list_fields
    ]

  type fields = as_name_node_list_fields =
    
  {
    list : as_name_node list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> as_name_node_list) with
    | `AsNameNodeList fields ->
        Lazy.force fields.list



end

module ArgList = struct
  type t =
    [
      | `ArgList of
          arg_list_fields
    ]

  type fields = arg_list_fields =
    
  {
    list : arg list Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))



  let f_list node =
    match (node :> arg_list) with
    | `ArgList fields ->
        Lazy.force fields.list



end

module TurkixirNodeBaseList = struct
  type t =
    [
      | ArgList.t
      | AsNameNodeList.t
      | DecoratorList.t
      | DictAssocList.t
      | DotList.t
      | ElifBranchList.t
      | ExceptPartList.t
      | ExprList.t
      | IdList.t
      | NLList.t
      | SingleParamList.t
      | StringLitList.t
      | TurkixirNodeList.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module WithStmt = struct
  type t =
    [
      | `WithStmt of
          with_stmt_fields
    ]

  type fields = with_stmt_fields =
    
  {
         
    f_bindings: as_name_node_list
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_bindings node =
    match (node :> with_stmt) with
    | `WithStmt fields ->
        Lazy.force fields.f_bindings
  let f_statements node =
    match (node :> with_stmt) with
    | `WithStmt fields ->
        Lazy.force fields.f_statements



end

module WhileStmt = struct
  type t =
    [
      | `WhileStmt of
          while_stmt_fields
    ]

  type fields = while_stmt_fields =
    
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_cond_test node =
    match (node :> while_stmt) with
    | `WhileStmt fields ->
        Lazy.force fields.f_cond_test
  let f_statements node =
    match (node :> while_stmt) with
    | `WhileStmt fields ->
        Lazy.force fields.f_statements
  let f_else_part node =
    match (node :> while_stmt) with
    | `WhileStmt fields ->
        Lazy.force fields.f_else_part



end

module TryStmt = struct
  type t =
    [
      | `TryStmt of
          try_stmt_fields
    ]

  type fields = try_stmt_fields =
    
  {
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_except_parts: except_part_list
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
         
    f_finally_part: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_statements node =
    match (node :> try_stmt) with
    | `TryStmt fields ->
        Lazy.force fields.f_statements
  let f_except_parts node =
    match (node :> try_stmt) with
    | `TryStmt fields ->
        Lazy.force fields.f_except_parts
  let f_else_part node =
    match (node :> try_stmt) with
    | `TryStmt fields ->
        Lazy.force fields.f_else_part
  let f_finally_part node =
    match (node :> try_stmt) with
    | `TryStmt fields ->
        Lazy.force fields.f_finally_part



end

module StreamPrintStmt = struct
  type t =
    [
      | `StreamPrintStmt of
          stream_print_stmt_fields
    ]

  type fields = stream_print_stmt_fields =
    
  {
         
    f_stream_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_stream_expr node =
    match (node :> stream_print_stmt) with
    | `StreamPrintStmt fields ->
        Lazy.force fields.f_stream_expr
  let f_exprs node =
    match (node :> stream_print_stmt) with
    | `StreamPrintStmt fields ->
        Lazy.force fields.f_exprs



end

module ReturnStmt = struct
  type t =
    [
      | `ReturnStmt of
          return_stmt_fields
    ]

  type fields = return_stmt_fields =
    
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> return_stmt) with
    | `ReturnStmt fields ->
        Lazy.force fields.f_exprs



end

module RaiseStmt = struct
  type t =
    [
      | `RaiseStmt of
          raise_stmt_fields
    ]

  type fields = raise_stmt_fields =
    
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> raise_stmt) with
    | `RaiseStmt fields ->
        Lazy.force fields.f_exprs



end

module PrintStmt = struct
  type t =
    [
      | `PrintStmt of
          print_stmt_fields
    ]

  type fields = print_stmt_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> print_stmt) with
    | `PrintStmt fields ->
        Lazy.force fields.f_exprs



end

module PassStmt = struct
  type t =
    [
      | `PassStmt of
          pass_stmt_fields
    ]

  type fields = pass_stmt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module ImportName = struct
  type t =
    [
      | `ImportName of
          import_name_fields
    ]

  type fields = import_name_fields =
    
  {
         
    f_imported_names: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_imported_names node =
    match (node :> import_name) with
    | `ImportName fields ->
        Lazy.force fields.f_imported_names



end

module ImportFrom = struct
  type t =
    [
      | `ImportFrom of
          import_from_fields
    ]

  type fields = import_from_fields =
    
  {
         
    f_rel_name: [
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `RelName
          of rel_name_fields
    ]
    Lazy.t;
         
    f_imported: [
      | `ImportStar
          of import_star_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_rel_name node =
    match (node :> import_from) with
    | `ImportFrom fields ->
        Lazy.force fields.f_rel_name
  let f_imported node =
    match (node :> import_from) with
    | `ImportFrom fields ->
        Lazy.force fields.f_imported



end

module IfStmt = struct
  type t =
    [
      | `IfStmt of
          if_stmt_fields
    ]

  type fields = if_stmt_fields =
    
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_elif_branchs: elif_branch_list
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_cond_test node =
    match (node :> if_stmt) with
    | `IfStmt fields ->
        Lazy.force fields.f_cond_test
  let f_statements node =
    match (node :> if_stmt) with
    | `IfStmt fields ->
        Lazy.force fields.f_statements
  let f_elif_branchs node =
    match (node :> if_stmt) with
    | `IfStmt fields ->
        Lazy.force fields.f_elif_branchs
  let f_else_part node =
    match (node :> if_stmt) with
    | `IfStmt fields ->
        Lazy.force fields.f_else_part



end

module GlobalStmt = struct
  type t =
    [
      | `GlobalStmt of
          global_stmt_fields
    ]

  type fields = global_stmt_fields =
    
  {
         
    f_names: id_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_names node =
    match (node :> global_stmt) with
    | `GlobalStmt fields ->
        Lazy.force fields.f_names



end

module ForStmt = struct
  type t =
    [
      | `ForStmt of
          for_stmt_fields
    ]

  type fields = for_stmt_fields =
    
  {
         
    f_bindings: expr_list
    Lazy.t;
         
    f_expr: expr_list
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_part: else_part
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_bindings node =
    match (node :> for_stmt) with
    | `ForStmt fields ->
        Lazy.force fields.f_bindings
  let f_expr node =
    match (node :> for_stmt) with
    | `ForStmt fields ->
        Lazy.force fields.f_expr
  let f_statements node =
    match (node :> for_stmt) with
    | `ForStmt fields ->
        Lazy.force fields.f_statements
  let f_else_part node =
    match (node :> for_stmt) with
    | `ForStmt fields ->
        Lazy.force fields.f_else_part



end

module ExecStmt = struct
  type t =
    [
      | `ExecStmt of
          exec_stmt_fields
    ]

  type fields = exec_stmt_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_in_list: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> exec_stmt) with
    | `ExecStmt fields ->
        Lazy.force fields.f_expr
  let f_in_list node =
    match (node :> exec_stmt) with
    | `ExecStmt fields ->
        Lazy.force fields.f_in_list



end

module ElifBranch = struct
  type t =
    [
      | `ElifBranch of
          elif_branch_fields
    ]

  type fields = elif_branch_fields =
    
  {
         
    f_cond_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_cond_test node =
    match (node :> elif_branch) with
    | `ElifBranch fields ->
        Lazy.force fields.f_cond_test
  let f_statements node =
    match (node :> elif_branch) with
    | `ElifBranch fields ->
        Lazy.force fields.f_statements



end

module DelStmt = struct
  type t =
    [
      | `DelStmt of
          del_stmt_fields
    ]

  type fields = del_stmt_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> del_stmt) with
    | `DelStmt fields ->
        Lazy.force fields.f_exprs



end

module FuncDef = struct
  type t =
    [
      | `FuncDef of
          func_def_fields
    ]

  type fields = func_def_fields =
    
  {
         
    f_name: id
    Lazy.t;
         
    f_parameters: params
    option
    Lazy.t;
         
    f_body: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_name node =
    match (node :> func_def) with
    | `FuncDef fields ->
        Lazy.force fields.f_name
  let f_parameters node =
    match (node :> func_def) with
    | `FuncDef fields ->
        Lazy.force fields.f_parameters
  let f_body node =
    match (node :> func_def) with
    | `FuncDef fields ->
        Lazy.force fields.f_body



end

module ClassDef = struct
  type t =
    [
      | `ClassDef of
          class_def_fields
    ]

  type fields = class_def_fields =
    
  {
         
    f_name: id
    Lazy.t;
         
    f_bases: expr_list
    option
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_name node =
    match (node :> class_def) with
    | `ClassDef fields ->
        Lazy.force fields.f_name
  let f_bases node =
    match (node :> class_def) with
    | `ClassDef fields ->
        Lazy.force fields.f_bases
  let f_statements node =
    match (node :> class_def) with
    | `ClassDef fields ->
        Lazy.force fields.f_statements



end

module DefStmt = struct
  type t =
    [
      | ClassDef.t
      | FuncDef.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module Decorated = struct
  type t =
    [
      | `Decorated of
          decorated_fields
    ]

  type fields = decorated_fields =
    
  {
         
    f_decorators: decorator_list
    Lazy.t;
         
    f_defn: def_stmt
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_decorators node =
    match (node :> decorated) with
    | `Decorated fields ->
        Lazy.force fields.f_decorators
  let f_defn node =
    match (node :> decorated) with
    | `Decorated fields ->
        Lazy.force fields.f_defn



end

module ContinueStmt = struct
  type t =
    [
      | `ContinueStmt of
          continue_stmt_fields
    ]

  type fields = continue_stmt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module BreakStmt = struct
  type t =
    [
      | `BreakStmt of
          break_stmt_fields
    ]

  type fields = break_stmt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module AugAssignStmt = struct
  type t =
    [
      | `AugAssignStmt of
          aug_assign_stmt_fields
    ]

  type fields = aug_assign_stmt_fields =
    
  {
         
    f_l_value: expr_list
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_r_value: [
      | `ExprList
          of expr_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_l_value node =
    match (node :> aug_assign_stmt) with
    | `AugAssignStmt fields ->
        Lazy.force fields.f_l_value
  let f_op node =
    match (node :> aug_assign_stmt) with
    | `AugAssignStmt fields ->
        Lazy.force fields.f_op
  let f_r_value node =
    match (node :> aug_assign_stmt) with
    | `AugAssignStmt fields ->
        Lazy.force fields.f_r_value



end

module AssignStmt = struct
  type t =
    [
      | `AssignStmt of
          assign_stmt_fields
    ]

  type fields = assign_stmt_fields =
    
  {
         
    f_l_value: expr_list
    Lazy.t;
         
    f_r_values: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_l_value node =
    match (node :> assign_stmt) with
    | `AssignStmt fields ->
        Lazy.force fields.f_l_value
  let f_r_values node =
    match (node :> assign_stmt) with
    | `AssignStmt fields ->
        Lazy.force fields.f_r_values



end

module AssertStmt = struct
  type t =
    [
      | `AssertStmt of
          assert_stmt_fields
    ]

  type fields = assert_stmt_fields =
    
  {
         
    f_test_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_msg: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_test_expr node =
    match (node :> assert_stmt) with
    | `AssertStmt fields ->
        Lazy.force fields.f_test_expr
  let f_msg node =
    match (node :> assert_stmt) with
    | `AssertStmt fields ->
        Lazy.force fields.f_msg



end

module Stmt = struct
  type t =
    [
      | AssertStmt.t
      | AssignStmt.t
      | AugAssignStmt.t
      | BreakStmt.t
      | ContinueStmt.t
      | Decorated.t
      | DefStmt.t
      | DelStmt.t
      | ElifBranch.t
      | ExecStmt.t
      | ForStmt.t
      | GlobalStmt.t
      | IfStmt.t
      | ImportFrom.t
      | ImportName.t
      | PassStmt.t
      | PrintStmt.t
      | RaiseStmt.t
      | ReturnStmt.t
      | StreamPrintStmt.t
      | TryStmt.t
      | WhileStmt.t
      | WithStmt.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module SingleParam = struct
  type t =
    [
      | `SingleParam of
          single_param_fields
    ]

  type fields = single_param_fields =
    
  {
         
    f_is_varargs: var_args_flag
    Lazy.t;
         
    f_is_kwargs: kw_args_flag
    Lazy.t;
         
    f_name: [
      | `Id
          of id_fields
      | `IdList
          of id_list_fields
    ]
    Lazy.t;
         
    f_default_value: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_is_varargs node =
    match (node :> single_param) with
    | `SingleParam fields ->
        Lazy.force fields.f_is_varargs
  let f_is_kwargs node =
    match (node :> single_param) with
    | `SingleParam fields ->
        Lazy.force fields.f_is_kwargs
  let f_name node =
    match (node :> single_param) with
    | `SingleParam fields ->
        Lazy.force fields.f_name
  let f_default_value node =
    match (node :> single_param) with
    | `SingleParam fields ->
        Lazy.force fields.f_default_value



end

module RelName = struct
  type t =
    [
      | `RelName of
          rel_name_fields
    ]

  type fields = rel_name_fields =
    
  {
         
    f_dots: dot_list
    Lazy.t;
         
    f_name: name
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_dots node =
    match (node :> rel_name) with
    | `RelName fields ->
        Lazy.force fields.f_dots
  let f_name node =
    match (node :> rel_name) with
    | `RelName fields ->
        Lazy.force fields.f_name



end

module Params = struct
  type t =
    [
      | `Params of
          params_fields
    ]

  type fields = params_fields =
    
  {
         
    f_single_params: single_param_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_single_params node =
    match (node :> params) with
    | `Params fields ->
        Lazy.force fields.f_single_params



end

module Op = struct
  type t =
    [
      | `Op of
          op_fields
    ]

  type fields = op_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module NL = struct
  type t =
    [
      | `NL of
          nl_fields
    ]

  type fields = nl_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module KwArgsFlagPresent = struct
  type t =
    [
      | `KwArgsFlagPresent of
          kw_args_flag_present_fields
    ]

  type fields = kw_args_flag_present_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module KwArgsFlagAbsent = struct
  type t =
    [
      | `KwArgsFlagAbsent of
          kw_args_flag_absent_fields
    ]

  type fields = kw_args_flag_absent_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module KwArgsFlag = struct
  type t =
    [
      | KwArgsFlagAbsent.t
      | KwArgsFlagPresent.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))

let p_as_bool
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.kw_args_flag_p_as_bool
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr





end

module ImportStar = struct
  type t =
    [
      | `ImportStar of
          import_star_fields
    ]

  type fields = import_star_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module FileNode = struct
  type t =
    [
      | `FileNode of
          file_node_fields
    ]

  type fields = file_node_fields =
    
  {
         
    f_statements: turkixir_node_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_statements node =
    match (node :> file_node) with
    | `FileNode fields ->
        Lazy.force fields.f_statements



end

module YieldExpr = struct
  type t =
    [
      | `YieldExpr of
          yield_expr_fields
    ]

  type fields = yield_expr_fields =
    
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> yield_expr) with
    | `YieldExpr fields ->
        Lazy.force fields.f_exprs



end

module XorExpr = struct
  type t =
    [
      | `XorExpr of
          xor_expr_fields
    ]

  type fields = xor_expr_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> xor_expr) with
    | `XorExpr fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> xor_expr) with
    | `XorExpr fields ->
        Lazy.force fields.f_right



end

module TupleLit = struct
  type t =
    [
      | `TupleLit of
          tuple_lit_fields
    ]

  type fields = tuple_lit_fields =
    
  {
         
    f_exprs: expr_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> tuple_lit) with
    | `TupleLit fields ->
        Lazy.force fields.f_exprs



end

module SubscriptExpr = struct
  type t =
    [
      | `SubscriptExpr of
          subscript_expr_fields
    ]

  type fields = subscript_expr_fields =
    
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_prefix node =
    match (node :> subscript_expr) with
    | `SubscriptExpr fields ->
        Lazy.force fields.f_prefix
  let f_suffix node =
    match (node :> subscript_expr) with
    | `SubscriptExpr fields ->
        Lazy.force fields.f_suffix



end

module StringLit = struct
  type t =
    [
      | `StringLit of
          string_lit_fields
    ]

  type fields = string_lit_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module ExtSliceExpr = struct
  type t =
    [
      | `ExtSliceExpr of
          ext_slice_expr_fields
    ]

  type fields = ext_slice_expr_fields =
    
  {
         
    f_first: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_last: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_stride: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_first node =
    match (node :> ext_slice_expr) with
    | `ExtSliceExpr fields ->
        Lazy.force fields.f_first
  let f_last node =
    match (node :> ext_slice_expr) with
    | `ExtSliceExpr fields ->
        Lazy.force fields.f_last
  let f_stride node =
    match (node :> ext_slice_expr) with
    | `ExtSliceExpr fields ->
        Lazy.force fields.f_stride



end

module SliceExpr = struct
  type t =
    [
      | `SliceExpr of
          slice_expr_fields
      | `ExtSliceExpr of
          ext_slice_expr_fields
    ]

  type fields = slice_expr_fields =
    
  {
         
    f_first: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_last: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_first node =
    match (node :> slice_expr) with
    | `SliceExpr fields ->
        Lazy.force fields.f_first
    | `ExtSliceExpr fields ->
        Lazy.force fields.f_first
  let f_last node =
    match (node :> slice_expr) with
    | `SliceExpr fields ->
        Lazy.force fields.f_last
    | `ExtSliceExpr fields ->
        Lazy.force fields.f_last



end

module SetLit = struct
  type t =
    [
      | `SetLit of
          set_lit_fields
    ]

  type fields = set_lit_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> set_lit) with
    | `SetLit fields ->
        Lazy.force fields.f_exprs



end

module SetComp = struct
  type t =
    [
      | `SetComp of
          set_comp_fields
    ]

  type fields = set_comp_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> set_comp) with
    | `SetComp fields ->
        Lazy.force fields.f_expr
  let f_comprehension node =
    match (node :> set_comp) with
    | `SetComp fields ->
        Lazy.force fields.f_comprehension



end

module Power = struct
  type t =
    [
      | `Power of
          power_fields
    ]

  type fields = power_fields =
    
  {
         
    f_left: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> power) with
    | `Power fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> power) with
    | `Power fields ->
        Lazy.force fields.f_right



end

module OrOp = struct
  type t =
    [
      | `OrOp of
          or_op_fields
    ]

  type fields = or_op_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> or_op) with
    | `OrOp fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> or_op) with
    | `OrOp fields ->
        Lazy.force fields.f_right



end

module OrExpr = struct
  type t =
    [
      | `OrExpr of
          or_expr_fields
    ]

  type fields = or_expr_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> or_expr) with
    | `OrExpr fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> or_expr) with
    | `OrExpr fields ->
        Lazy.force fields.f_right



end

module NumberLit = struct
  type t =
    [
      | `NumberLit of
          number_lit_fields
    ]

  type fields = number_lit_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module NotOp = struct
  type t =
    [
      | `NotOp of
          not_op_fields
    ]

  type fields = not_op_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> not_op) with
    | `NotOp fields ->
        Lazy.force fields.f_expr



end

module Id = struct
  type t =
    [
      | `Id of
          id_fields
    ]

  type fields = id_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module DottedName = struct
  type t =
    [
      | `DottedName of
          dotted_name_fields
    ]

  type fields = dotted_name_fields =
    
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: id
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_prefix node =
    match (node :> dotted_name) with
    | `DottedName fields ->
        Lazy.force fields.f_prefix
  let f_suffix node =
    match (node :> dotted_name) with
    | `DottedName fields ->
        Lazy.force fields.f_suffix



end

module Name = struct
  type t =
    [
      | DottedName.t
      | Id.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module ListLit = struct
  type t =
    [
      | `ListLit of
          list_lit_fields
    ]

  type fields = list_lit_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> list_lit) with
    | `ListLit fields ->
        Lazy.force fields.f_exprs



end

module ListGen = struct
  type t =
    [
      | `ListGen of
          list_gen_fields
    ]

  type fields = list_gen_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_forl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> list_gen) with
    | `ListGen fields ->
        Lazy.force fields.f_expr
  let f_comprehension node =
    match (node :> list_gen) with
    | `ListGen fields ->
        Lazy.force fields.f_comprehension



end

module ListComp = struct
  type t =
    [
      | `ListComp of
          list_comp_fields
    ]

  type fields = list_comp_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_forl
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> list_comp) with
    | `ListComp fields ->
        Lazy.force fields.f_expr
  let f_comprehension node =
    match (node :> list_comp) with
    | `ListComp fields ->
        Lazy.force fields.f_comprehension



end

module LambdaDef = struct
  type t =
    [
      | `LambdaDef of
          lambda_def_fields
    ]

  type fields = lambda_def_fields =
    
  {
         
    f_args: params
    Lazy.t;
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_args node =
    match (node :> lambda_def) with
    | `LambdaDef fields ->
        Lazy.force fields.f_args
  let f_expr node =
    match (node :> lambda_def) with
    | `LambdaDef fields ->
        Lazy.force fields.f_expr



end

module InlineEval = struct
  type t =
    [
      | `InlineEval of
          inline_eval_fields
    ]

  type fields = inline_eval_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> inline_eval) with
    | `InlineEval fields ->
        Lazy.force fields.f_exprs



end

module IfExpr = struct
  type t =
    [
      | `IfExpr of
          if_expr_fields
    ]

  type fields = if_expr_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_cond: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_else_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> if_expr) with
    | `IfExpr fields ->
        Lazy.force fields.f_expr
  let f_cond node =
    match (node :> if_expr) with
    | `IfExpr fields ->
        Lazy.force fields.f_cond
  let f_else_expr node =
    match (node :> if_expr) with
    | `IfExpr fields ->
        Lazy.force fields.f_else_expr



end

module Factor = struct
  type t =
    [
      | `Factor of
          factor_fields
    ]

  type fields = factor_fields =
    
  {
         
    f_op: op
    Lazy.t;
         
    f_expr: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_op node =
    match (node :> factor) with
    | `Factor fields ->
        Lazy.force fields.f_op
  let f_expr node =
    match (node :> factor) with
    | `Factor fields ->
        Lazy.force fields.f_expr



end

module EllipsisExpr = struct
  type t =
    [
      | `EllipsisExpr of
          ellipsis_expr_fields
    ]

  type fields = ellipsis_expr_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module Dot = struct
  type t =
    [
      | `Dot of
          dot_fields
    ]

  type fields = dot_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module DictLit = struct
  type t =
    [
      | `DictLit of
          dict_lit_fields
    ]

  type fields = dict_lit_fields =
    
  {
         
    f_assocs: dict_assoc_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_assocs node =
    match (node :> dict_lit) with
    | `DictLit fields ->
        Lazy.force fields.f_assocs



end

module DictComp = struct
  type t =
    [
      | `DictComp of
          dict_comp_fields
    ]

  type fields = dict_comp_fields =
    
  {
         
    f_assoc: dict_assoc
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_assoc node =
    match (node :> dict_comp) with
    | `DictComp fields ->
        Lazy.force fields.f_assoc
  let f_comprehension node =
    match (node :> dict_comp) with
    | `DictComp fields ->
        Lazy.force fields.f_comprehension



end

module ConcatStringLit = struct
  type t =
    [
      | `ConcatStringLit of
          concat_string_lit_fields
    ]

  type fields = concat_string_lit_fields =
    
  {
         
    f_first_str: string_lit
    Lazy.t;
         
    f_subsequent_str: string_lit_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_first_str node =
    match (node :> concat_string_lit) with
    | `ConcatStringLit fields ->
        Lazy.force fields.f_first_str
  let f_subsequent_str node =
    match (node :> concat_string_lit) with
    | `ConcatStringLit fields ->
        Lazy.force fields.f_subsequent_str



end

module CompOp = struct
  type t =
    [
      | `CompOp of
          comp_op_fields
    ]

  type fields = comp_op_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: comp_op_kind
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> comp_op) with
    | `CompOp fields ->
        Lazy.force fields.f_left
  let f_op node =
    match (node :> comp_op) with
    | `CompOp fields ->
        Lazy.force fields.f_op
  let f_right node =
    match (node :> comp_op) with
    | `CompOp fields ->
        Lazy.force fields.f_right



end

module CallExpr = struct
  type t =
    [
      | `CallExpr of
          call_expr_fields
    ]

  type fields = call_expr_fields =
    
  {
         
    f_prefix: [
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_suffix: arg_list
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_prefix node =
    match (node :> call_expr) with
    | `CallExpr fields ->
        Lazy.force fields.f_prefix
  let f_suffix node =
    match (node :> call_expr) with
    | `CallExpr fields ->
        Lazy.force fields.f_suffix



end

module Term = struct
  type t =
    [
      | `Term of
          term_fields
    ]

  type fields = term_fields =
    
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> term) with
    | `Term fields ->
        Lazy.force fields.f_left
  let f_op node =
    match (node :> term) with
    | `Term fields ->
        Lazy.force fields.f_op
  let f_right node =
    match (node :> term) with
    | `Term fields ->
        Lazy.force fields.f_right



end

module ShiftExpr = struct
  type t =
    [
      | `ShiftExpr of
          shift_expr_fields
    ]

  type fields = shift_expr_fields =
    
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> shift_expr) with
    | `ShiftExpr fields ->
        Lazy.force fields.f_left
  let f_op node =
    match (node :> shift_expr) with
    | `ShiftExpr fields ->
        Lazy.force fields.f_op
  let f_right node =
    match (node :> shift_expr) with
    | `ShiftExpr fields ->
        Lazy.force fields.f_right



end

module ArithExpr = struct
  type t =
    [
      | `ArithExpr of
          arith_expr_fields
    ]

  type fields = arith_expr_fields =
    
  {
         
    f_left: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_op: op
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> arith_expr) with
    | `ArithExpr fields ->
        Lazy.force fields.f_left
  let f_op node =
    match (node :> arith_expr) with
    | `ArithExpr fields ->
        Lazy.force fields.f_op
  let f_right node =
    match (node :> arith_expr) with
    | `ArithExpr fields ->
        Lazy.force fields.f_right



end

module BinOp = struct
  type t =
    [
      | ArithExpr.t
      | ShiftExpr.t
      | Term.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> bin_op) with
    | `ArithExpr fields ->
        Lazy.force fields.f_left
    | `ShiftExpr fields ->
        Lazy.force fields.f_left
    | `Term fields ->
        Lazy.force fields.f_left
  let f_op node =
    match (node :> bin_op) with
    | `ArithExpr fields ->
        Lazy.force fields.f_op
    | `ShiftExpr fields ->
        Lazy.force fields.f_op
    | `Term fields ->
        Lazy.force fields.f_op
  let f_right node =
    match (node :> bin_op) with
    | `ArithExpr fields ->
        Lazy.force fields.f_right
    | `ShiftExpr fields ->
        Lazy.force fields.f_right
    | `Term fields ->
        Lazy.force fields.f_right



end

module AndOp = struct
  type t =
    [
      | `AndOp of
          and_op_fields
    ]

  type fields = and_op_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> and_op) with
    | `AndOp fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> and_op) with
    | `AndOp fields ->
        Lazy.force fields.f_right



end

module AndExpr = struct
  type t =
    [
      | `AndExpr of
          and_expr_fields
    ]

  type fields = and_expr_fields =
    
  {
         
    f_left: [
      | `AndExpr
          of and_expr_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_right: [
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NumberLit
          of number_lit_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_left node =
    match (node :> and_expr) with
    | `AndExpr fields ->
        Lazy.force fields.f_left
  let f_right node =
    match (node :> and_expr) with
    | `AndExpr fields ->
        Lazy.force fields.f_right



end

module Expr = struct
  type t =
    [
      | AndExpr.t
      | AndOp.t
      | BinOp.t
      | CallExpr.t
      | CompOp.t
      | ConcatStringLit.t
      | DictComp.t
      | DictLit.t
      | Dot.t
      | EllipsisExpr.t
      | Factor.t
      | IfExpr.t
      | InlineEval.t
      | LambdaDef.t
      | ListComp.t
      | ListGen.t
      | ListLit.t
      | Name.t
      | NotOp.t
      | NumberLit.t
      | OrExpr.t
      | OrOp.t
      | Power.t
      | SetComp.t
      | SetLit.t
      | SliceExpr.t
      | StringLit.t
      | SubscriptExpr.t
      | TupleLit.t
      | XorExpr.t
      | YieldExpr.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module ExceptPart = struct
  type t =
    [
      | `ExceptPart of
          except_part_fields
    ]

  type fields = except_part_fields =
    
  {
         
    f_as_name: as_name_node
    option
    Lazy.t;
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_as_name node =
    match (node :> except_part) with
    | `ExceptPart fields ->
        Lazy.force fields.f_as_name
  let f_statements node =
    match (node :> except_part) with
    | `ExceptPart fields ->
        Lazy.force fields.f_statements



end

module ElsePart = struct
  type t =
    [
      | `ElsePart of
          else_part_fields
    ]

  type fields = else_part_fields =
    
  {
         
    f_statements: [
      | `AssertStmt
          of assert_stmt_fields
      | `AssignStmt
          of assign_stmt_fields
      | `AugAssignStmt
          of aug_assign_stmt_fields
      | `BreakStmt
          of break_stmt_fields
      | `ContinueStmt
          of continue_stmt_fields
      | `DelStmt
          of del_stmt_fields
      | `ExecStmt
          of exec_stmt_fields
      | `ExprList
          of expr_list_fields
      | `GlobalStmt
          of global_stmt_fields
      | `ImportFrom
          of import_from_fields
      | `ImportName
          of import_name_fields
      | `PassStmt
          of pass_stmt_fields
      | `PrintStmt
          of print_stmt_fields
      | `RaiseStmt
          of raise_stmt_fields
      | `ReturnStmt
          of return_stmt_fields
      | `StreamPrintStmt
          of stream_print_stmt_fields
      | `TurkixirNodeList
          of turkixir_node_list_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_statements node =
    match (node :> else_part) with
    | `ElsePart fields ->
        Lazy.force fields.f_statements



end

module DictAssoc = struct
  type t =
    [
      | `DictAssoc of
          dict_assoc_fields
    ]

  type fields = dict_assoc_fields =
    
  {
         
    f_key: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_value: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_key node =
    match (node :> dict_assoc) with
    | `DictAssoc fields ->
        Lazy.force fields.f_key
  let f_value node =
    match (node :> dict_assoc) with
    | `DictAssoc fields ->
        Lazy.force fields.f_value



end

module Decorator = struct
  type t =
    [
      | `Decorator of
          decorator_fields
    ]

  type fields = decorator_fields =
    
  {
         
    f_dec_name: name
    Lazy.t;
         
    f_arg_list: arg_list
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_dec_name node =
    match (node :> decorator) with
    | `Decorator fields ->
        Lazy.force fields.f_dec_name
  let f_arg_list node =
    match (node :> decorator) with
    | `Decorator fields ->
        Lazy.force fields.f_arg_list



end

module CompForL = struct
  type t =
    [
      | `CompForL of
          comp_forl_fields
    ]

  type fields = comp_forl_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
         
    f_target: expr_list
    Lazy.t;
         
    f_comp: [
      | `CompForL
          of comp_forl_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> comp_forl) with
    | `CompForL fields ->
        Lazy.force fields.f_exprs
  let f_target node =
    match (node :> comp_forl) with
    | `CompForL fields ->
        Lazy.force fields.f_target
  let f_comp node =
    match (node :> comp_forl) with
    | `CompForL fields ->
        Lazy.force fields.f_comp



end

module CompFor = struct
  type t =
    [
      | `CompFor of
          comp_for_fields
    ]

  type fields = comp_for_fields =
    
  {
         
    f_exprs: expr_list
    Lazy.t;
         
    f_target: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `InlineEval
          of inline_eval_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comp: [
      | `CompFor
          of comp_for_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_exprs node =
    match (node :> comp_for) with
    | `CompFor fields ->
        Lazy.force fields.f_exprs
  let f_target node =
    match (node :> comp_for) with
    | `CompFor fields ->
        Lazy.force fields.f_target
  let f_comp node =
    match (node :> comp_for) with
    | `CompFor fields ->
        Lazy.force fields.f_comp



end

module Comprehension = struct
  type t =
    [
      | CompFor.t
      | CompForL.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindNotin = struct
  type t =
    [
      | `CompOpKindNotin of
          comp_op_kind_notin_fields
    ]

  type fields = comp_op_kind_notin_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindNoteq = struct
  type t =
    [
      | `CompOpKindNoteq of
          comp_op_kind_noteq_fields
    ]

  type fields = comp_op_kind_noteq_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindLte = struct
  type t =
    [
      | `CompOpKindLte of
          comp_op_kind_lte_fields
    ]

  type fields = comp_op_kind_lte_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindLt = struct
  type t =
    [
      | `CompOpKindLt of
          comp_op_kind_lt_fields
    ]

  type fields = comp_op_kind_lt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindIsnot = struct
  type t =
    [
      | `CompOpKindIsnot of
          comp_op_kind_isnot_fields
    ]

  type fields = comp_op_kind_isnot_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindIs = struct
  type t =
    [
      | `CompOpKindIs of
          comp_op_kind_is_fields
    ]

  type fields = comp_op_kind_is_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindIn = struct
  type t =
    [
      | `CompOpKindIn of
          comp_op_kind_in_fields
    ]

  type fields = comp_op_kind_in_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindGte = struct
  type t =
    [
      | `CompOpKindGte of
          comp_op_kind_gte_fields
    ]

  type fields = comp_op_kind_gte_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindGt = struct
  type t =
    [
      | `CompOpKindGt of
          comp_op_kind_gt_fields
    ]

  type fields = comp_op_kind_gt_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindEq = struct
  type t =
    [
      | `CompOpKindEq of
          comp_op_kind_eq_fields
    ]

  type fields = comp_op_kind_eq_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKindDiamond = struct
  type t =
    [
      | `CompOpKindDiamond of
          comp_op_kind_diamond_fields
    ]

  type fields = comp_op_kind_diamond_fields =
    
  {
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompOpKind = struct
  type t =
    [
      | CompOpKindDiamond.t
      | CompOpKindEq.t
      | CompOpKindGt.t
      | CompOpKindGte.t
      | CompOpKindIn.t
      | CompOpKindIs.t
      | CompOpKindIsnot.t
      | CompOpKindLt.t
      | CompOpKindLte.t
      | CompOpKindNoteq.t
      | CompOpKindNotin.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module CompIf = struct
  type t =
    [
      | `CompIf of
          comp_if_fields
    ]

  type fields = comp_if_fields =
    
  {
         
    f_test: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comp: [
      | `CompFor
          of comp_for_fields
      | `CompForL
          of comp_forl_fields
      | `CompIf
          of comp_if_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_test node =
    match (node :> comp_if) with
    | `CompIf fields ->
        Lazy.force fields.f_test
  let f_comp node =
    match (node :> comp_if) with
    | `CompIf fields ->
        Lazy.force fields.f_comp



end

module AsNameNode = struct
  type t =
    [
      | `AsNameNode of
          as_name_node_fields
    ]

  type fields = as_name_node_fields =
    
  {
         
    f_imported: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_as_name: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_imported node =
    match (node :> as_name_node) with
    | `AsNameNode fields ->
        Lazy.force fields.f_imported
  let f_as_name node =
    match (node :> as_name_node) with
    | `AsNameNode fields ->
        Lazy.force fields.f_as_name



end

module VarArgs = struct
  type t =
    [
      | `VarArgs of
          var_args_fields
    ]

  type fields = var_args_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> var_args) with
    | `VarArgs fields ->
        Lazy.force fields.f_expr



end

module KwArgs = struct
  type t =
    [
      | `KwArgs of
          kw_args_fields
    ]

  type fields = kw_args_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> kw_args) with
    | `KwArgs fields ->
        Lazy.force fields.f_expr



end

module ArgGen = struct
  type t =
    [
      | `ArgGen of
          arg_gen_fields
    ]

  type fields = arg_gen_fields =
    
  {
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
         
    f_comprehension: comp_for
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_expr node =
    match (node :> arg_gen) with
    | `ArgGen fields ->
        Lazy.force fields.f_expr
  let f_comprehension node =
    match (node :> arg_gen) with
    | `ArgGen fields ->
        Lazy.force fields.f_comprehension



end

module ArgAssoc = struct
  type t =
    [
      | `ArgAssoc of
          arg_assoc_fields
    ]

  type fields = arg_assoc_fields =
    
  {
         
    f_name: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    option
    Lazy.t;
         
    f_expr: [
      | `AndExpr
          of and_expr_fields
      | `AndOp
          of and_op_fields
      | `ArithExpr
          of arith_expr_fields
      | `CallExpr
          of call_expr_fields
      | `CompOp
          of comp_op_fields
      | `ConcatStringLit
          of concat_string_lit_fields
      | `DictComp
          of dict_comp_fields
      | `DictLit
          of dict_lit_fields
      | `DottedName
          of dotted_name_fields
      | `Factor
          of factor_fields
      | `Id
          of id_fields
      | `IfExpr
          of if_expr_fields
      | `InlineEval
          of inline_eval_fields
      | `LambdaDef
          of lambda_def_fields
      | `ListComp
          of list_comp_fields
      | `ListGen
          of list_gen_fields
      | `ListLit
          of list_lit_fields
      | `NotOp
          of not_op_fields
      | `NumberLit
          of number_lit_fields
      | `OrExpr
          of or_expr_fields
      | `OrOp
          of or_op_fields
      | `Power
          of power_fields
      | `SetComp
          of set_comp_fields
      | `SetLit
          of set_lit_fields
      | `ShiftExpr
          of shift_expr_fields
      | `StringLit
          of string_lit_fields
      | `SubscriptExpr
          of subscript_expr_fields
      | `Term
          of term_fields
      | `TupleLit
          of tuple_lit_fields
      | `XorExpr
          of xor_expr_fields
      | `YieldExpr
          of yield_expr_fields
    ]
    Lazy.t;
    c_value : entity;
    context : analysis_context
  }


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))


  let f_name node =
    match (node :> arg_assoc) with
    | `ArgAssoc fields ->
        Lazy.force fields.f_name
  let f_expr node =
    match (node :> arg_assoc) with
    | `ArgAssoc fields ->
        Lazy.force fields.f_expr



end

module Arg = struct
  type t =
    [
      | ArgAssoc.t
      | ArgGen.t
      | KwArgs.t
      | VarArgs.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))





end

module TurkixirNode = struct
  type t =
    [
      | Arg.t
      | AsNameNode.t
      | CompIf.t
      | CompOpKind.t
      | Comprehension.t
      | Decorator.t
      | DictAssoc.t
      | ElsePart.t
      | ExceptPart.t
      | Expr.t
      | FileNode.t
      | ImportStar.t
      | KwArgsFlag.t
      | NL.t
      | Op.t
      | Params.t
      | RelName.t
      | SingleParam.t
      | Stmt.t
      | TurkixirNodeBaseList.t
      | VarArgsFlag.t
    ]


  let equal node1 node2 =
    Entity.equal
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let compare node1 node2 =
    Entity.compare
      (unwrap_turkixir_node ((node1 :> turkixir_node)))
      (unwrap_turkixir_node ((node2 :> turkixir_node)))

  let hash node =
    Entity.hash
      (unwrap_turkixir_node ((node :> turkixir_node)))

let parent
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_parent
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_turkixir_node (context node) (!@ result_ptr))

let parents
    ?(with_self=true)
    (node)
    =
      let result_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let c_with_self =
            
        with_self
      in
      let _ : int =
        CFunctions.turkixir_node_parents
          (addr (unwrap_turkixir_node (node)))
          c_with_self
          (result_ptr)
      in
         
      TurkixirNodeArray.wrap (context node) (!@ result_ptr)

let children
    (node)
    =
      let result_ptr =
        allocate_n TurkixirNodeArrayStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_children
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      TurkixirNodeArray.wrap (context node) (!@ result_ptr)

let token_start
    (node)
    =
      let result_ptr =
        allocate_n Token.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_token_start
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let token_end
    (node)
    =
      let result_ptr =
        allocate_n Token.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_token_end
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let child_index
    (node)
    =
      let result_ptr =
        allocate_n int ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_child_index
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let previous_sibling
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_previous_sibling
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_turkixir_node (context node) (!@ result_ptr))

let next_sibling
    (node)
    =
      let result_ptr =
        allocate_n EntityStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_next_sibling
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_turkixir_node (context node) (!@ result_ptr))

let unit
    (node)
    =
      let result_ptr =
        allocate_n AnalysisUnitStruct.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_unit
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      wrap_analysis_unit (context node) (!@ result_ptr)

let is_ghost
    (node)
    =
      let result_ptr =
        allocate_n bool ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_is_ghost
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      !@ result_ptr

let full_sloc_image
    (node)
    =
      let result_ptr =
        allocate_n StringType.c_type ~count:1
      in
      (* The result of this call should already be checked by the raisable
         mechanism *)
      let _ : int =
        CFunctions.turkixir_node_full_sloc_image
          (addr (unwrap_turkixir_node (node)))
          (result_ptr)
      in
      StringType.wrap (!@ result_ptr)





  let kind_name = function
    | #var_args_flag_present ->
        "VarArgsFlagPresent"
    | #var_args_flag_absent ->
        "VarArgsFlagAbsent"
    | #turkixir_node_list ->
        "TurkixirNodeList"
    | #string_lit_list ->
        "StringLitList"
    | #single_param_list ->
        "SingleParamList"
    | #nl_list ->
        "NLList"
    | #id_list ->
        "IdList"
    | #expr_list ->
        "ExprList"
    | #except_part_list ->
        "ExceptPartList"
    | #elif_branch_list ->
        "ElifBranchList"
    | #dot_list ->
        "DotList"
    | #dict_assoc_list ->
        "DictAssocList"
    | #decorator_list ->
        "DecoratorList"
    | #as_name_node_list ->
        "AsNameNodeList"
    | #arg_list ->
        "ArgList"
    | #with_stmt ->
        "WithStmt"
    | #while_stmt ->
        "WhileStmt"
    | #try_stmt ->
        "TryStmt"
    | #stream_print_stmt ->
        "StreamPrintStmt"
    | #return_stmt ->
        "ReturnStmt"
    | #raise_stmt ->
        "RaiseStmt"
    | #print_stmt ->
        "PrintStmt"
    | #pass_stmt ->
        "PassStmt"
    | #import_name ->
        "ImportName"
    | #import_from ->
        "ImportFrom"
    | #if_stmt ->
        "IfStmt"
    | #global_stmt ->
        "GlobalStmt"
    | #for_stmt ->
        "ForStmt"
    | #exec_stmt ->
        "ExecStmt"
    | #elif_branch ->
        "ElifBranch"
    | #del_stmt ->
        "DelStmt"
    | #func_def ->
        "FuncDef"
    | #class_def ->
        "ClassDef"
    | #decorated ->
        "Decorated"
    | #continue_stmt ->
        "ContinueStmt"
    | #break_stmt ->
        "BreakStmt"
    | #aug_assign_stmt ->
        "AugAssignStmt"
    | #assign_stmt ->
        "AssignStmt"
    | #assert_stmt ->
        "AssertStmt"
    | #single_param ->
        "SingleParam"
    | #rel_name ->
        "RelName"
    | #params ->
        "Params"
    | #op ->
        "Op"
    | #nl ->
        "NL"
    | #kw_args_flag_present ->
        "KwArgsFlagPresent"
    | #kw_args_flag_absent ->
        "KwArgsFlagAbsent"
    | #import_star ->
        "ImportStar"
    | #file_node ->
        "FileNode"
    | #yield_expr ->
        "YieldExpr"
    | #xor_expr ->
        "XorExpr"
    | #tuple_lit ->
        "TupleLit"
    | #subscript_expr ->
        "SubscriptExpr"
    | #string_lit ->
        "StringLit"
    | #ext_slice_expr ->
        "ExtSliceExpr"
    | #slice_expr ->
        "SliceExpr"
    | #set_lit ->
        "SetLit"
    | #set_comp ->
        "SetComp"
    | #power ->
        "Power"
    | #or_op ->
        "OrOp"
    | #or_expr ->
        "OrExpr"
    | #number_lit ->
        "NumberLit"
    | #not_op ->
        "NotOp"
    | #id ->
        "Id"
    | #dotted_name ->
        "DottedName"
    | #list_lit ->
        "ListLit"
    | #list_gen ->
        "ListGen"
    | #list_comp ->
        "ListComp"
    | #lambda_def ->
        "LambdaDef"
    | #inline_eval ->
        "InlineEval"
    | #if_expr ->
        "IfExpr"
    | #factor ->
        "Factor"
    | #ellipsis_expr ->
        "EllipsisExpr"
    | #dot ->
        "Dot"
    | #dict_lit ->
        "DictLit"
    | #dict_comp ->
        "DictComp"
    | #concat_string_lit ->
        "ConcatStringLit"
    | #comp_op ->
        "CompOp"
    | #call_expr ->
        "CallExpr"
    | #term ->
        "Term"
    | #shift_expr ->
        "ShiftExpr"
    | #arith_expr ->
        "ArithExpr"
    | #and_op ->
        "AndOp"
    | #and_expr ->
        "AndExpr"
    | #except_part ->
        "ExceptPart"
    | #else_part ->
        "ElsePart"
    | #dict_assoc ->
        "DictAssoc"
    | #decorator ->
        "Decorator"
    | #comp_forl ->
        "CompForL"
    | #comp_for ->
        "CompFor"
    | #comp_op_kind_notin ->
        "CompOpKindNotin"
    | #comp_op_kind_noteq ->
        "CompOpKindNoteq"
    | #comp_op_kind_lte ->
        "CompOpKindLte"
    | #comp_op_kind_lt ->
        "CompOpKindLt"
    | #comp_op_kind_isnot ->
        "CompOpKindIsnot"
    | #comp_op_kind_is ->
        "CompOpKindIs"
    | #comp_op_kind_in ->
        "CompOpKindIn"
    | #comp_op_kind_gte ->
        "CompOpKindGte"
    | #comp_op_kind_gt ->
        "CompOpKindGt"
    | #comp_op_kind_eq ->
        "CompOpKindEq"
    | #comp_op_kind_diamond ->
        "CompOpKindDiamond"
    | #comp_if ->
        "CompIf"
    | #as_name_node ->
        "AsNameNode"
    | #var_args ->
        "VarArgs"
    | #kw_args ->
        "KwArgs"
    | #arg_gen ->
        "ArgGen"
    | #arg_assoc ->
        "ArgAssoc"

  let text node =
    Token.text_range (token_start node) (token_end node)

  let image node =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    CFunctions.image
      (addr (unwrap_turkixir_node (node)))
      c_result_ptr;
    !@ c_result_ptr

  let entity_image node =
    let c_result_ptr = allocate_n Text.c_type ~count:1 in
    let node_c_value = unwrap_turkixir_node (node) in
    CFunctions.entity_image (addr node_c_value) c_result_ptr;
    !@ c_result_ptr

  let is_token_node node =
    let node_c_value = unwrap_turkixir_node (node) in
    CFunctions.node_is_token_node (addr node_c_value)

  let sloc_range node =
    let c_result_ptr = allocate_n SlocRange.c_type ~count:1 in
    CFunctions.node_sloc_range
      (addr (unwrap_turkixir_node (node)))
      c_result_ptr;
    !@ c_result_ptr

  
  let fold_tokens f init node =
    let tok_start = token_start node in
    let tok_end = token_end node in
    let rec aux acc tok_curr =
      let new_acc = f acc tok_curr in
      if Token.equal tok_curr tok_end then
        new_acc
      else
        aux new_acc (Token.next tok_curr)
    in
    aux init tok_start

  let iter_tokens f node =
    let tok_start = token_start node in
    let tok_end = token_end node in
    let rec aux tok_curr =
      f tok_curr;
      if not (Token.equal tok_curr tok_end) then
        aux (Token.next tok_curr)
    in
    aux tok_start

  let map_tokens f node =
    let tok_start = token_start node in
    let tok_end = token_end node in
    let rec aux tok_curr =
      let value = f tok_curr in
      if Token.equal tok_curr tok_end then
        [value]
      else
        value :: aux (Token.next tok_curr)
    in
    aux tok_start

  let tokens node =
    map_tokens (fun x -> x) node


  let lookup node sloc =
    let node_c_value = unwrap_turkixir_node (node) in
    let sloc_ptr = allocate Sloc.c_type sloc in
    let result_ptr = allocate_n EntityStruct.c_type ~count:1 in
    CFunctions.lookup_in_node
      (addr node_c_value) sloc_ptr result_ptr;
    if is_null (getf !@ result_ptr EntityStruct.node) then None else Some (wrap_turkixir_node (context node) (!@ result_ptr))

  let children_opt node =
    let node_c_value = unwrap_turkixir_node (node) in
    let context = context node in
    let c_value_ptr = allocate_n TurkixirNodeArrayStruct.c_type ~count:1 in
    let _ : int =
      CFunctions.turkixir_node_children
        (addr node_c_value)
        (c_value_ptr)
    in
    let c_value = !@(!@(c_value_ptr)) in
    let length = getf c_value TurkixirNodeArrayStruct.n in
    let items = c_value @. TurkixirNodeArrayStruct.items in
    let f i =
      let fresh = allocate EntityStruct.c_type !@(items +@ i) in
      if is_null (getf !@ fresh EntityStruct.node) then None else Some (wrap_turkixir_node context (!@ fresh))
    in
    let result = List.init length f in
    TurkixirNodeArrayStruct.dec_ref (!@ c_value_ptr);
    result

  let iter_fields f node =
    children_opt (node :> turkixir_node)
    |> List.iter (function None -> () | Some node -> f node)

  let fold_fields f acc node =
    children_opt (node :> turkixir_node)
    |> List.fold_left (fun x -> function None -> x | Some node -> f x node) acc

  let exists_fields p node =
    children_opt (node :> turkixir_node)
    |> List.exists (function | None -> false | Some node -> p node)

  let for_all_fields p node =
    children_opt (node :> turkixir_node)
    |> List.for_all (function | None -> true | Some node -> p node)

  let fold f acc node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux acc node = fold_fields aux (f acc node) node in
    aux acc (node :> turkixir_node)

  let iter f node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = f node; iter_fields aux node in
    aux (node :> turkixir_node)

  let filter p node =
    fold (fun acc node -> if p node then node :: acc else acc) [] node
    |> List.rev

  let exists p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node =
      p node || exists_fields aux node in aux (node :> turkixir_node)

  let for_all p node =
    (* Use an auxiliary function here to have a better type for the function *)
    let rec aux node = p node && for_all_fields aux node in
    aux (node :> turkixir_node)

  let lookup_with_kind :
    type a. a node -> [< turkixir_node] -> Sloc.t -> a option =
    fun node_type node sloc ->
      let lookup_res = lookup node sloc in
      let rec aux : a node -> [< turkixir_node] -> a option =
        fun node_type node ->
        match node_type, node with
        | TurkixirNode
          , (#turkixir_node as node) ->
          Some node
        | Arg
          , (#arg as node) ->
          Some node
        | ArgAssoc
          , (#arg_assoc as node) ->
          Some node
        | ArgGen
          , (#arg_gen as node) ->
          Some node
        | KwArgs
          , (#kw_args as node) ->
          Some node
        | VarArgs
          , (#var_args as node) ->
          Some node
        | AsNameNode
          , (#as_name_node as node) ->
          Some node
        | CompIf
          , (#comp_if as node) ->
          Some node
        | CompOpKind
          , (#comp_op_kind as node) ->
          Some node
        | CompOpKindDiamond
          , (#comp_op_kind_diamond as node) ->
          Some node
        | CompOpKindEq
          , (#comp_op_kind_eq as node) ->
          Some node
        | CompOpKindGt
          , (#comp_op_kind_gt as node) ->
          Some node
        | CompOpKindGte
          , (#comp_op_kind_gte as node) ->
          Some node
        | CompOpKindIn
          , (#comp_op_kind_in as node) ->
          Some node
        | CompOpKindIs
          , (#comp_op_kind_is as node) ->
          Some node
        | CompOpKindIsnot
          , (#comp_op_kind_isnot as node) ->
          Some node
        | CompOpKindLt
          , (#comp_op_kind_lt as node) ->
          Some node
        | CompOpKindLte
          , (#comp_op_kind_lte as node) ->
          Some node
        | CompOpKindNoteq
          , (#comp_op_kind_noteq as node) ->
          Some node
        | CompOpKindNotin
          , (#comp_op_kind_notin as node) ->
          Some node
        | Comprehension
          , (#comprehension as node) ->
          Some node
        | CompFor
          , (#comp_for as node) ->
          Some node
        | CompForL
          , (#comp_forl as node) ->
          Some node
        | Decorator
          , (#decorator as node) ->
          Some node
        | DictAssoc
          , (#dict_assoc as node) ->
          Some node
        | ElsePart
          , (#else_part as node) ->
          Some node
        | ExceptPart
          , (#except_part as node) ->
          Some node
        | Expr
          , (#expr as node) ->
          Some node
        | AndExpr
          , (#and_expr as node) ->
          Some node
        | AndOp
          , (#and_op as node) ->
          Some node
        | BinOp
          , (#bin_op as node) ->
          Some node
        | ArithExpr
          , (#arith_expr as node) ->
          Some node
        | ShiftExpr
          , (#shift_expr as node) ->
          Some node
        | Term
          , (#term as node) ->
          Some node
        | CallExpr
          , (#call_expr as node) ->
          Some node
        | CompOp
          , (#comp_op as node) ->
          Some node
        | ConcatStringLit
          , (#concat_string_lit as node) ->
          Some node
        | DictComp
          , (#dict_comp as node) ->
          Some node
        | DictLit
          , (#dict_lit as node) ->
          Some node
        | Dot
          , (#dot as node) ->
          Some node
        | EllipsisExpr
          , (#ellipsis_expr as node) ->
          Some node
        | Factor
          , (#factor as node) ->
          Some node
        | IfExpr
          , (#if_expr as node) ->
          Some node
        | InlineEval
          , (#inline_eval as node) ->
          Some node
        | LambdaDef
          , (#lambda_def as node) ->
          Some node
        | ListComp
          , (#list_comp as node) ->
          Some node
        | ListGen
          , (#list_gen as node) ->
          Some node
        | ListLit
          , (#list_lit as node) ->
          Some node
        | Name
          , (#name as node) ->
          Some node
        | DottedName
          , (#dotted_name as node) ->
          Some node
        | Id
          , (#id as node) ->
          Some node
        | NotOp
          , (#not_op as node) ->
          Some node
        | NumberLit
          , (#number_lit as node) ->
          Some node
        | OrExpr
          , (#or_expr as node) ->
          Some node
        | OrOp
          , (#or_op as node) ->
          Some node
        | Power
          , (#power as node) ->
          Some node
        | SetComp
          , (#set_comp as node) ->
          Some node
        | SetLit
          , (#set_lit as node) ->
          Some node
        | SliceExpr
          , (#slice_expr as node) ->
          Some node
        | ExtSliceExpr
          , (#ext_slice_expr as node) ->
          Some node
        | StringLit
          , (#string_lit as node) ->
          Some node
        | SubscriptExpr
          , (#subscript_expr as node) ->
          Some node
        | TupleLit
          , (#tuple_lit as node) ->
          Some node
        | XorExpr
          , (#xor_expr as node) ->
          Some node
        | YieldExpr
          , (#yield_expr as node) ->
          Some node
        | FileNode
          , (#file_node as node) ->
          Some node
        | ImportStar
          , (#import_star as node) ->
          Some node
        | KwArgsFlag
          , (#kw_args_flag as node) ->
          Some node
        | KwArgsFlagAbsent
          , (#kw_args_flag_absent as node) ->
          Some node
        | KwArgsFlagPresent
          , (#kw_args_flag_present as node) ->
          Some node
        | NL
          , (#nl as node) ->
          Some node
        | Op
          , (#op as node) ->
          Some node
        | Params
          , (#params as node) ->
          Some node
        | RelName
          , (#rel_name as node) ->
          Some node
        | SingleParam
          , (#single_param as node) ->
          Some node
        | Stmt
          , (#stmt as node) ->
          Some node
        | AssertStmt
          , (#assert_stmt as node) ->
          Some node
        | AssignStmt
          , (#assign_stmt as node) ->
          Some node
        | AugAssignStmt
          , (#aug_assign_stmt as node) ->
          Some node
        | BreakStmt
          , (#break_stmt as node) ->
          Some node
        | ContinueStmt
          , (#continue_stmt as node) ->
          Some node
        | Decorated
          , (#decorated as node) ->
          Some node
        | DefStmt
          , (#def_stmt as node) ->
          Some node
        | ClassDef
          , (#class_def as node) ->
          Some node
        | FuncDef
          , (#func_def as node) ->
          Some node
        | DelStmt
          , (#del_stmt as node) ->
          Some node
        | ElifBranch
          , (#elif_branch as node) ->
          Some node
        | ExecStmt
          , (#exec_stmt as node) ->
          Some node
        | ForStmt
          , (#for_stmt as node) ->
          Some node
        | GlobalStmt
          , (#global_stmt as node) ->
          Some node
        | IfStmt
          , (#if_stmt as node) ->
          Some node
        | ImportFrom
          , (#import_from as node) ->
          Some node
        | ImportName
          , (#import_name as node) ->
          Some node
        | PassStmt
          , (#pass_stmt as node) ->
          Some node
        | PrintStmt
          , (#print_stmt as node) ->
          Some node
        | RaiseStmt
          , (#raise_stmt as node) ->
          Some node
        | ReturnStmt
          , (#return_stmt as node) ->
          Some node
        | StreamPrintStmt
          , (#stream_print_stmt as node) ->
          Some node
        | TryStmt
          , (#try_stmt as node) ->
          Some node
        | WhileStmt
          , (#while_stmt as node) ->
          Some node
        | WithStmt
          , (#with_stmt as node) ->
          Some node
        | TurkixirNodeBaseList
          , (#turkixir_node_base_list as node) ->
          Some node
        | ArgList
          , (#arg_list as node) ->
          Some node
        | AsNameNodeList
          , (#as_name_node_list as node) ->
          Some node
        | DecoratorList
          , (#decorator_list as node) ->
          Some node
        | DictAssocList
          , (#dict_assoc_list as node) ->
          Some node
        | DotList
          , (#dot_list as node) ->
          Some node
        | ElifBranchList
          , (#elif_branch_list as node) ->
          Some node
        | ExceptPartList
          , (#except_part_list as node) ->
          Some node
        | ExprList
          , (#expr_list as node) ->
          Some node
        | IdList
          , (#id_list as node) ->
          Some node
        | NLList
          , (#nl_list as node) ->
          Some node
        | SingleParamList
          , (#single_param_list as node) ->
          Some node
        | StringLitList
          , (#string_lit_list as node) ->
          Some node
        | TurkixirNodeList
          , (#turkixir_node_list as node) ->
          Some node
        | VarArgsFlag
          , (#var_args_flag as node) ->
          Some node
        | VarArgsFlagAbsent
          , (#var_args_flag_absent as node) ->
          Some node
        | VarArgsFlagPresent
          , (#var_args_flag_present as node) ->
          Some node
        | _ -> (match parent node with
                | Some parent_node -> aux node_type parent_node
                | _ -> None) in
    match lookup_res with
      | Some node -> aux node_type node
      | _ -> None

  let as_a : type a. a node -> [< turkixir_node ] -> a option =
   fun node_type node ->
    match node_type, (node :> turkixir_node) with
    | TurkixirNode
      , (#turkixir_node as node) ->
        Some node
    | Arg
      , (#arg as node) ->
        Some node
    | ArgAssoc
      , (#arg_assoc as node) ->
        Some node
    | ArgGen
      , (#arg_gen as node) ->
        Some node
    | KwArgs
      , (#kw_args as node) ->
        Some node
    | VarArgs
      , (#var_args as node) ->
        Some node
    | AsNameNode
      , (#as_name_node as node) ->
        Some node
    | CompIf
      , (#comp_if as node) ->
        Some node
    | CompOpKind
      , (#comp_op_kind as node) ->
        Some node
    | CompOpKindDiamond
      , (#comp_op_kind_diamond as node) ->
        Some node
    | CompOpKindEq
      , (#comp_op_kind_eq as node) ->
        Some node
    | CompOpKindGt
      , (#comp_op_kind_gt as node) ->
        Some node
    | CompOpKindGte
      , (#comp_op_kind_gte as node) ->
        Some node
    | CompOpKindIn
      , (#comp_op_kind_in as node) ->
        Some node
    | CompOpKindIs
      , (#comp_op_kind_is as node) ->
        Some node
    | CompOpKindIsnot
      , (#comp_op_kind_isnot as node) ->
        Some node
    | CompOpKindLt
      , (#comp_op_kind_lt as node) ->
        Some node
    | CompOpKindLte
      , (#comp_op_kind_lte as node) ->
        Some node
    | CompOpKindNoteq
      , (#comp_op_kind_noteq as node) ->
        Some node
    | CompOpKindNotin
      , (#comp_op_kind_notin as node) ->
        Some node
    | Comprehension
      , (#comprehension as node) ->
        Some node
    | CompFor
      , (#comp_for as node) ->
        Some node
    | CompForL
      , (#comp_forl as node) ->
        Some node
    | Decorator
      , (#decorator as node) ->
        Some node
    | DictAssoc
      , (#dict_assoc as node) ->
        Some node
    | ElsePart
      , (#else_part as node) ->
        Some node
    | ExceptPart
      , (#except_part as node) ->
        Some node
    | Expr
      , (#expr as node) ->
        Some node
    | AndExpr
      , (#and_expr as node) ->
        Some node
    | AndOp
      , (#and_op as node) ->
        Some node
    | BinOp
      , (#bin_op as node) ->
        Some node
    | ArithExpr
      , (#arith_expr as node) ->
        Some node
    | ShiftExpr
      , (#shift_expr as node) ->
        Some node
    | Term
      , (#term as node) ->
        Some node
    | CallExpr
      , (#call_expr as node) ->
        Some node
    | CompOp
      , (#comp_op as node) ->
        Some node
    | ConcatStringLit
      , (#concat_string_lit as node) ->
        Some node
    | DictComp
      , (#dict_comp as node) ->
        Some node
    | DictLit
      , (#dict_lit as node) ->
        Some node
    | Dot
      , (#dot as node) ->
        Some node
    | EllipsisExpr
      , (#ellipsis_expr as node) ->
        Some node
    | Factor
      , (#factor as node) ->
        Some node
    | IfExpr
      , (#if_expr as node) ->
        Some node
    | InlineEval
      , (#inline_eval as node) ->
        Some node
    | LambdaDef
      , (#lambda_def as node) ->
        Some node
    | ListComp
      , (#list_comp as node) ->
        Some node
    | ListGen
      , (#list_gen as node) ->
        Some node
    | ListLit
      , (#list_lit as node) ->
        Some node
    | Name
      , (#name as node) ->
        Some node
    | DottedName
      , (#dotted_name as node) ->
        Some node
    | Id
      , (#id as node) ->
        Some node
    | NotOp
      , (#not_op as node) ->
        Some node
    | NumberLit
      , (#number_lit as node) ->
        Some node
    | OrExpr
      , (#or_expr as node) ->
        Some node
    | OrOp
      , (#or_op as node) ->
        Some node
    | Power
      , (#power as node) ->
        Some node
    | SetComp
      , (#set_comp as node) ->
        Some node
    | SetLit
      , (#set_lit as node) ->
        Some node
    | SliceExpr
      , (#slice_expr as node) ->
        Some node
    | ExtSliceExpr
      , (#ext_slice_expr as node) ->
        Some node
    | StringLit
      , (#string_lit as node) ->
        Some node
    | SubscriptExpr
      , (#subscript_expr as node) ->
        Some node
    | TupleLit
      , (#tuple_lit as node) ->
        Some node
    | XorExpr
      , (#xor_expr as node) ->
        Some node
    | YieldExpr
      , (#yield_expr as node) ->
        Some node
    | FileNode
      , (#file_node as node) ->
        Some node
    | ImportStar
      , (#import_star as node) ->
        Some node
    | KwArgsFlag
      , (#kw_args_flag as node) ->
        Some node
    | KwArgsFlagAbsent
      , (#kw_args_flag_absent as node) ->
        Some node
    | KwArgsFlagPresent
      , (#kw_args_flag_present as node) ->
        Some node
    | NL
      , (#nl as node) ->
        Some node
    | Op
      , (#op as node) ->
        Some node
    | Params
      , (#params as node) ->
        Some node
    | RelName
      , (#rel_name as node) ->
        Some node
    | SingleParam
      , (#single_param as node) ->
        Some node
    | Stmt
      , (#stmt as node) ->
        Some node
    | AssertStmt
      , (#assert_stmt as node) ->
        Some node
    | AssignStmt
      , (#assign_stmt as node) ->
        Some node
    | AugAssignStmt
      , (#aug_assign_stmt as node) ->
        Some node
    | BreakStmt
      , (#break_stmt as node) ->
        Some node
    | ContinueStmt
      , (#continue_stmt as node) ->
        Some node
    | Decorated
      , (#decorated as node) ->
        Some node
    | DefStmt
      , (#def_stmt as node) ->
        Some node
    | ClassDef
      , (#class_def as node) ->
        Some node
    | FuncDef
      , (#func_def as node) ->
        Some node
    | DelStmt
      , (#del_stmt as node) ->
        Some node
    | ElifBranch
      , (#elif_branch as node) ->
        Some node
    | ExecStmt
      , (#exec_stmt as node) ->
        Some node
    | ForStmt
      , (#for_stmt as node) ->
        Some node
    | GlobalStmt
      , (#global_stmt as node) ->
        Some node
    | IfStmt
      , (#if_stmt as node) ->
        Some node
    | ImportFrom
      , (#import_from as node) ->
        Some node
    | ImportName
      , (#import_name as node) ->
        Some node
    | PassStmt
      , (#pass_stmt as node) ->
        Some node
    | PrintStmt
      , (#print_stmt as node) ->
        Some node
    | RaiseStmt
      , (#raise_stmt as node) ->
        Some node
    | ReturnStmt
      , (#return_stmt as node) ->
        Some node
    | StreamPrintStmt
      , (#stream_print_stmt as node) ->
        Some node
    | TryStmt
      , (#try_stmt as node) ->
        Some node
    | WhileStmt
      , (#while_stmt as node) ->
        Some node
    | WithStmt
      , (#with_stmt as node) ->
        Some node
    | TurkixirNodeBaseList
      , (#turkixir_node_base_list as node) ->
        Some node
    | ArgList
      , (#arg_list as node) ->
        Some node
    | AsNameNodeList
      , (#as_name_node_list as node) ->
        Some node
    | DecoratorList
      , (#decorator_list as node) ->
        Some node
    | DictAssocList
      , (#dict_assoc_list as node) ->
        Some node
    | DotList
      , (#dot_list as node) ->
        Some node
    | ElifBranchList
      , (#elif_branch_list as node) ->
        Some node
    | ExceptPartList
      , (#except_part_list as node) ->
        Some node
    | ExprList
      , (#expr_list as node) ->
        Some node
    | IdList
      , (#id_list as node) ->
        Some node
    | NLList
      , (#nl_list as node) ->
        Some node
    | SingleParamList
      , (#single_param_list as node) ->
        Some node
    | StringLitList
      , (#string_lit_list as node) ->
        Some node
    | TurkixirNodeList
      , (#turkixir_node_list as node) ->
        Some node
    | VarArgsFlag
      , (#var_args_flag as node) ->
        Some node
    | VarArgsFlagAbsent
      , (#var_args_flag_absent as node) ->
        Some node
    | VarArgsFlagPresent
      , (#var_args_flag_present as node) ->
        Some node
    | _ ->
        None

  let find : type a. a node ->  [< turkixir_node ] -> a =
    fun node_type node ->
      let exception Found of a in
      let aux node =
        match node_type, node with
        | TurkixirNode
          , (#turkixir_node as node) ->
            raise (Found node)
        | Arg
          , (#arg as node) ->
            raise (Found node)
        | ArgAssoc
          , (#arg_assoc as node) ->
            raise (Found node)
        | ArgGen
          , (#arg_gen as node) ->
            raise (Found node)
        | KwArgs
          , (#kw_args as node) ->
            raise (Found node)
        | VarArgs
          , (#var_args as node) ->
            raise (Found node)
        | AsNameNode
          , (#as_name_node as node) ->
            raise (Found node)
        | CompIf
          , (#comp_if as node) ->
            raise (Found node)
        | CompOpKind
          , (#comp_op_kind as node) ->
            raise (Found node)
        | CompOpKindDiamond
          , (#comp_op_kind_diamond as node) ->
            raise (Found node)
        | CompOpKindEq
          , (#comp_op_kind_eq as node) ->
            raise (Found node)
        | CompOpKindGt
          , (#comp_op_kind_gt as node) ->
            raise (Found node)
        | CompOpKindGte
          , (#comp_op_kind_gte as node) ->
            raise (Found node)
        | CompOpKindIn
          , (#comp_op_kind_in as node) ->
            raise (Found node)
        | CompOpKindIs
          , (#comp_op_kind_is as node) ->
            raise (Found node)
        | CompOpKindIsnot
          , (#comp_op_kind_isnot as node) ->
            raise (Found node)
        | CompOpKindLt
          , (#comp_op_kind_lt as node) ->
            raise (Found node)
        | CompOpKindLte
          , (#comp_op_kind_lte as node) ->
            raise (Found node)
        | CompOpKindNoteq
          , (#comp_op_kind_noteq as node) ->
            raise (Found node)
        | CompOpKindNotin
          , (#comp_op_kind_notin as node) ->
            raise (Found node)
        | Comprehension
          , (#comprehension as node) ->
            raise (Found node)
        | CompFor
          , (#comp_for as node) ->
            raise (Found node)
        | CompForL
          , (#comp_forl as node) ->
            raise (Found node)
        | Decorator
          , (#decorator as node) ->
            raise (Found node)
        | DictAssoc
          , (#dict_assoc as node) ->
            raise (Found node)
        | ElsePart
          , (#else_part as node) ->
            raise (Found node)
        | ExceptPart
          , (#except_part as node) ->
            raise (Found node)
        | Expr
          , (#expr as node) ->
            raise (Found node)
        | AndExpr
          , (#and_expr as node) ->
            raise (Found node)
        | AndOp
          , (#and_op as node) ->
            raise (Found node)
        | BinOp
          , (#bin_op as node) ->
            raise (Found node)
        | ArithExpr
          , (#arith_expr as node) ->
            raise (Found node)
        | ShiftExpr
          , (#shift_expr as node) ->
            raise (Found node)
        | Term
          , (#term as node) ->
            raise (Found node)
        | CallExpr
          , (#call_expr as node) ->
            raise (Found node)
        | CompOp
          , (#comp_op as node) ->
            raise (Found node)
        | ConcatStringLit
          , (#concat_string_lit as node) ->
            raise (Found node)
        | DictComp
          , (#dict_comp as node) ->
            raise (Found node)
        | DictLit
          , (#dict_lit as node) ->
            raise (Found node)
        | Dot
          , (#dot as node) ->
            raise (Found node)
        | EllipsisExpr
          , (#ellipsis_expr as node) ->
            raise (Found node)
        | Factor
          , (#factor as node) ->
            raise (Found node)
        | IfExpr
          , (#if_expr as node) ->
            raise (Found node)
        | InlineEval
          , (#inline_eval as node) ->
            raise (Found node)
        | LambdaDef
          , (#lambda_def as node) ->
            raise (Found node)
        | ListComp
          , (#list_comp as node) ->
            raise (Found node)
        | ListGen
          , (#list_gen as node) ->
            raise (Found node)
        | ListLit
          , (#list_lit as node) ->
            raise (Found node)
        | Name
          , (#name as node) ->
            raise (Found node)
        | DottedName
          , (#dotted_name as node) ->
            raise (Found node)
        | Id
          , (#id as node) ->
            raise (Found node)
        | NotOp
          , (#not_op as node) ->
            raise (Found node)
        | NumberLit
          , (#number_lit as node) ->
            raise (Found node)
        | OrExpr
          , (#or_expr as node) ->
            raise (Found node)
        | OrOp
          , (#or_op as node) ->
            raise (Found node)
        | Power
          , (#power as node) ->
            raise (Found node)
        | SetComp
          , (#set_comp as node) ->
            raise (Found node)
        | SetLit
          , (#set_lit as node) ->
            raise (Found node)
        | SliceExpr
          , (#slice_expr as node) ->
            raise (Found node)
        | ExtSliceExpr
          , (#ext_slice_expr as node) ->
            raise (Found node)
        | StringLit
          , (#string_lit as node) ->
            raise (Found node)
        | SubscriptExpr
          , (#subscript_expr as node) ->
            raise (Found node)
        | TupleLit
          , (#tuple_lit as node) ->
            raise (Found node)
        | XorExpr
          , (#xor_expr as node) ->
            raise (Found node)
        | YieldExpr
          , (#yield_expr as node) ->
            raise (Found node)
        | FileNode
          , (#file_node as node) ->
            raise (Found node)
        | ImportStar
          , (#import_star as node) ->
            raise (Found node)
        | KwArgsFlag
          , (#kw_args_flag as node) ->
            raise (Found node)
        | KwArgsFlagAbsent
          , (#kw_args_flag_absent as node) ->
            raise (Found node)
        | KwArgsFlagPresent
          , (#kw_args_flag_present as node) ->
            raise (Found node)
        | NL
          , (#nl as node) ->
            raise (Found node)
        | Op
          , (#op as node) ->
            raise (Found node)
        | Params
          , (#params as node) ->
            raise (Found node)
        | RelName
          , (#rel_name as node) ->
            raise (Found node)
        | SingleParam
          , (#single_param as node) ->
            raise (Found node)
        | Stmt
          , (#stmt as node) ->
            raise (Found node)
        | AssertStmt
          , (#assert_stmt as node) ->
            raise (Found node)
        | AssignStmt
          , (#assign_stmt as node) ->
            raise (Found node)
        | AugAssignStmt
          , (#aug_assign_stmt as node) ->
            raise (Found node)
        | BreakStmt
          , (#break_stmt as node) ->
            raise (Found node)
        | ContinueStmt
          , (#continue_stmt as node) ->
            raise (Found node)
        | Decorated
          , (#decorated as node) ->
            raise (Found node)
        | DefStmt
          , (#def_stmt as node) ->
            raise (Found node)
        | ClassDef
          , (#class_def as node) ->
            raise (Found node)
        | FuncDef
          , (#func_def as node) ->
            raise (Found node)
        | DelStmt
          , (#del_stmt as node) ->
            raise (Found node)
        | ElifBranch
          , (#elif_branch as node) ->
            raise (Found node)
        | ExecStmt
          , (#exec_stmt as node) ->
            raise (Found node)
        | ForStmt
          , (#for_stmt as node) ->
            raise (Found node)
        | GlobalStmt
          , (#global_stmt as node) ->
            raise (Found node)
        | IfStmt
          , (#if_stmt as node) ->
            raise (Found node)
        | ImportFrom
          , (#import_from as node) ->
            raise (Found node)
        | ImportName
          , (#import_name as node) ->
            raise (Found node)
        | PassStmt
          , (#pass_stmt as node) ->
            raise (Found node)
        | PrintStmt
          , (#print_stmt as node) ->
            raise (Found node)
        | RaiseStmt
          , (#raise_stmt as node) ->
            raise (Found node)
        | ReturnStmt
          , (#return_stmt as node) ->
            raise (Found node)
        | StreamPrintStmt
          , (#stream_print_stmt as node) ->
            raise (Found node)
        | TryStmt
          , (#try_stmt as node) ->
            raise (Found node)
        | WhileStmt
          , (#while_stmt as node) ->
            raise (Found node)
        | WithStmt
          , (#with_stmt as node) ->
            raise (Found node)
        | TurkixirNodeBaseList
          , (#turkixir_node_base_list as node) ->
            raise (Found node)
        | ArgList
          , (#arg_list as node) ->
            raise (Found node)
        | AsNameNodeList
          , (#as_name_node_list as node) ->
            raise (Found node)
        | DecoratorList
          , (#decorator_list as node) ->
            raise (Found node)
        | DictAssocList
          , (#dict_assoc_list as node) ->
            raise (Found node)
        | DotList
          , (#dot_list as node) ->
            raise (Found node)
        | ElifBranchList
          , (#elif_branch_list as node) ->
            raise (Found node)
        | ExceptPartList
          , (#except_part_list as node) ->
            raise (Found node)
        | ExprList
          , (#expr_list as node) ->
            raise (Found node)
        | IdList
          , (#id_list as node) ->
            raise (Found node)
        | NLList
          , (#nl_list as node) ->
            raise (Found node)
        | SingleParamList
          , (#single_param_list as node) ->
            raise (Found node)
        | StringLitList
          , (#string_lit_list as node) ->
            raise (Found node)
        | TurkixirNodeList
          , (#turkixir_node_list as node) ->
            raise (Found node)
        | VarArgsFlag
          , (#var_args_flag as node) ->
            raise (Found node)
        | VarArgsFlagAbsent
          , (#var_args_flag_absent as node) ->
            raise (Found node)
        | VarArgsFlagPresent
          , (#var_args_flag_present as node) ->
            raise (Found node)
        | _ ->
          ()
      in
      try
        iter aux node;
        raise Not_found
      with (Found node) -> node



  let findall : type a. a node ->  [< turkixir_node ] -> a list =
    fun node_type node ->
      let aux : a list -> [< turkixir_node ] -> a list =
       fun acc node ->
        match node_type, node with
        | TurkixirNode
          , (#turkixir_node as node) ->
            node :: acc
        | Arg
          , (#arg as node) ->
            node :: acc
        | ArgAssoc
          , (#arg_assoc as node) ->
            node :: acc
        | ArgGen
          , (#arg_gen as node) ->
            node :: acc
        | KwArgs
          , (#kw_args as node) ->
            node :: acc
        | VarArgs
          , (#var_args as node) ->
            node :: acc
        | AsNameNode
          , (#as_name_node as node) ->
            node :: acc
        | CompIf
          , (#comp_if as node) ->
            node :: acc
        | CompOpKind
          , (#comp_op_kind as node) ->
            node :: acc
        | CompOpKindDiamond
          , (#comp_op_kind_diamond as node) ->
            node :: acc
        | CompOpKindEq
          , (#comp_op_kind_eq as node) ->
            node :: acc
        | CompOpKindGt
          , (#comp_op_kind_gt as node) ->
            node :: acc
        | CompOpKindGte
          , (#comp_op_kind_gte as node) ->
            node :: acc
        | CompOpKindIn
          , (#comp_op_kind_in as node) ->
            node :: acc
        | CompOpKindIs
          , (#comp_op_kind_is as node) ->
            node :: acc
        | CompOpKindIsnot
          , (#comp_op_kind_isnot as node) ->
            node :: acc
        | CompOpKindLt
          , (#comp_op_kind_lt as node) ->
            node :: acc
        | CompOpKindLte
          , (#comp_op_kind_lte as node) ->
            node :: acc
        | CompOpKindNoteq
          , (#comp_op_kind_noteq as node) ->
            node :: acc
        | CompOpKindNotin
          , (#comp_op_kind_notin as node) ->
            node :: acc
        | Comprehension
          , (#comprehension as node) ->
            node :: acc
        | CompFor
          , (#comp_for as node) ->
            node :: acc
        | CompForL
          , (#comp_forl as node) ->
            node :: acc
        | Decorator
          , (#decorator as node) ->
            node :: acc
        | DictAssoc
          , (#dict_assoc as node) ->
            node :: acc
        | ElsePart
          , (#else_part as node) ->
            node :: acc
        | ExceptPart
          , (#except_part as node) ->
            node :: acc
        | Expr
          , (#expr as node) ->
            node :: acc
        | AndExpr
          , (#and_expr as node) ->
            node :: acc
        | AndOp
          , (#and_op as node) ->
            node :: acc
        | BinOp
          , (#bin_op as node) ->
            node :: acc
        | ArithExpr
          , (#arith_expr as node) ->
            node :: acc
        | ShiftExpr
          , (#shift_expr as node) ->
            node :: acc
        | Term
          , (#term as node) ->
            node :: acc
        | CallExpr
          , (#call_expr as node) ->
            node :: acc
        | CompOp
          , (#comp_op as node) ->
            node :: acc
        | ConcatStringLit
          , (#concat_string_lit as node) ->
            node :: acc
        | DictComp
          , (#dict_comp as node) ->
            node :: acc
        | DictLit
          , (#dict_lit as node) ->
            node :: acc
        | Dot
          , (#dot as node) ->
            node :: acc
        | EllipsisExpr
          , (#ellipsis_expr as node) ->
            node :: acc
        | Factor
          , (#factor as node) ->
            node :: acc
        | IfExpr
          , (#if_expr as node) ->
            node :: acc
        | InlineEval
          , (#inline_eval as node) ->
            node :: acc
        | LambdaDef
          , (#lambda_def as node) ->
            node :: acc
        | ListComp
          , (#list_comp as node) ->
            node :: acc
        | ListGen
          , (#list_gen as node) ->
            node :: acc
        | ListLit
          , (#list_lit as node) ->
            node :: acc
        | Name
          , (#name as node) ->
            node :: acc
        | DottedName
          , (#dotted_name as node) ->
            node :: acc
        | Id
          , (#id as node) ->
            node :: acc
        | NotOp
          , (#not_op as node) ->
            node :: acc
        | NumberLit
          , (#number_lit as node) ->
            node :: acc
        | OrExpr
          , (#or_expr as node) ->
            node :: acc
        | OrOp
          , (#or_op as node) ->
            node :: acc
        | Power
          , (#power as node) ->
            node :: acc
        | SetComp
          , (#set_comp as node) ->
            node :: acc
        | SetLit
          , (#set_lit as node) ->
            node :: acc
        | SliceExpr
          , (#slice_expr as node) ->
            node :: acc
        | ExtSliceExpr
          , (#ext_slice_expr as node) ->
            node :: acc
        | StringLit
          , (#string_lit as node) ->
            node :: acc
        | SubscriptExpr
          , (#subscript_expr as node) ->
            node :: acc
        | TupleLit
          , (#tuple_lit as node) ->
            node :: acc
        | XorExpr
          , (#xor_expr as node) ->
            node :: acc
        | YieldExpr
          , (#yield_expr as node) ->
            node :: acc
        | FileNode
          , (#file_node as node) ->
            node :: acc
        | ImportStar
          , (#import_star as node) ->
            node :: acc
        | KwArgsFlag
          , (#kw_args_flag as node) ->
            node :: acc
        | KwArgsFlagAbsent
          , (#kw_args_flag_absent as node) ->
            node :: acc
        | KwArgsFlagPresent
          , (#kw_args_flag_present as node) ->
            node :: acc
        | NL
          , (#nl as node) ->
            node :: acc
        | Op
          , (#op as node) ->
            node :: acc
        | Params
          , (#params as node) ->
            node :: acc
        | RelName
          , (#rel_name as node) ->
            node :: acc
        | SingleParam
          , (#single_param as node) ->
            node :: acc
        | Stmt
          , (#stmt as node) ->
            node :: acc
        | AssertStmt
          , (#assert_stmt as node) ->
            node :: acc
        | AssignStmt
          , (#assign_stmt as node) ->
            node :: acc
        | AugAssignStmt
          , (#aug_assign_stmt as node) ->
            node :: acc
        | BreakStmt
          , (#break_stmt as node) ->
            node :: acc
        | ContinueStmt
          , (#continue_stmt as node) ->
            node :: acc
        | Decorated
          , (#decorated as node) ->
            node :: acc
        | DefStmt
          , (#def_stmt as node) ->
            node :: acc
        | ClassDef
          , (#class_def as node) ->
            node :: acc
        | FuncDef
          , (#func_def as node) ->
            node :: acc
        | DelStmt
          , (#del_stmt as node) ->
            node :: acc
        | ElifBranch
          , (#elif_branch as node) ->
            node :: acc
        | ExecStmt
          , (#exec_stmt as node) ->
            node :: acc
        | ForStmt
          , (#for_stmt as node) ->
            node :: acc
        | GlobalStmt
          , (#global_stmt as node) ->
            node :: acc
        | IfStmt
          , (#if_stmt as node) ->
            node :: acc
        | ImportFrom
          , (#import_from as node) ->
            node :: acc
        | ImportName
          , (#import_name as node) ->
            node :: acc
        | PassStmt
          , (#pass_stmt as node) ->
            node :: acc
        | PrintStmt
          , (#print_stmt as node) ->
            node :: acc
        | RaiseStmt
          , (#raise_stmt as node) ->
            node :: acc
        | ReturnStmt
          , (#return_stmt as node) ->
            node :: acc
        | StreamPrintStmt
          , (#stream_print_stmt as node) ->
            node :: acc
        | TryStmt
          , (#try_stmt as node) ->
            node :: acc
        | WhileStmt
          , (#while_stmt as node) ->
            node :: acc
        | WithStmt
          , (#with_stmt as node) ->
            node :: acc
        | TurkixirNodeBaseList
          , (#turkixir_node_base_list as node) ->
            node :: acc
        | ArgList
          , (#arg_list as node) ->
            node :: acc
        | AsNameNodeList
          , (#as_name_node_list as node) ->
            node :: acc
        | DecoratorList
          , (#decorator_list as node) ->
            node :: acc
        | DictAssocList
          , (#dict_assoc_list as node) ->
            node :: acc
        | DotList
          , (#dot_list as node) ->
            node :: acc
        | ElifBranchList
          , (#elif_branch_list as node) ->
            node :: acc
        | ExceptPartList
          , (#except_part_list as node) ->
            node :: acc
        | ExprList
          , (#expr_list as node) ->
            node :: acc
        | IdList
          , (#id_list as node) ->
            node :: acc
        | NLList
          , (#nl_list as node) ->
            node :: acc
        | SingleParamList
          , (#single_param_list as node) ->
            node :: acc
        | StringLitList
          , (#string_lit_list as node) ->
            node :: acc
        | TurkixirNodeList
          , (#turkixir_node_list as node) ->
            node :: acc
        | VarArgsFlag
          , (#var_args_flag as node) ->
            node :: acc
        | VarArgsFlagAbsent
          , (#var_args_flag_absent as node) ->
            node :: acc
        | VarArgsFlagPresent
          , (#var_args_flag_present as node) ->
            node :: acc
        | _ ->
          acc
      in
      List.rev (fold aux [] node)

  let fields_with_names node =
    let aux i x =
      (Format.sprintf "item_%d" i), x
    in
    match (node :> turkixir_node) with
    | `VarArgsFlagPresent value ->
        [
        ]
    | `VarArgsFlagAbsent value ->
        [
        ]
    | `TurkixirNodeList value ->
        List.mapi aux (children_opt node)
    | `StringLitList value ->
        List.mapi aux (children_opt node)
    | `SingleParamList value ->
        List.mapi aux (children_opt node)
    | `NLList value ->
        List.mapi aux (children_opt node)
    | `IdList value ->
        List.mapi aux (children_opt node)
    | `ExprList value ->
        List.mapi aux (children_opt node)
    | `ExceptPartList value ->
        List.mapi aux (children_opt node)
    | `ElifBranchList value ->
        List.mapi aux (children_opt node)
    | `DotList value ->
        List.mapi aux (children_opt node)
    | `DictAssocList value ->
        List.mapi aux (children_opt node)
    | `DecoratorList value ->
        List.mapi aux (children_opt node)
    | `AsNameNodeList value ->
        List.mapi aux (children_opt node)
    | `ArgList value ->
        List.mapi aux (children_opt node)
    | `WithStmt value ->
        [
        (try
           ("bindings"
           , Some (Lazy.force value.f_bindings
                    :> turkixir_node))
        with SyntaxError ->
          ("bindings", None) );
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `WhileStmt value ->
        [
        (try
           ("cond_test"
           , Some (Lazy.force value.f_cond_test
                    :> turkixir_node))
        with SyntaxError ->
          ("cond_test", None) );
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ("else_part"
        , (Lazy.force value.f_else_part
           :> turkixir_node option));
        ]
    | `TryStmt value ->
        [
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        (try
           ("except_parts"
           , Some (Lazy.force value.f_except_parts
                    :> turkixir_node))
        with SyntaxError ->
          ("except_parts", None) );
        ("else_part"
        , (Lazy.force value.f_else_part
           :> turkixir_node option));
        ("finally_part"
        , (Lazy.force value.f_finally_part
           :> turkixir_node option));
        ]
    | `StreamPrintStmt value ->
        [
        (try
           ("stream_expr"
           , Some (Lazy.force value.f_stream_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("stream_expr", None) );
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `ReturnStmt value ->
        [
        ("exprs"
        , (Lazy.force value.f_exprs
           :> turkixir_node option));
        ]
    | `RaiseStmt value ->
        [
        ("exprs"
        , (Lazy.force value.f_exprs
           :> turkixir_node option));
        ]
    | `PrintStmt value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `PassStmt value ->
        [
        ]
    | `ImportName value ->
        [
        (try
           ("imported_names"
           , Some (Lazy.force value.f_imported_names
                    :> turkixir_node))
        with SyntaxError ->
          ("imported_names", None) );
        ]
    | `ImportFrom value ->
        [
        (try
           ("rel_name"
           , Some (Lazy.force value.f_rel_name
                    :> turkixir_node))
        with SyntaxError ->
          ("rel_name", None) );
        (try
           ("imported"
           , Some (Lazy.force value.f_imported
                    :> turkixir_node))
        with SyntaxError ->
          ("imported", None) );
        ]
    | `IfStmt value ->
        [
        (try
           ("cond_test"
           , Some (Lazy.force value.f_cond_test
                    :> turkixir_node))
        with SyntaxError ->
          ("cond_test", None) );
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        (try
           ("elif_branchs"
           , Some (Lazy.force value.f_elif_branchs
                    :> turkixir_node))
        with SyntaxError ->
          ("elif_branchs", None) );
        ("else_part"
        , (Lazy.force value.f_else_part
           :> turkixir_node option));
        ]
    | `GlobalStmt value ->
        [
        (try
           ("names"
           , Some (Lazy.force value.f_names
                    :> turkixir_node))
        with SyntaxError ->
          ("names", None) );
        ]
    | `ForStmt value ->
        [
        (try
           ("bindings"
           , Some (Lazy.force value.f_bindings
                    :> turkixir_node))
        with SyntaxError ->
          ("bindings", None) );
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ("else_part"
        , (Lazy.force value.f_else_part
           :> turkixir_node option));
        ]
    | `ExecStmt value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ("in_list"
        , (Lazy.force value.f_in_list
           :> turkixir_node option));
        ]
    | `ElifBranch value ->
        [
        (try
           ("cond_test"
           , Some (Lazy.force value.f_cond_test
                    :> turkixir_node))
        with SyntaxError ->
          ("cond_test", None) );
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `DelStmt value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `FuncDef value ->
        [
        (try
           ("name"
           , Some (Lazy.force value.f_name
                    :> turkixir_node))
        with SyntaxError ->
          ("name", None) );
        ("parameters"
        , (Lazy.force value.f_parameters
           :> turkixir_node option));
        (try
           ("body"
           , Some (Lazy.force value.f_body
                    :> turkixir_node))
        with SyntaxError ->
          ("body", None) );
        ]
    | `ClassDef value ->
        [
        (try
           ("name"
           , Some (Lazy.force value.f_name
                    :> turkixir_node))
        with SyntaxError ->
          ("name", None) );
        ("bases"
        , (Lazy.force value.f_bases
           :> turkixir_node option));
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `Decorated value ->
        [
        (try
           ("decorators"
           , Some (Lazy.force value.f_decorators
                    :> turkixir_node))
        with SyntaxError ->
          ("decorators", None) );
        (try
           ("defn"
           , Some (Lazy.force value.f_defn
                    :> turkixir_node))
        with SyntaxError ->
          ("defn", None) );
        ]
    | `ContinueStmt value ->
        [
        ]
    | `BreakStmt value ->
        [
        ]
    | `AugAssignStmt value ->
        [
        (try
           ("l_value"
           , Some (Lazy.force value.f_l_value
                    :> turkixir_node))
        with SyntaxError ->
          ("l_value", None) );
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("r_value"
           , Some (Lazy.force value.f_r_value
                    :> turkixir_node))
        with SyntaxError ->
          ("r_value", None) );
        ]
    | `AssignStmt value ->
        [
        (try
           ("l_value"
           , Some (Lazy.force value.f_l_value
                    :> turkixir_node))
        with SyntaxError ->
          ("l_value", None) );
        (try
           ("r_values"
           , Some (Lazy.force value.f_r_values
                    :> turkixir_node))
        with SyntaxError ->
          ("r_values", None) );
        ]
    | `AssertStmt value ->
        [
        (try
           ("test_expr"
           , Some (Lazy.force value.f_test_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("test_expr", None) );
        ("msg"
        , (Lazy.force value.f_msg
           :> turkixir_node option));
        ]
    | `SingleParam value ->
        [
        (try
           ("is_varargs"
           , Some (Lazy.force value.f_is_varargs
                    :> turkixir_node))
        with SyntaxError ->
          ("is_varargs", None) );
        (try
           ("is_kwargs"
           , Some (Lazy.force value.f_is_kwargs
                    :> turkixir_node))
        with SyntaxError ->
          ("is_kwargs", None) );
        (try
           ("name"
           , Some (Lazy.force value.f_name
                    :> turkixir_node))
        with SyntaxError ->
          ("name", None) );
        ("default_value"
        , (Lazy.force value.f_default_value
           :> turkixir_node option));
        ]
    | `RelName value ->
        [
        (try
           ("dots"
           , Some (Lazy.force value.f_dots
                    :> turkixir_node))
        with SyntaxError ->
          ("dots", None) );
        ("name"
        , (Lazy.force value.f_name
           :> turkixir_node option));
        ]
    | `Params value ->
        [
        (try
           ("single_params"
           , Some (Lazy.force value.f_single_params
                    :> turkixir_node))
        with SyntaxError ->
          ("single_params", None) );
        ]
    | `Op value ->
        [
        ]
    | `NL value ->
        [
        ]
    | `KwArgsFlagPresent value ->
        [
        ]
    | `KwArgsFlagAbsent value ->
        [
        ]
    | `ImportStar value ->
        [
        ]
    | `FileNode value ->
        [
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `YieldExpr value ->
        [
        ("exprs"
        , (Lazy.force value.f_exprs
           :> turkixir_node option));
        ]
    | `XorExpr value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `TupleLit value ->
        [
        ("exprs"
        , (Lazy.force value.f_exprs
           :> turkixir_node option));
        ]
    | `SubscriptExpr value ->
        [
        (try
           ("prefix"
           , Some (Lazy.force value.f_prefix
                    :> turkixir_node))
        with SyntaxError ->
          ("prefix", None) );
        (try
           ("suffix"
           , Some (Lazy.force value.f_suffix
                    :> turkixir_node))
        with SyntaxError ->
          ("suffix", None) );
        ]
    | `StringLit value ->
        [
        ]
    | `ExtSliceExpr value ->
        [
        ("first"
        , (Lazy.force value.f_first
           :> turkixir_node option));
        ("last"
        , (Lazy.force value.f_last
           :> turkixir_node option));
        ("stride"
        , (Lazy.force value.f_stride
           :> turkixir_node option));
        ]
    | `SliceExpr value ->
        [
        ("first"
        , (Lazy.force value.f_first
           :> turkixir_node option));
        ("last"
        , (Lazy.force value.f_last
           :> turkixir_node option));
        ]
    | `SetLit value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `SetComp value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("comprehension"
           , Some (Lazy.force value.f_comprehension
                    :> turkixir_node))
        with SyntaxError ->
          ("comprehension", None) );
        ]
    | `Power value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `OrOp value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `OrExpr value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `NumberLit value ->
        [
        ]
    | `NotOp value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]
    | `Id value ->
        [
        ]
    | `DottedName value ->
        [
        (try
           ("prefix"
           , Some (Lazy.force value.f_prefix
                    :> turkixir_node))
        with SyntaxError ->
          ("prefix", None) );
        (try
           ("suffix"
           , Some (Lazy.force value.f_suffix
                    :> turkixir_node))
        with SyntaxError ->
          ("suffix", None) );
        ]
    | `ListLit value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `ListGen value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("comprehension"
           , Some (Lazy.force value.f_comprehension
                    :> turkixir_node))
        with SyntaxError ->
          ("comprehension", None) );
        ]
    | `ListComp value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("comprehension"
           , Some (Lazy.force value.f_comprehension
                    :> turkixir_node))
        with SyntaxError ->
          ("comprehension", None) );
        ]
    | `LambdaDef value ->
        [
        (try
           ("args"
           , Some (Lazy.force value.f_args
                    :> turkixir_node))
        with SyntaxError ->
          ("args", None) );
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]
    | `InlineEval value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        ]
    | `IfExpr value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("cond"
           , Some (Lazy.force value.f_cond
                    :> turkixir_node))
        with SyntaxError ->
          ("cond", None) );
        (try
           ("else_expr"
           , Some (Lazy.force value.f_else_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("else_expr", None) );
        ]
    | `Factor value ->
        [
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]
    | `EllipsisExpr value ->
        [
        ]
    | `Dot value ->
        [
        ]
    | `DictLit value ->
        [
        (try
           ("assocs"
           , Some (Lazy.force value.f_assocs
                    :> turkixir_node))
        with SyntaxError ->
          ("assocs", None) );
        ]
    | `DictComp value ->
        [
        (try
           ("assoc"
           , Some (Lazy.force value.f_assoc
                    :> turkixir_node))
        with SyntaxError ->
          ("assoc", None) );
        (try
           ("comprehension"
           , Some (Lazy.force value.f_comprehension
                    :> turkixir_node))
        with SyntaxError ->
          ("comprehension", None) );
        ]
    | `ConcatStringLit value ->
        [
        (try
           ("first_str"
           , Some (Lazy.force value.f_first_str
                    :> turkixir_node))
        with SyntaxError ->
          ("first_str", None) );
        (try
           ("subsequent_str"
           , Some (Lazy.force value.f_subsequent_str
                    :> turkixir_node))
        with SyntaxError ->
          ("subsequent_str", None) );
        ]
    | `CompOp value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `CallExpr value ->
        [
        (try
           ("prefix"
           , Some (Lazy.force value.f_prefix
                    :> turkixir_node))
        with SyntaxError ->
          ("prefix", None) );
        (try
           ("suffix"
           , Some (Lazy.force value.f_suffix
                    :> turkixir_node))
        with SyntaxError ->
          ("suffix", None) );
        ]
    | `Term value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `ShiftExpr value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `ArithExpr value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("op"
           , Some (Lazy.force value.f_op
                    :> turkixir_node))
        with SyntaxError ->
          ("op", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `AndOp value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `AndExpr value ->
        [
        (try
           ("left"
           , Some (Lazy.force value.f_left
                    :> turkixir_node))
        with SyntaxError ->
          ("left", None) );
        (try
           ("right"
           , Some (Lazy.force value.f_right
                    :> turkixir_node))
        with SyntaxError ->
          ("right", None) );
        ]
    | `ExceptPart value ->
        [
        ("as_name"
        , (Lazy.force value.f_as_name
           :> turkixir_node option));
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `ElsePart value ->
        [
        (try
           ("statements"
           , Some (Lazy.force value.f_statements
                    :> turkixir_node))
        with SyntaxError ->
          ("statements", None) );
        ]
    | `DictAssoc value ->
        [
        (try
           ("key"
           , Some (Lazy.force value.f_key
                    :> turkixir_node))
        with SyntaxError ->
          ("key", None) );
        (try
           ("value"
           , Some (Lazy.force value.f_value
                    :> turkixir_node))
        with SyntaxError ->
          ("value", None) );
        ]
    | `Decorator value ->
        [
        (try
           ("dec_name"
           , Some (Lazy.force value.f_dec_name
                    :> turkixir_node))
        with SyntaxError ->
          ("dec_name", None) );
        ("arg_list"
        , (Lazy.force value.f_arg_list
           :> turkixir_node option));
        ]
    | `CompForL value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        (try
           ("target"
           , Some (Lazy.force value.f_target
                    :> turkixir_node))
        with SyntaxError ->
          ("target", None) );
        ("comp"
        , (Lazy.force value.f_comp
           :> turkixir_node option));
        ]
    | `CompFor value ->
        [
        (try
           ("exprs"
           , Some (Lazy.force value.f_exprs
                    :> turkixir_node))
        with SyntaxError ->
          ("exprs", None) );
        (try
           ("target"
           , Some (Lazy.force value.f_target
                    :> turkixir_node))
        with SyntaxError ->
          ("target", None) );
        ("comp"
        , (Lazy.force value.f_comp
           :> turkixir_node option));
        ]
    | `CompOpKindNotin value ->
        [
        ]
    | `CompOpKindNoteq value ->
        [
        ]
    | `CompOpKindLte value ->
        [
        ]
    | `CompOpKindLt value ->
        [
        ]
    | `CompOpKindIsnot value ->
        [
        ]
    | `CompOpKindIs value ->
        [
        ]
    | `CompOpKindIn value ->
        [
        ]
    | `CompOpKindGte value ->
        [
        ]
    | `CompOpKindGt value ->
        [
        ]
    | `CompOpKindEq value ->
        [
        ]
    | `CompOpKindDiamond value ->
        [
        ]
    | `CompIf value ->
        [
        (try
           ("test"
           , Some (Lazy.force value.f_test
                    :> turkixir_node))
        with SyntaxError ->
          ("test", None) );
        ("comp"
        , (Lazy.force value.f_comp
           :> turkixir_node option));
        ]
    | `AsNameNode value ->
        [
        (try
           ("imported"
           , Some (Lazy.force value.f_imported
                    :> turkixir_node))
        with SyntaxError ->
          ("imported", None) );
        ("as_name"
        , (Lazy.force value.f_as_name
           :> turkixir_node option));
        ]
    | `VarArgs value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]
    | `KwArgs value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]
    | `ArgGen value ->
        [
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        (try
           ("comprehension"
           , Some (Lazy.force value.f_comprehension
                    :> turkixir_node))
        with SyntaxError ->
          ("comprehension", None) );
        ]
    | `ArgAssoc value ->
        [
        ("name"
        , (Lazy.force value.f_name
           :> turkixir_node option));
        (try
           ("expr"
           , Some (Lazy.force value.f_expr
                    :> turkixir_node))
        with SyntaxError ->
          ("expr", None) );
        ]

  let rec pp_tree fmt node =
    let rec pp_node_field fmt (name, node) =
      match node with
      | Some node ->
          Format.fprintf fmt "@[<v 2>%s:@ %a@]" name pp_node node
      | None ->
          Format.fprintf fmt "@[<v 2>%s: None@]" name
    and pp_node_fields fmt node =
      let name_field_list = fields_with_names node in
      match name_field_list with
      | [] ->
          ()
      | l ->
          Format.fprintf fmt "@ @[<v>%a@]"
            (Format.pp_print_list pp_node_field) l
    and pp_node fmt node =
      let repr = entity_image node in
      let len = String.length repr in
      let erepr = String.sub repr 1 (len - 2) in
      Format.fprintf fmt "@[<v 2>%s%s%a@]"
        erepr
        (if is_token_node node then (": " ^ (text node)) else "")
        pp_node_fields node
    in
    let default = Format.pp_get_formatter_out_functions fmt () in
    let out_indent n =
      let the_end = n in
      let rec make n =
        if n = the_end then ""
        else (if n mod 4 = 2 then "|" else " ") ^ make (n + 1)
      in
      default.out_string (make 0) 0 n
    in
    Format.pp_set_formatter_out_functions fmt {default with out_indent} ;
    Format.fprintf fmt "%a%!" pp_node (node :> turkixir_node);
    Format.pp_set_formatter_out_functions fmt default


end

