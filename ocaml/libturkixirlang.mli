





(**
 * OCaml binding of the Libturkixirlang API.
 *)

(**
 * Raised when introspection functions (``Libturkixirlang.Introspection``) are
 * provided mismatching types/values.
 *)
exception BadTypeError of string

(**
 * Raised when introspection functions (``Libturkixirlang.Introspection``) are
 * passed an out of bounds index.
 *)
exception OutOfBoundsError of string

(**
 * Raised by lexing functions (``Libturkixirlang.Lexer``) when the input
 * contains an invalid byte sequence.
 *)
exception InvalidInput of string

(**
 * Exception raise when an invalid symbol is passed to a subprogram.
 *)
exception InvalidSymbolError of string

(**
 * Raised when an invalid unit name is provided.
 *)
exception InvalidUnitNameError of string

(**
 * Exception raised in language bindings when the underlying C API reports an
 * unexpected error that occurred in the library.

 * This kind of exception is raised for internal errors: they should never
 * happen in normal situations and if they are raised at some point, it means
 * the library state is potentially corrupted.

 * Nevertheless, the library does its best not to crash the program,
 * materializing internal errors using this kind of exception.
 *)
exception NativeException of string

(**
 * Exception raised when an API is called while its preconditions are not
 * satisfied.
 *)
exception PreconditionFailure of string

(**
 * Exception that is raised when an error occurs while evaluating any AST node
 * method whose name starts with ``p_``. This is the only exceptions that such
 * functions can raise.
 *)
exception PropertyError of string

(**
 * Exception raised when the provided arguments for a template don't match what
 * the template expects.
 *)
exception TemplateArgsError of string

(**
 * Exception raised when a template has an invalid syntax, such as badly
 * formatted placeholders.
 *)
exception TemplateFormatError of string

(**
 * Exception raised when the instantiation of a template cannot be parsed.
 *)
exception TemplateInstantiationError of string

(**
 * Exception raised while trying to access data that was deallocated. This
 * happens when one tries to use a node whose unit has been reparsed, for
 * instance.
 *)
exception StaleReferenceError of string

(**
 * Raised by lexing functions (``Libturkixirlang.Lexer``) when the input
 * charset is not supported.
 *)
exception UnknownCharset of string





(**
 * Raised if a field of a node is null due to a syntax error
 *)
exception SyntaxError

module AnalysisUnitKind : sig
  (**
   * Specify a kind of analysis unit. Specification units provide an interface
   * to the outer world while body units provide an implementation for the
   * corresponding interface.
   *)

  type t =
  | UnitSpecification
  | UnitBody

  val name : unit -> string
end

module LookupKind : sig
  

  type t =
  | Recursive
  | Flat
  | Minimal

  val name : unit -> string
end

module DesignatedEnvKind : sig
  (**
   * Discriminant for DesignatedEnv structures.
   *)

  type t =
  | None
  | CurrentEnv
  | NamedEnv
  | DirectEnv

  val name : unit -> string
end

module GrammarRule : sig
  (**
   * Gramar rule to use for parsing.
   *)

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

  val name : unit -> string
end

val default_grammar_rule : GrammarRule.t

module Camomile : CamomileLibrary.Type

module Sloc : sig
  (**
   * Location in a source file. Line and column numbers are one-based.
   *)

  type t = {
    line : int;
    column : int;
  }
end

module SlocRange : sig
  (**
   * Location of a span of text in a source file.
   *)

  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  val pp : Format.formatter -> t -> unit
end

module Diagnostic : sig
  (**
   * Diagnostic for an analysis unit: cannot open the source file, parsing
   * error, ...
   *)

  type t = {
    sloc_range : SlocRange.t;
    message : string;
  }
end

module TokenData : sig
  type t
end

module Token : sig
  (**
   * Reference to a token in an analysis unit.
   *)

  type dummy_context

  type t = {
    context : dummy_context;
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  val kind_name : t -> string
  (**
   * Return a human-readable name for a token kind.
   *)

  val text_range : t -> t -> string
  (**
   * Compute the source buffer slice corresponding to the text that spans
   * between the ``First`` and ``Last`` tokens (both included). This yields an
   * empty slice if ``Last`` actually appears before ``First``.
  
   * if ``First`` and ``Last`` don't belong to the same analysis unit.
   *)

  val is_trivia : t -> bool
  (**
   * Return whether this token is a trivia. If it's not, it's a regular token.
   *)

  val index : t -> int
  (**
   * Zero-based index for this token/trivia. Tokens and trivias get their own
   * index space.
   *)

  val next : t -> t
  (**
   * Return a reference to the first token scanned in this unit.
   *)

  val previous : t -> t
  (**
   * Return a reference to the last token scanned in this unit.
   *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val is_equivalent : t -> t -> bool
  (**
   * Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   * means that their position in the stream won't be taken into account, only
   * the kind and text of the token.
   *)

  val pp : Format.formatter -> t -> unit
end

module BigInteger : sig
  type t = Z.t
end

module BareNode : sig
  type t
end

module Rebindings : sig
  type t
end

module UnitProvider : sig
  (**
   * Interface to fetch analysis units from a name and a unit kind.
  
   * The unit provider mechanism provides an abstraction which assumes that to
   * any couple (unit name, unit kind) we can associate at most one source file.
   * This means that several couples can be associated to the same source file,
   * but on the other hand, only one one source file can be associated to a
   * couple.
  
   * This is used to make the semantic analysis able to switch from one analysis
   * units to another.
  
   * See the documentation of each unit provider for the exact semantics of the
   * unit name/kind information.
   *)

  type t

  

end

type analysis_context

and analysis_unit

and entity


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



module Entity : sig
  type t = entity

  val info : t -> entity_info
end

module AnalysisUnit : sig
  (**
   * This type represents the analysis of a single file.
  
   * This type has strong-reference semantics and is ref-counted. Furthermore, a
   * reference to a unit contains an implicit reference to the context that owns
   * it. This means that keeping a reference to a unit will keep the context and
   * all the unit it contains allocated.
   *)

  type t = analysis_unit

  val root : t -> turkixir_node option
  (**
   * Return the root node for this unit, or ``None`` if there is none.
   *)

  val diagnostics : t -> Diagnostic.t list
  (**
   * Diagnostics for this unit.
   *)

  val filename : t -> string
  (**
   * Return the filename this unit is associated to.
   *)

  val reparse : ?charset:string -> ?buffer:string -> t -> unit
  (**
   * Reparse an analysis unit from a buffer, if provided, or from the original
   * file otherwise. If ``Charset`` is empty or ``None``, use the last charset
   * successfuly used for this unit, otherwise use it to decode the content of
   * the source file.
  
   * If any failure occurs, such as decoding, lexing or parsing failure,
   * diagnostic are emitted to explain what happened.
   *)

  val first_token : t -> Token.t
  (**
   * Return a reference to the first token scanned in this unit.
   *)

  val last_token : t -> Token.t
  (**
   * Return a reference to the last token scanned in this unit.
   *)

  val token_count : t -> int
  (**
   * Return the number of tokens in this unit.
   *)

  val trivia_count : t -> int
  (**
   * Return the number of trivias in this unit. This is 0 for units that were
   * parsed with trivia analysis disabled.
   *)

  
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> t -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> t-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> t -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : t -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)

end

module AnalysisContext : sig
  (**
   * This type represents a context for all source analysis. This is the first
   * type you need to create to use Libturkixirlang. It will contain the results
   * of all analysis, and is the main holder for all the data.
  
   * You can create several analysis contexts if you need to, which enables you,
   * for example to:
  
   * * analyze several different projects at the same time;
  
   * * analyze different parts of the same projects in parallel.
  
   * In the current design, contexts always keep all of their analysis units
   * allocated. If you need to get this memory released, the only option at your
   * disposal is to destroy your analysis context instance.
   *)

  type t = analysis_context

  val create :
    ?charset:string
    -> ?with_trivia:bool
    -> ?tab_stop:int
    -> ?unit_provider:UnitProvider.t
    -> unit
    -> t
  (**
   * Create a new analysis context.
  
   * ``Charset`` will be used as a default charset to decode input sources in
   * analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   * charsets. Be careful: passing an unsupported charset is not guaranteed to
   * raise an error here. If no charset is provided, ``"utf-8"`` is the default.
  
   * .. todo:: Passing an unsupported charset here is not guaranteed to raise an
   *    error right here, but this would be really helpful for users.
  
   * When ``With_Trivia`` is true, the parsed analysis units will contain
   * trivias.
  
   * If provided, ``File_Reader`` will be used to fetch the contents of source
   * files instead of the default, which is to just read it from the filesystem
   * and decode it using the regular charset rules. Note that if provided, all
   * parsing APIs that provide a buffer are forbidden, and any use of the
   * rewriting API with the returned context is rejected.
  
   * If provided, ``Unit_Provider`` will be used to query the file name that
   * corresponds to a unit reference during semantic analysis. If it is
   * ``None``, the default one is used instead.
  
   * ``Tab_Stop`` is a positive number to describe the effect of tabulation
   * characters on the column number in source files.
   *)

  val get_from_file :
    ?charset:string
    -> ?reparse:bool
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> AnalysisUnit.t
  (**
   * Create a new analysis unit for ``Filename`` or return the existing one if
   * any. If ``Reparse`` is true and the analysis unit already exists, reparse
   * it from ``Filename``.
  
   * ``Rule`` controls which grammar rule is used to parse the unit.
  
   * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   * use the context's default charset.
  
   * If any failure occurs, such as file opening, decoding, lexing or parsing
   * failure, return an analysis unit anyway: errors are described as
   * diagnostics of the returned analysis unit.
   *)

  val get_from_buffer :
    ?charset:string
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> string
    -> AnalysisUnit.t
  (**
   * Create a new analysis unit for ``Filename`` or return the existing one if
   * any. Whether the analysis unit already exists or not, (re)parse it from the
   * source code in ``Buffer``.
  
   * ``Rule`` controls which grammar rule is used to parse the unit.
  
   * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   * use the context's default charset.
  
   * If any failure occurs, such as file opening, decoding, lexing or parsing
   * failure, return an analysis unit anyway: errors are described as
   * diagnostics of the returned analysis unit.
   *)
end


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

module VarArgsFlagPresent : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module VarArgsFlagAbsent : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module VarArgsFlag : sig
  

  type t =
    [
      | VarArgsFlagAbsent.t
      | VarArgsFlagPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< var_args_flag ]
    -> bool
  (**
   * Return whether this is an instance of VarArgsFlagPresent
   *)



end

module TurkixirNodeList : sig
  (**
   * List of TurkixirNode.
  
   * This list node can contain one of the following nodes:
  
   * * as_name_node
  
   * * assert_stmt
  
   * * assign_stmt
  
   * * aug_assign_stmt
  
   * * break_stmt
  
   * * continue_stmt
  
   * * decorated
  
   * * def_stmt
  
   * * del_stmt
  
   * * exec_stmt
  
   * * expr_list
  
   * * for_stmt
  
   * * global_stmt
  
   * * if_stmt
  
   * * import_from
  
   * * import_name
  
   * * name
  
   * * pass_stmt
  
   * * print_stmt
  
   * * raise_stmt
  
   * * return_stmt
  
   * * stream_print_stmt
  
   * * try_stmt
  
   * * turkixir_node_list
  
   * * while_stmt
  
   * * with_stmt
  
   * * yield_expr
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< turkixir_node_list]
    -> turkixir_node list

end

module StringLitList : sig
  (**
   * List of StringLit.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< string_lit_list]
    -> string_lit list

end

module SingleParamList : sig
  (**
   * List of SingleParam.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< single_param_list]
    -> single_param list

end

module NLList : sig
  (**
   * List of NL.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< nl_list]
    -> nl list

end

module IdList : sig
  (**
   * List of Id.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< id_list]
    -> id list

end

module ExprList : sig
  (**
   * List of Expr.
  
   * This list node can contain one of the following nodes:
  
   * * and_expr
  
   * * and_op
  
   * * bin_op
  
   * * call_expr
  
   * * comp_op
  
   * * concat_string_lit
  
   * * dict_comp
  
   * * dict_lit
  
   * * ellipsis_expr
  
   * * factor
  
   * * if_expr
  
   * * inline_eval
  
   * * lambda_def
  
   * * list_comp
  
   * * list_gen
  
   * * list_lit
  
   * * name
  
   * * not_op
  
   * * number_lit
  
   * * or_expr
  
   * * or_op
  
   * * power
  
   * * set_comp
  
   * * set_lit
  
   * * slice_expr
  
   * * string_lit
  
   * * subscript_expr
  
   * * tuple_lit
  
   * * xor_expr
  
   * * yield_expr
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< expr_list]
    -> expr list

end

module ExceptPartList : sig
  (**
   * List of ExceptPart.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< except_part_list]
    -> except_part list

end

module ElifBranchList : sig
  (**
   * List of ElifBranch.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< elif_branch_list]
    -> elif_branch list

end

module DotList : sig
  (**
   * List of Dot.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< dot_list]
    -> dot list

end

module DictAssocList : sig
  (**
   * List of DictAssoc.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< dict_assoc_list]
    -> dict_assoc list

end

module DecoratorList : sig
  (**
   * List of Decorator.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< decorator_list]
    -> decorator list

end

module AsNameNodeList : sig
  (**
   * List of AsNameNode.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< as_name_node_list]
    -> as_name_node list

end

module ArgList : sig
  (**
   * List of Arg.
   *)

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




  val f_list :
    [< arg_list]
    -> arg list

end

module TurkixirNodeBaseList : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module WithStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_bindings :
    [< with_stmt]
    -> as_name_node_list

      
  val f_statements :
    [< with_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module WhileStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_test :
    [< while_stmt]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_statements :
    [< while_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]

      
  val f_else_part :
    [< while_stmt]
    -> else_part option


end

module TryStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_statements :
    [< try_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]

      
  val f_except_parts :
    [< try_stmt]
    -> except_part_list

      
  val f_else_part :
    [< try_stmt]
    -> else_part option

      
  val f_finally_part :
    [< try_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr] option


end

module StreamPrintStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_stream_expr :
    [< stream_print_stmt]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_exprs :
    [< stream_print_stmt]
    -> expr_list


end

module ReturnStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< return_stmt]
    -> expr_list option


end

module RaiseStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< raise_stmt]
    -> expr_list option


end

module PrintStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< print_stmt]
    -> expr_list


end

module PassStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ImportName : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_imported_names :
    [< import_name]
    -> turkixir_node_list


end

module ImportFrom : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_rel_name :
    [< import_from]
    -> [name | rel_name]

      
  val f_imported :
    [< import_from]
    -> [import_star | turkixir_node_list]


end

module IfStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_test :
    [< if_stmt]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_statements :
    [< if_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]

      
  val f_elif_branchs :
    [< if_stmt]
    -> elif_branch_list

      
  val f_else_part :
    [< if_stmt]
    -> else_part option


end

module GlobalStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_names :
    [< global_stmt]
    -> id_list


end

module ForStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_bindings :
    [< for_stmt]
    -> expr_list

      
  val f_expr :
    [< for_stmt]
    -> expr_list

      
  val f_statements :
    [< for_stmt]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]

      
  val f_else_part :
    [< for_stmt]
    -> else_part option


end

module ExecStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< exec_stmt]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_in_list :
    [< exec_stmt]
    -> expr_list option


end

module ElifBranch : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_cond_test :
    [< elif_branch]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_statements :
    [< elif_branch]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module DelStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< del_stmt]
    -> expr_list


end

module FuncDef : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< func_def]
    -> id

      
  val f_parameters :
    [< func_def]
    -> params option

      
  val f_body :
    [< func_def]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module ClassDef : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< class_def]
    -> id

      
  val f_bases :
    [< class_def]
    -> expr_list option

      
  val f_statements :
    [< class_def]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module DefStmt : sig
  

  type t =
    [
      | ClassDef.t
      | FuncDef.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Decorated : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_decorators :
    [< decorated]
    -> decorator_list

      
  val f_defn :
    [< decorated]
    -> def_stmt


end

module ContinueStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module BreakStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module AugAssignStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_l_value :
    [< aug_assign_stmt]
    -> expr_list

      
  val f_op :
    [< aug_assign_stmt]
    -> op

      
  val f_r_value :
    [< aug_assign_stmt]
    -> [expr_list | yield_expr]


end

module AssignStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_l_value :
    [< assign_stmt]
    -> expr_list

      
  val f_r_values :
    [< assign_stmt]
    -> turkixir_node_list


end

module AssertStmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_test_expr :
    [< assert_stmt]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_msg :
    [< assert_stmt]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option


end

module Stmt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module SingleParam : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_is_varargs :
    [< single_param]
    -> var_args_flag

      
  val f_is_kwargs :
    [< single_param]
    -> kw_args_flag

      
  val f_name :
    [< single_param]
    -> [id | id_list]

      
  val f_default_value :
    [< single_param]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option


end

module RelName : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_dots :
    [< rel_name]
    -> dot_list

      
  val f_name :
    [< rel_name]
    -> name option


end

module Params : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_single_params :
    [< params]
    -> single_param_list


end

module Op : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NL : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module KwArgsFlagPresent : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module KwArgsFlagAbsent : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module KwArgsFlag : sig
  

  type t =
    [
      | KwArgsFlagAbsent.t
      | KwArgsFlagPresent.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int


      
  val p_as_bool :
    [< kw_args_flag ]
    -> bool
  (**
   * Return whether this is an instance of KwArgsFlagPresent
   *)



end

module ImportStar : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module FileNode : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_statements :
    [< file_node]
    -> turkixir_node_list


end

module YieldExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< yield_expr]
    -> expr_list option


end

module XorExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< xor_expr]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_right :
    [< xor_expr]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]


end

module TupleLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< tuple_lit]
    -> expr_list option


end

module SubscriptExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< subscript_expr]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | inline_eval | list_comp | list_gen | list_lit | name | number_lit | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_suffix :
    [< subscript_expr]
    -> expr_list


end

module StringLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ExtSliceExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_first :
    [< ext_slice_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option

      
  val f_last :
    [< ext_slice_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option

      
  val f_stride :
    [< ext_slice_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option


end

module SliceExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_first :
    [< slice_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option

      
  val f_last :
    [< slice_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option


end

module SetLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< set_lit]
    -> expr_list


end

module SetComp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< set_comp]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comprehension :
    [< set_comp]
    -> comp_for


end

module Power : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< power]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | inline_eval | list_comp | list_gen | list_lit | name | number_lit | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_right :
    [< power]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]


end

module OrOp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< or_op]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_right :
    [< or_op]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module OrExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< or_expr]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_right :
    [< or_expr]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module NumberLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module NotOp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< not_op]
    -> [and_expr | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module Id : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module DottedName : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< dotted_name]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | inline_eval | list_comp | list_gen | list_lit | name | number_lit | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_suffix :
    [< dotted_name]
    -> id


end

module Name : sig
  

  type t =
    [
      | DottedName.t
      | Id.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ListLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< list_lit]
    -> expr_list


end

module ListGen : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< list_gen]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comprehension :
    [< list_gen]
    -> comp_forl


end

module ListComp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< list_comp]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comprehension :
    [< list_comp]
    -> comp_forl


end

module LambdaDef : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_args :
    [< lambda_def]
    -> params

      
  val f_expr :
    [< lambda_def]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module InlineEval : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< inline_eval]
    -> expr_list


end

module IfExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< if_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_cond :
    [< if_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_else_expr :
    [< if_expr]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module Factor : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_op :
    [< factor]
    -> op

      
  val f_expr :
    [< factor]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]


end

module EllipsisExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module Dot : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module DictLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_assocs :
    [< dict_lit]
    -> dict_assoc_list


end

module DictComp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_assoc :
    [< dict_comp]
    -> dict_assoc

      
  val f_comprehension :
    [< dict_comp]
    -> comp_for


end

module ConcatStringLit : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_first_str :
    [< concat_string_lit]
    -> string_lit

      
  val f_subsequent_str :
    [< concat_string_lit]
    -> string_lit_list


end

module CompOp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< comp_op]
    -> [and_expr | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_op :
    [< comp_op]
    -> comp_op_kind

      
  val f_right :
    [< comp_op]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module CallExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_prefix :
    [< call_expr]
    -> [call_expr | concat_string_lit | dict_comp | dict_lit | inline_eval | list_comp | list_gen | list_lit | name | number_lit | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_suffix :
    [< call_expr]
    -> arg_list


end

module Term : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< term]
    -> [bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_op :
    [< term]
    -> op

      
  val f_right :
    [< term]
    -> [arith_expr | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | term | tuple_lit | yield_expr]


end

module ShiftExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< shift_expr]
    -> [bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_op :
    [< shift_expr]
    -> op

      
  val f_right :
    [< shift_expr]
    -> [arith_expr | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | term | tuple_lit | yield_expr]


end

module ArithExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< arith_expr]
    -> [bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_op :
    [< arith_expr]
    -> op

      
  val f_right :
    [< arith_expr]
    -> [arith_expr | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | term | tuple_lit | yield_expr]


end

module BinOp : sig
  

  type t =
    [
      | ArithExpr.t
      | ShiftExpr.t
      | Term.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< bin_op]
    -> [bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_op :
    [< bin_op]
    -> op

      
  val f_right :
    [< bin_op]
    -> [arith_expr | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | term | tuple_lit | yield_expr]


end

module AndOp : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< and_op]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_right :
    [< and_op]
    -> [and_expr | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module AndExpr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_left :
    [< and_expr]
    -> [and_expr | bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]

      
  val f_right :
    [< and_expr]
    -> [bin_op | call_expr | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | number_lit | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | yield_expr]


end

module Expr : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module ExceptPart : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_as_name :
    [< except_part]
    -> as_name_node option

      
  val f_statements :
    [< except_part]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module ElsePart : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_statements :
    [< else_part]
    -> [assert_stmt | assign_stmt | aug_assign_stmt | break_stmt | continue_stmt | del_stmt | exec_stmt | expr_list | global_stmt | import_from | import_name | pass_stmt | print_stmt | raise_stmt | return_stmt | stream_print_stmt | turkixir_node_list | yield_expr]


end

module DictAssoc : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_key :
    [< dict_assoc]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_value :
    [< dict_assoc]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module Decorator : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_dec_name :
    [< decorator]
    -> name

      
  val f_arg_list :
    [< decorator]
    -> arg_list option


end

module CompForL : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< comp_forl]
    -> expr_list

      
  val f_target :
    [< comp_forl]
    -> expr_list

      
  val f_comp :
    [< comp_forl]
    -> [comp_forl | comp_if] option


end

module CompFor : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_exprs :
    [< comp_for]
    -> expr_list

      
  val f_target :
    [< comp_for]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | inline_eval | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comp :
    [< comp_for]
    -> [comp_for | comp_if] option


end

module Comprehension : sig
  

  type t =
    [
      | CompFor.t
      | CompForL.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindNotin : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindNoteq : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindLte : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindLt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindIsnot : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindIs : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindIn : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindGte : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindGt : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindEq : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKindDiamond : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompOpKind : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module CompIf : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_test :
    [< comp_if]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comp :
    [< comp_if]
    -> [comp_if | comprehension] option


end

module AsNameNode : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_imported :
    [< as_name_node]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_as_name :
    [< as_name_node]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option


end

module VarArgs : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< var_args]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module KwArgs : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< kw_args]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module ArgGen : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_expr :
    [< arg_gen]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]

      
  val f_comprehension :
    [< arg_gen]
    -> comp_for


end

module ArgAssoc : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int



      
  val f_name :
    [< arg_assoc]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr] option

      
  val f_expr :
    [< arg_assoc]
    -> [and_expr | and_op | bin_op | call_expr | comp_op | concat_string_lit | dict_comp | dict_lit | factor | if_expr | inline_eval | lambda_def | list_comp | list_gen | list_lit | name | not_op | number_lit | or_expr | or_op | power | set_comp | set_lit | string_lit | subscript_expr | tuple_lit | xor_expr | yield_expr]


end

module Arg : sig
  

  type t =
    [
      | ArgAssoc.t
      | ArgGen.t
      | KwArgs.t
      | VarArgs.t
    ]


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int




end

module TurkixirNode : sig
  

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


  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int

  val kind_name : [< turkixir_node] -> string
  (**
   * Return the kind of this node.
   *)

  val text : [< turkixir_node ] -> string
  (**
   * Return the source buffer slice corresponding to the text that spans
   * between the first and the last tokens of the given node.
   *)

  val image : [< turkixir_node ] -> string
  (**
   * Return a representation of this node as a string.
   *)

  val sloc_range : [< turkixir_node ] -> SlocRange.t
  (**
   * Return the spanning source location range for this node.
  
   * Note that this returns the sloc of the parent for synthetic nodes.
   *)

  val lookup : [< turkixir_node ] -> Sloc.t -> turkixir_node option
  (**
   * Return the bottom-most node from in ``Node`` and its children which
   * contains ``Sloc``, or ``None`` if there is none.
   *)

  
  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> [< turkixir_node] -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> [< turkixir_node]-> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> [< turkixir_node] -> 'a list
  (**
   * Map each token calling the given function
   *)

  val tokens : [< turkixir_node] -> Token.t list
  (**
   * Return a list of tokens for the given node
   *)


  val entity_image : [< turkixir_node ] -> string

  val children_opt : [< turkixir_node ] -> turkixir_node option list
  (**
   * Return an optional list of nodes which are the children of the given node.
   * Each child is optional because it can either be because of a syntax error,
   * or an optional field evaluated to null.
   *)

  val fold_fields :
    ('a -> turkixir_node -> 'a) -> 'a -> [< turkixir_node ] -> 'a
  (**
   * Fold all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val iter_fields :
    (turkixir_node -> unit) -> [< turkixir_node ] -> unit
  (**
   * Iter all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val exists_fields :
    (turkixir_node -> bool) -> [< turkixir_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for at least one node.
   *)

  val for_all_fields :
    (turkixir_node -> bool) -> [< turkixir_node ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for all nodes.
   *)

  val fold :
    ('a -> turkixir_node -> 'a) -> 'a -> [< turkixir_node ] -> 'a
  (**
   * Fold the entire AST, below the given node, and call the given function on
   * each node in prefix order.
   *)

  val iter :
    (turkixir_node -> unit) -> [< turkixir_node ] -> unit
  (**
   * Iterate over the entire AST, below the given node, and call the given
   * function on each node in prefix order.
   *)

  val filter :
    (turkixir_node -> bool)
    -> [< turkixir_node ]
    -> turkixir_node list
  (**
   * Fold the entire AST, below the given node, and return the list of node
   * evaluated to true by the given function
   *)

  val exists :
    (turkixir_node -> bool) -> [< turkixir_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true by at least one node.
   *)

  val for_all :
    (turkixir_node -> bool) -> [< turkixir_node ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true for all nodes.
   *)

  val lookup_with_kind :
    'a node -> [< turkixir_node] -> Sloc.t -> 'a option
  (**
   * Given the kind of a node, a source location and a node, return the deepest
   * node containing the source location and of the right kind. Returns None if
   * there is no match.
   *)

  val as_a : 'a node -> [< turkixir_node ] -> 'a option
  (**
   * Given the kind of a node, try to cast the given node to this kind. Return
   * None if the node is not of this type and thus, cannot be cast.
   *)

  val find : 'a node -> [< turkixir_node ] -> 'a
  (**
   * Given the kind of node, return the first node found by walking the given
   * node. The type of the resulting node depends on the desired kind
   *)


  val findall : 'a node -> [< turkixir_node ] -> 'a list
  (**
   * Given the kind of node, return the all nodes of this kind found by walking
   * the given node. The type of the resulting nodes depends on the desired
   * kind
   *)

  val fields_with_names :
    [< turkixir_node ] -> (string * turkixir_node option) list
  (**
   * Given a node, return the list of it's fields, together with the name of
   * the field. This function does not raise SyntaxError, but instead the
   * returned node is None.
   *)

  val pp_tree : Format.formatter -> [< turkixir_node] -> unit
  (**
   * Pretty print the whole tree by completely walking it.
   *)


      
  val parent :
    [< turkixir_node ]
    -> turkixir_node option
  (**
   * Return the syntactic parent for this node. Return null for the root node.
   *)

      
  val parents :
    ?with_self:
    bool
    -> [< turkixir_node ]
    -> turkixir_node list
  (**
   * Return an array that contains the lexical parents, this node included iff
   * ``with_self`` is True. Nearer parents are first in the list.
   *)

      
  val children :
    [< turkixir_node ]
    -> turkixir_node list
  (**
   * Return an array that contains the direct lexical children.
   *)

      
  val token_start :
    [< turkixir_node ]
    -> Token.t
  (**
   * Return the first token used to parse this node.
   *)

      
  val token_end :
    [< turkixir_node ]
    -> Token.t
  (**
   * Return the last token used to parse this node.
   *)

      
  val child_index :
    [< turkixir_node ]
    -> int
  (**
   * Return the 0-based index for Node in its parent's children.
   *)

      
  val previous_sibling :
    [< turkixir_node ]
    -> turkixir_node option
  (**
   * Return the node's previous sibling, or null if there is no such sibling.
   *)

      
  val next_sibling :
    [< turkixir_node ]
    -> turkixir_node option
  (**
   * Return the node's next sibling, or null if there is no such sibling.
   *)

      
  val unit :
    [< turkixir_node ]
    -> analysis_unit
  (**
   * Return the analysis unit owning this node.
   *)

      
  val is_ghost :
    [< turkixir_node ]
    -> bool
  (**
   * Return whether the node is a ghost.
  
   * Unlike regular nodes, ghost nodes cover no token in the input source: they
   * are logically located instead between two tokens. Both the ``token_start``
   * and the ``token_end`` of all ghost nodes is the token right after this
   * logical position.
   *)

      
  val full_sloc_image :
    [< turkixir_node ]
    -> string
  (**
   * Return a string containing the filename + the sloc in GNU conformant
   * format. Useful to create diagnostics from a node.
   *)



end


