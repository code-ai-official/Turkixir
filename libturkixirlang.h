








#ifndef LIBTURKIXIRLANG
#define LIBTURKIXIRLANG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* This type represents a context for all source analysis. This is the first
   type you need to create to use libturkixirlang. It will contain the results
   of all analysis, and is the main holder for all the data.

   You can create several analysis contexts if you need to, which enables you,
   for example to:

   * analyze several different projects at the same time;

   * analyze different parts of the same projects in parallel.

   In the current design, contexts always keep all of their analysis units
   allocated. If you need to get this memory released, the only option at your
   disposal is to destroy your analysis context instance.

   This structure is partially opaque: some fields are exposed to allow direct
   access, for performance concerns.  */
typedef struct
{
   uint64_t serial_number;
} *turkixir_analysis_context;

/* This type represents the analysis of a single file.

   This type has strong-reference semantics and is ref-counted. Furthermore, a
   reference to a unit contains an implicit reference to the context that owns
   it. This means that keeping a reference to a unit will keep the context and
   all the unit it contains allocated.

   This structure is partially opaque: some fields are exposed to allow direct
   access, for performance concerns.  */
typedef struct
{
   uint64_t version_number;
} *turkixir_analysis_unit;

/* Data type for all nodes. Nodes are assembled to make up a tree.  See the
   node primitives below to inspect such trees.

   Unlike for contexts and units, this type has weak-reference semantics:
   keeping a reference to a node has no effect on the decision to keep the unit
   that it owns allocated. This means that once all references to the context
   and units related to a node are dropped, the context and its units are
   deallocated and the node becomes a stale reference: most operations on it
   will raise a ``Stale_Reference_Error``.

   Note that since reparsing an analysis unit deallocates all the nodes it
   contains, this operation makes all reference to these nodes stale as
   well.   */
typedef void* turkixir_base_node;

/* Kind of AST nodes in parse trees.  */
typedef enum {
    

        /* turkixir_node (abstract)  */
        
    

        /* arg (abstract)  */
        
    

        
        turkixir_arg_assoc = 1,
    

        
        turkixir_arg_gen = 2,
    

        
        turkixir_kw_args = 3,
    

        
        turkixir_var_args = 4,
    

        
        turkixir_as_name_node = 5,
    

        
        turkixir_comp_if = 6,
    

        /* comp_op_kind (abstract)  */
        
    

        
        turkixir_comp_op_kind_diamond = 7,
    

        
        turkixir_comp_op_kind_eq = 8,
    

        
        turkixir_comp_op_kind_gt = 9,
    

        
        turkixir_comp_op_kind_gte = 10,
    

        
        turkixir_comp_op_kind_in = 11,
    

        
        turkixir_comp_op_kind_is = 12,
    

        
        turkixir_comp_op_kind_isnot = 13,
    

        
        turkixir_comp_op_kind_lt = 14,
    

        
        turkixir_comp_op_kind_lte = 15,
    

        
        turkixir_comp_op_kind_noteq = 16,
    

        
        turkixir_comp_op_kind_notin = 17,
    

        /* comprehension (abstract)  */
        
    

        
        turkixir_comp_for = 18,
    

        
        turkixir_comp_forl = 19,
    

        
        turkixir_decorator = 20,
    

        
        turkixir_dict_assoc = 21,
    

        
        turkixir_else_part = 22,
    

        
        turkixir_except_part = 23,
    

        /* expr (abstract)  */
        
    

        
        turkixir_and_expr = 24,
    

        
        turkixir_and_op = 25,
    

        /* bin_op (abstract)  */
        
    

        
        turkixir_arith_expr = 26,
    

        
        turkixir_shift_expr = 27,
    

        
        turkixir_term = 28,
    

        
        turkixir_call_expr = 29,
    

        
        turkixir_comp_op = 30,
    

        
        turkixir_concat_string_lit = 31,
    

        
        turkixir_dict_comp = 32,
    

        
        turkixir_dict_lit = 33,
    

        
        turkixir_dot = 34,
    

        
        turkixir_ellipsis_expr = 35,
    

        
        turkixir_factor = 36,
    

        
        turkixir_if_expr = 37,
    

        
        turkixir_inline_eval = 38,
    

        
        turkixir_lambda_def = 39,
    

        
        turkixir_list_comp = 40,
    

        
        turkixir_list_gen = 41,
    

        
        turkixir_list_lit = 42,
    

        /* name (abstract)  */
        
    

        
        turkixir_dotted_name = 43,
    

        
        turkixir_id = 44,
    

        
        turkixir_not_op = 45,
    

        
        turkixir_number_lit = 46,
    

        
        turkixir_or_expr = 47,
    

        
        turkixir_or_op = 48,
    

        
        turkixir_power = 49,
    

        
        turkixir_set_comp = 50,
    

        
        turkixir_set_lit = 51,
    

        
        turkixir_slice_expr = 52,
    

        
        turkixir_ext_slice_expr = 53,
    

        
        turkixir_string_lit = 54,
    

        
        turkixir_subscript_expr = 55,
    

        
        turkixir_tuple_lit = 56,
    

        
        turkixir_xor_expr = 57,
    

        
        turkixir_yield_expr = 58,
    

        
        turkixir_file_node = 59,
    

        
        turkixir_import_star = 60,
    

        /* kw_args_flag (abstract)  */
        
    

        
        turkixir_kw_args_flag_absent = 61,
    

        
        turkixir_kw_args_flag_present = 62,
    

        
        turkixir_nl = 63,
    

        
        turkixir_op = 64,
    

        
        turkixir_params = 65,
    

        
        turkixir_rel_name = 66,
    

        
        turkixir_single_param = 67,
    

        /* stmt (abstract)  */
        
    

        
        turkixir_assert_stmt = 68,
    

        
        turkixir_assign_stmt = 69,
    

        
        turkixir_aug_assign_stmt = 70,
    

        
        turkixir_break_stmt = 71,
    

        
        turkixir_continue_stmt = 72,
    

        
        turkixir_decorated = 73,
    

        /* def_stmt (abstract)  */
        
    

        
        turkixir_class_def = 74,
    

        
        turkixir_func_def = 75,
    

        
        turkixir_del_stmt = 76,
    

        
        turkixir_elif_branch = 77,
    

        
        turkixir_exec_stmt = 78,
    

        
        turkixir_for_stmt = 79,
    

        
        turkixir_global_stmt = 80,
    

        
        turkixir_if_stmt = 81,
    

        
        turkixir_import_from = 82,
    

        
        turkixir_import_name = 83,
    

        
        turkixir_pass_stmt = 84,
    

        
        turkixir_print_stmt = 85,
    

        
        turkixir_raise_stmt = 86,
    

        
        turkixir_return_stmt = 87,
    

        
        turkixir_stream_print_stmt = 88,
    

        
        turkixir_try_stmt = 89,
    

        
        turkixir_while_stmt = 90,
    

        
        turkixir_with_stmt = 91,
    

        /* turkixir_node_base_list (abstract)  */
        
    

        /* List of Arg.  */
        turkixir_arg_list = 92,
    

        /* List of AsNameNode.  */
        turkixir_as_name_node_list = 93,
    

        /* List of Decorator.  */
        turkixir_decorator_list = 94,
    

        /* List of DictAssoc.  */
        turkixir_dict_assoc_list = 95,
    

        /* List of Dot.  */
        turkixir_dot_list = 96,
    

        /* List of ElifBranch.  */
        turkixir_elif_branch_list = 97,
    

        /* List of ExceptPart.  */
        turkixir_except_part_list = 98,
    

        /* List of Expr.
        
           This list node can contain one of the following nodes:
        
           * turkixir_and_expr
        
           * turkixir_and_op
        
           * turkixir_bin_op
        
           * turkixir_call_expr
        
           * turkixir_comp_op
        
           * turkixir_concat_string_lit
        
           * turkixir_dict_comp
        
           * turkixir_dict_lit
        
           * turkixir_ellipsis_expr
        
           * turkixir_factor
        
           * turkixir_if_expr
        
           * turkixir_inline_eval
        
           * turkixir_lambda_def
        
           * turkixir_list_comp
        
           * turkixir_list_gen
        
           * turkixir_list_lit
        
           * turkixir_name
        
           * turkixir_not_op
        
           * turkixir_number_lit
        
           * turkixir_or_expr
        
           * turkixir_or_op
        
           * turkixir_power
        
           * turkixir_set_comp
        
           * turkixir_set_lit
        
           * turkixir_slice_expr
        
           * turkixir_string_lit
        
           * turkixir_subscript_expr
        
           * turkixir_tuple_lit
        
           * turkixir_xor_expr
        
           * turkixir_yield_expr  */
        turkixir_expr_list = 99,
    

        /* List of Id.  */
        turkixir_id_list = 100,
    

        /* List of NL.  */
        turkixir_nl_list = 101,
    

        /* List of SingleParam.  */
        turkixir_single_param_list = 102,
    

        /* List of StringLit.  */
        turkixir_string_lit_list = 103,
    

        /* List of TurkixirNode.
        
           This list node can contain one of the following nodes:
        
           * turkixir_as_name_node
        
           * turkixir_assert_stmt
        
           * turkixir_assign_stmt
        
           * turkixir_aug_assign_stmt
        
           * turkixir_break_stmt
        
           * turkixir_continue_stmt
        
           * turkixir_decorated
        
           * turkixir_def_stmt
        
           * turkixir_del_stmt
        
           * turkixir_exec_stmt
        
           * turkixir_expr_list
        
           * turkixir_for_stmt
        
           * turkixir_global_stmt
        
           * turkixir_if_stmt
        
           * turkixir_import_from
        
           * turkixir_import_name
        
           * turkixir_name
        
           * turkixir_pass_stmt
        
           * turkixir_print_stmt
        
           * turkixir_raise_stmt
        
           * turkixir_return_stmt
        
           * turkixir_stream_print_stmt
        
           * turkixir_try_stmt
        
           * turkixir_turkixir_node_list
        
           * turkixir_while_stmt
        
           * turkixir_with_stmt
        
           * turkixir_yield_expr  */
        turkixir_turkixir_node_list = 104,
    

        /* var_args_flag (abstract)  */
        
    

        
        turkixir_var_args_flag_absent = 105,
    

        
        turkixir_var_args_flag_present = 106,
} turkixir_node_kind_enum;

/* Reference to a symbol. Symbols are owned by analysis contexts, so they must
   not outlive them. This type exists only in the C API, and roughly wraps the
   corresponding Ada type (an array fat pointer).  */
typedef struct {
   void *data;
   void *bounds;
} turkixir_symbol_type;

/* Type to contain Unicode text data.  */
typedef struct {
   int length;
   int ref_count;
   uint32_t content[1];
} *turkixir_string_type;

/* Data type for env rebindings. For internal use only.  */
typedef void *turkixir_env_rebindings_type;

typedef uint8_t turkixir_bool;

/* Helper data structures for source location handling.  */

/* Location in a source file. Line and column numbers are one-based.  */
typedef struct {
    uint32_t line;
    uint16_t column;
} turkixir_source_location;

/* Location of a span of text in a source file.  */
typedef struct {
    turkixir_source_location start;
    turkixir_source_location end;
} turkixir_source_location_range;


/* String encoded in UTF-32 (native endianness).  */
typedef struct {
   /* Address for the content of the string.  */
    uint32_t *chars;
   /* Size of the string (in characters).  */
    size_t length;

    int is_allocated;
} turkixir_text;

/* Arbitrarily large integer.  */
typedef void *turkixir_big_integer;

/* Kind for this token.  */
typedef enum {
   
      
      TURKIXIR_TERMINATION = 0
      ,
      TURKIXIR_LEXING_FAILURE = 1
      ,
      TURKIXIR_T_T__RSH_ASSIGN = 2
      ,
      TURKIXIR_T_T__IS = 3
      ,
      TURKIXIR_T_T__EQUALS = 4
      ,
      TURKIXIR_T_T__DEF = 5
      ,
      TURKIXIR_T_T__LTE = 6
      ,
      TURKIXIR_T_T__RAISE = 7
      ,
      TURKIXIR_T_T__MOD = 8
      ,
      TURKIXIR_T_T__YIELD = 9
      ,
      TURKIXIR_T_T__XOR_ASSIGN = 10
      ,
      TURKIXIR_T_T__AS = 11
      ,
      TURKIXIR_T_T__LAMBDA = 12
      ,
      TURKIXIR_T_T__BACKTICK = 13
      ,
      TURKIXIR_T_T__TRY = 14
      ,
      TURKIXIR_T_T__DIVIDE = 15
      ,
      TURKIXIR_T_T__INVERT = 16
      ,
      TURKIXIR_T_T__RETURN = 17
      ,
      TURKIXIR_T_T__ASSERT = 18
      ,
      TURKIXIR_T_T__XOR = 19
      ,
      TURKIXIR_T_T__BREAK = 20
      ,
      TURKIXIR_T_T__RBRACK = 21
      ,
      TURKIXIR_T_T__POWER_ASSIGN = 22
      ,
      TURKIXIR_T_T__IMPORT = 23
      ,
      TURKIXIR_T_T__EXEC = 24
      ,
      TURKIXIR_T_T__COMMA = 25
      ,
      TURKIXIR_T_T_L_PAR = 26
      ,
      TURKIXIR_T_T__DOT = 27
      ,
      TURKIXIR_T_T__GTE = 28
      ,
      TURKIXIR_T_T__FLOORDIV_ASSIGN = 29
      ,
      TURKIXIR_T_T__MULTIPLY = 30
      ,
      TURKIXIR_T_T__DIV_ASSIGN = 31
      ,
      TURKIXIR_T_T__AT = 32
      ,
      TURKIXIR_T_T__ASSIGN = 33
      ,
      TURKIXIR_T_T__FLOORDIV = 34
      ,
      TURKIXIR_T_T__NOTEQUAL = 35
      ,
      TURKIXIR_T_T__MULT_ASSIGN = 36
      ,
      TURKIXIR_T_T__MOD_ASSIGN = 37
      ,
      TURKIXIR_T_T__GT = 38
      ,
      TURKIXIR_T_T__POWER = 39
      ,
      TURKIXIR_T_T__AMP = 40
      ,
      TURKIXIR_T_T__NOT = 41
      ,
      TURKIXIR_T_T__COLON = 42
      ,
      TURKIXIR_T_T__DIAMOND = 43
      ,
      TURKIXIR_T_T__IN = 44
      ,
      TURKIXIR_T_T_L_CURL = 45
      ,
      TURKIXIR_T_T__CLASS = 46
      ,
      TURKIXIR_T_T__OR_ASSIGN = 47
      ,
      TURKIXIR_T_T__ELIF = 48
      ,
      TURKIXIR_T_T__AND = 49
      ,
      TURKIXIR_T_T__SEMICOLON = 50
      ,
      TURKIXIR_T_T__ADD_ASIGN = 51
      ,
      TURKIXIR_T_T__PRINT = 52
      ,
      TURKIXIR_T_T__LSH = 53
      ,
      TURKIXIR_T_T__CONTINUE = 54
      ,
      TURKIXIR_T_T__WHILE = 55
      ,
      TURKIXIR_T_T__EXCEPT = 56
      ,
      TURKIXIR_T_T__IF = 57
      ,
      TURKIXIR_T_T__ELSE = 58
      ,
      TURKIXIR_T_T__DEL = 59
      ,
      TURKIXIR_T_T__MINUS_ASSIGN = 60
      ,
      TURKIXIR_T_T__OR = 61
      ,
      TURKIXIR_T_T__MINUS = 62
      ,
      TURKIXIR_T_T__LBRACK = 63
      ,
      TURKIXIR_T_T__AND_ASSIGN = 64
      ,
      TURKIXIR_T_T_R_PAR = 65
      ,
      TURKIXIR_T_T__GLOBAL = 66
      ,
      TURKIXIR_T_T__FOR = 67
      ,
      TURKIXIR_T_T__FROM = 68
      ,
      TURKIXIR_T_T__RSH = 69
      ,
      TURKIXIR_T_T__FINALLY = 70
      ,
      TURKIXIR_T_T__PASS = 71
      ,
      TURKIXIR_T_T__LSH_ASSIGN = 72
      ,
      TURKIXIR_T_T__BIN_OR = 73
      ,
      TURKIXIR_T_T__RCURL = 74
      ,
      TURKIXIR_T_T__WITH = 75
      ,
      TURKIXIR_T_T__PLUS = 76
      ,
      TURKIXIR_T_T__LT = 77
      ,
      TURKIXIR_T_T__NUMBER = 78
      ,
      TURKIXIR_T_T__STRING = 79
      ,
      TURKIXIR_T_T__COMMENT = 80
      ,
      TURKIXIR_T_T__ID = 81
      ,
      TURKIXIR_INDENT = 82
      ,
      TURKIXIR_DEDENT = 83
      ,
      TURKIXIR_NEWLINE = 84
} turkixir_token_kind;

typedef struct
{
   uint64_t version;
} *turkixir_token_data_handler;

/* Reference to a token in an analysis unit.  */
typedef struct {
    /* Private data associated to this token, including stale reference
       checking data, or NULL if this designates no token.  */
    turkixir_analysis_context context;
    turkixir_token_data_handler token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;

    turkixir_token_kind kind;
    turkixir_text text;
    turkixir_source_location_range sloc_range;
} turkixir_token;


/* Diagnostic for an analysis unit: cannot open the source file, parsing error,
   ...  */
typedef struct {
    turkixir_source_location_range sloc_range;
    turkixir_text message;
} turkixir_diagnostic;

   typedef enum {
      TURKIXIR_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, TURKIXIR_ANALYSIS_UNIT_KIND_UNIT_BODY
   } turkixir_analysis_unit_kind;
   /* Specify a kind of analysis unit. Specification units provide an interface
      to the outer world while body units provide an implementation for the
      corresponding interface.  */
   typedef enum {
      TURKIXIR_LOOKUP_KIND_RECURSIVE, TURKIXIR_LOOKUP_KIND_FLAT, TURKIXIR_LOOKUP_KIND_MINIMAL
   } turkixir_lookup_kind;
   
   typedef enum {
      TURKIXIR_DESIGNATED_ENV_KIND_NONE, TURKIXIR_DESIGNATED_ENV_KIND_CURRENT_ENV, TURKIXIR_DESIGNATED_ENV_KIND_NAMED_ENV, TURKIXIR_DESIGNATED_ENV_KIND_DIRECT_ENV
   } turkixir_designated_env_kind;
   /* Discriminant for DesignatedEnv structures.  */
   typedef enum {
      TURKIXIR_GRAMMAR_RULE_NAME_RULE, TURKIXIR_GRAMMAR_RULE_NUMBER_RULE, TURKIXIR_GRAMMAR_RULE_STRING_RULE, TURKIXIR_GRAMMAR_RULE_CAT_STRING_RULE, TURKIXIR_GRAMMAR_RULE_NL_RULE, TURKIXIR_GRAMMAR_RULE_MAIN_RULE_RULE, TURKIXIR_GRAMMAR_RULE_DECORATOR_RULE, TURKIXIR_GRAMMAR_RULE_DECORATORS_RULE, TURKIXIR_GRAMMAR_RULE_DECORATED_RULE, TURKIXIR_GRAMMAR_RULE_FUNC_DEF_RULE, TURKIXIR_GRAMMAR_RULE_PARAMETERS_RULE, TURKIXIR_GRAMMAR_RULE_VARARGSLIST_RULE, TURKIXIR_GRAMMAR_RULE_FPDEF_RULE, TURKIXIR_GRAMMAR_RULE_NAME_LIST_RULE, TURKIXIR_GRAMMAR_RULE_STMT_RULE, TURKIXIR_GRAMMAR_RULE_SIMPLE_STMT_RULE, TURKIXIR_GRAMMAR_RULE_SMALL_STMT_RULE, TURKIXIR_GRAMMAR_RULE_EXPR_STMT_RULE, TURKIXIR_GRAMMAR_RULE_PRINT_STMT_RULE, TURKIXIR_GRAMMAR_RULE_DEL_STMT_RULE, TURKIXIR_GRAMMAR_RULE_PASS_STMT_RULE, TURKIXIR_GRAMMAR_RULE_FLOW_STMT_RULE, TURKIXIR_GRAMMAR_RULE_BREAK_STMT_RULE, TURKIXIR_GRAMMAR_RULE_CONTINUE_STMT_RULE, TURKIXIR_GRAMMAR_RULE_RETURN_STMT_RULE, TURKIXIR_GRAMMAR_RULE_YIELD_STMT_RULE, TURKIXIR_GRAMMAR_RULE_RAISE_STMT_RULE, TURKIXIR_GRAMMAR_RULE_IMPORT_STMT_RULE, TURKIXIR_GRAMMAR_RULE_IMPORT_NAME_RULE, TURKIXIR_GRAMMAR_RULE_DOT_RULE, TURKIXIR_GRAMMAR_RULE_IMPORT_FROM_RULE, TURKIXIR_GRAMMAR_RULE_AS_NAME_RULE, TURKIXIR_GRAMMAR_RULE_DOTTED_AS_NAME_RULE, TURKIXIR_GRAMMAR_RULE_IMPORT_AS_NAMES_RULE, TURKIXIR_GRAMMAR_RULE_DOTTED_AS_NAMES_RULE, TURKIXIR_GRAMMAR_RULE_DOTTED_NAME_RULE, TURKIXIR_GRAMMAR_RULE_GLOBAL_STMT_RULE, TURKIXIR_GRAMMAR_RULE_EXEC_STMT_RULE, TURKIXIR_GRAMMAR_RULE_ASSERT_STMT_RULE, TURKIXIR_GRAMMAR_RULE_COMPOUND_STMT_RULE, TURKIXIR_GRAMMAR_RULE_ELSE_PART_RULE, TURKIXIR_GRAMMAR_RULE_IF_STMT_RULE, TURKIXIR_GRAMMAR_RULE_WHILE_STMT_RULE, TURKIXIR_GRAMMAR_RULE_FOR_STMT_RULE, TURKIXIR_GRAMMAR_RULE_TRY_STMT_RULE, TURKIXIR_GRAMMAR_RULE_WITH_STMT_RULE, TURKIXIR_GRAMMAR_RULE_WITH_ITEM_RULE, TURKIXIR_GRAMMAR_RULE_SUITE_RULE, TURKIXIR_GRAMMAR_RULE_TEST_RULE, TURKIXIR_GRAMMAR_RULE_OR_TEST_RULE, TURKIXIR_GRAMMAR_RULE_AND_TEST_RULE, TURKIXIR_GRAMMAR_RULE_NOT_TEST_RULE, TURKIXIR_GRAMMAR_RULE_COMPARISON_RULE, TURKIXIR_GRAMMAR_RULE_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_XOR_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_AND_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_SHIFT_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_ARITH_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_TERM_RULE, TURKIXIR_GRAMMAR_RULE_FACTOR_RULE, TURKIXIR_GRAMMAR_RULE_POWER_RULE, TURKIXIR_GRAMMAR_RULE_ATOM_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_DICT_ASSOC_RULE, TURKIXIR_GRAMMAR_RULE_YIELD_EXPR_RULE, TURKIXIR_GRAMMAR_RULE_ATOM_RULE, TURKIXIR_GRAMMAR_RULE_SET_LIT_RULE, TURKIXIR_GRAMMAR_RULE_LAMBDEF_RULE, TURKIXIR_GRAMMAR_RULE_SUBSCRIPT_LIST_RULE, TURKIXIR_GRAMMAR_RULE_SUBSCRIPT_RULE, TURKIXIR_GRAMMAR_RULE_EXPR_LIST_RULE, TURKIXIR_GRAMMAR_RULE_TEST_LIST_RULE, TURKIXIR_GRAMMAR_RULE_EMPTY_TEST_LIST_RULE, TURKIXIR_GRAMMAR_RULE_CLASS_DEF_RULE, TURKIXIR_GRAMMAR_RULE_ARG_LIST_RULE, TURKIXIR_GRAMMAR_RULE_LIST_ITER_RULE, TURKIXIR_GRAMMAR_RULE_LIST_FOR_RULE, TURKIXIR_GRAMMAR_RULE_LIST_IF_RULE, TURKIXIR_GRAMMAR_RULE_COMP_ITER_RULE, TURKIXIR_GRAMMAR_RULE_COMP_FOR_RULE, TURKIXIR_GRAMMAR_RULE_COMP_IF_RULE
   } turkixir_grammar_rule;
   /* Gramar rule to use for parsing.  */

const turkixir_grammar_rule turkixir_default_grammar_rule = TURKIXIR_GRAMMAR_RULE_MAIN_RULE_RULE;

/* Enumerated type describing all possible exceptions that need to be handled
   in the C bindings.  */
typedef enum {
      EXCEPTION_BAD_TYPE_ERROR,
      EXCEPTION_OUT_OF_BOUNDS_ERROR,
      EXCEPTION_INVALID_INPUT,
      EXCEPTION_INVALID_SYMBOL_ERROR,
      EXCEPTION_INVALID_UNIT_NAME_ERROR,
      EXCEPTION_NATIVE_EXCEPTION,
      EXCEPTION_PRECONDITION_FAILURE,
      EXCEPTION_PROPERTY_ERROR,
      EXCEPTION_TEMPLATE_ARGS_ERROR,
      EXCEPTION_TEMPLATE_FORMAT_ERROR,
      EXCEPTION_TEMPLATE_INSTANTIATION_ERROR,
      EXCEPTION_STALE_REFERENCE_ERROR,
      EXCEPTION_UNKNOWN_CHARSET,
} turkixir_exception_kind;

/* Holder for native exceptions-related information.  Memory management for
   this and all the fields is handled by the library: one just has to make sure
   not to keep references to it.

   .. todo:: For the moment, this structure contains already formatted
      information, but depending on possible future Ada runtime improvements,
      this might change.  */
typedef struct {
   /* The kind of this exception.  */
   turkixir_exception_kind kind;

   /* Message and context information associated with this exception.  */
   const char *information;
} turkixir_exception;

/*
 * Array types incomplete declarations
 */

        

typedef struct turkixir_turkixir_node_array_record *turkixir_turkixir_node_array;


/*
 * Iterator types incomplete declarations
 */

/* An iterator provides a mean to retrieve values one-at-a-time.

   Currently, each iterator is bound to the analysis context used to create it.
   Iterators are invalidated as soon as any unit of that analysis is reparsed.
   Due to the nature of iterators (lazy computations), this invalidation is
   necessary to avoid use of inconsistent state, such as an iterator trying to
   use analysis context data that is stale.  */



typedef void* turkixir_turkixir_node_iterator;



/*
 * Struct types declarations
 */

        



typedef struct {
} turkixir_internal_metadata;



        



typedef struct {
        turkixir_internal_metadata md;
        turkixir_env_rebindings_type rebindings;
        turkixir_bool from_rebound;
} turkixir_internal_entity_info;



        



typedef struct {
        turkixir_base_node node;
        turkixir_internal_entity_info info;
} turkixir_base_entity;




/*
 * Types for event handler
 */

/* Interface to handle events sent by the analysis context.  */
typedef void *turkixir_event_handler;

/* Callback type for functions that are called when a unit is requested.

   ``name`` is the name of the requested unit.

   ``from`` is the unit from which the unit was requested.

   ``found`` indicates whether the requested unit was found or not.

   ``is_not_found_error`` indicates whether the fact that the unit was not
   found is an error or not.

   .. warning:: The interface of this callback is probably subject to change,
      so should be treated as experimental.  */
typedef void (*turkixir_event_handler_unit_requested_callback)(
   void *data,
   turkixir_analysis_context context,
   turkixir_text *name,
   turkixir_analysis_unit from,
   turkixir_bool found,
   turkixir_bool is_not_found_error
);

/* Callback type for functions that are called when destroying an event
   handler.  */
typedef void (*turkixir_event_handler_destroy_callback)(void *data);

/* Callback type for functions that are called when a unit is parsed.

   ``unit`` is the resulting unit.

   ``reparsed`` indicates whether the unit was reparsed, or whether it was the
   first parse.  */
typedef void (*turkixir_event_handler_unit_parsed_callback)(
   void *data,
   turkixir_analysis_context context,
   turkixir_analysis_unit unit,
   turkixir_bool reparsed
);

/*
 * Types for file readers
 */

/* Interface to override how source files are fetched and decoded.  */
typedef void *turkixir_file_reader;

/* Callback type for functions that are called when destroying a file
   reader.   */
typedef void (*turkixir_file_reader_destroy_callback)(void *data);

/* Callback type for functions that are called to fetch the decoded source
   buffer for a requested filename.  */
typedef void (*turkixir_file_reader_read_callback)(
   void *data,
   const char *filename,
   const char *charset,
   int read_bom,
   turkixir_text *buffer,
   turkixir_diagnostic *diagnostic
);

/*
 * Types for unit providers
 */

/* Interface to fetch analysis units from a name and a unit kind.

   The unit provider mechanism provides an abstraction which assumes that to
   any couple (unit name, unit kind) we can associate at most one source file.
   This means that several couples can be associated to the same source file,
   but on the other hand, only one one source file can be associated to a
   couple.

   This is used to make the semantic analysis able to switch from one analysis
   units to another.

   See the documentation of each unit provider for the exact semantics of the
   unit name/kind information.  */
typedef void *turkixir_unit_provider;

/* Callback type for functions that are called when destroying a unit file
   provider type.  */
typedef void (*turkixir_unit_provider_destroy_callback)(void *data);

/* Callback type for functions that are called to turn a unit reference encoded
   as a unit name into an analysis unit.  */
typedef char *(*turkixir_unit_provider_get_unit_filename_callback)(
   void *data,
   turkixir_text *name,
   turkixir_analysis_unit_kind kind
);

/* Callback type for functions that are called to turn a unit reference encoded
   as a unit name into an analysis unit.  */
typedef turkixir_analysis_unit (*turkixir_unit_provider_get_unit_from_name_callback)(
   void *data,
   turkixir_analysis_context context,
   turkixir_text *name,
   turkixir_analysis_unit_kind kind,
   const char *charset,
   int reparse
);

/* All the functions below can potentially raise an exception, so
   turkixir_get_last_exception must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

        




struct turkixir_turkixir_node_array_record {
   int n;
   int ref_count;
   turkixir_base_entity items[1];
};

/* Create a length-sized array.  */
extern turkixir_turkixir_node_array
turkixir_turkixir_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
turkixir_turkixir_node_array_inc_ref(turkixir_turkixir_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
turkixir_turkixir_node_array_dec_ref(turkixir_turkixir_node_array a);



/*
 * Analysis primitives
 */

/* Create a new analysis context.

   ``Charset`` will be used as a default charset to decode input sources in
   analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   charsets. Be careful: passing an unsupported charset is not guaranteed to
   raise an error here. If no charset is provided, ``"utf-8"`` is the default.

   .. todo:: Passing an unsupported charset here is not guaranteed to raise an
      error right here, but this would be really helpful for users.

   When ``With_Trivia`` is true, the parsed analysis units will contain
   trivias.

   If provided, ``File_Reader`` will be used to fetch the contents of source
   files instead of the default, which is to just read it from the filesystem
   and decode it using the regular charset rules. Note that if provided, all
   parsing APIs that provide a buffer are forbidden, and any use of the
   rewriting API with the returned context is rejected.

   If provided, ``Unit_Provider`` will be used to query the file name that
   corresponds to a unit reference during semantic analysis. If it is ``NULL``,
   the default one is used instead.

   ``Tab_Stop`` is a positive number to describe the effect of tabulation
   characters on the column number in source files.  */
extern turkixir_analysis_context
turkixir_create_analysis_context(
   const char *charset,
   turkixir_file_reader file_reader,
   turkixir_unit_provider unit_provider,
   turkixir_event_handler event_handler,
   int with_trivia,
   int tab_stop
);

/* Increase the reference count to an analysis context. Return the reference
   for convenience.  */
extern turkixir_analysis_context
turkixir_context_incref(turkixir_analysis_context context);

/* Decrease the reference count to an analysis context. Destruction happens
   when the ref-count reaches 0.  */
extern void
turkixir_context_decref(turkixir_analysis_context context);

/* If the given string is a valid symbol, yield it as a symbol and return true.
   Otherwise, return false.  */
extern int
turkixir_context_symbol(turkixir_analysis_context context,
                                   turkixir_text *text,
                                   turkixir_symbol_type *symbol);

/* Debug helper. Set whether ``Property_Error`` exceptions raised in
   ``Populate_Lexical_Env`` should be discarded. They are by default.  */
extern void
turkixir_context_discard_errors_in_populate_lexical_env(
        turkixir_analysis_context context,
        int discard);

/* Create a new analysis unit for ``Filename`` or return the existing one if
   any. If ``Reparse`` is true and the analysis unit already exists, reparse it
   from ``Filename``.

   ``Rule`` controls which grammar rule is used to parse the unit.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as file opening, decoding, lexing or parsing
   failure, return an analysis unit anyway: errors are described as diagnostics
   of the returned analysis unit.  */
extern turkixir_analysis_unit
turkixir_get_analysis_unit_from_file(
        turkixir_analysis_context context,
        const char *filename,
        const char *charset,
        int reparse,
        turkixir_grammar_rule rule);

/* Create a new analysis unit for ``Filename`` or return the existing one if
   any. Whether the analysis unit already exists or not, (re)parse it from the
   source code in ``Buffer``.

   ``Rule`` controls which grammar rule is used to parse the unit.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as file opening, decoding, lexing or parsing
   failure, return an analysis unit anyway: errors are described as diagnostics
   of the returned analysis unit.  */
extern turkixir_analysis_unit
turkixir_get_analysis_unit_from_buffer(
        turkixir_analysis_context context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        turkixir_grammar_rule rule);


/* Return the root node for this unit, or ``NULL`` if there is none.  */
extern void
turkixir_unit_root(turkixir_analysis_unit unit,
                              turkixir_base_entity *result_p);

/* Return a reference to the first token scanned in this unit.  */
extern void
turkixir_unit_first_token(turkixir_analysis_unit unit,
                                     turkixir_token *token);

/* Return a reference to the last token scanned in this unit.  */
extern void
turkixir_unit_last_token(turkixir_analysis_unit unit,
                                    turkixir_token *token);

/* Return the number of tokens in this unit.  */
extern int
turkixir_unit_token_count(turkixir_analysis_unit unit);

/* Return the number of trivias in this unit. This is 0 for units that were
   parsed with trivia analysis disabled.  */
extern int
turkixir_unit_trivia_count(turkixir_analysis_unit unit);

/* Debug helper: output the lexical envs for the given analysis unit.  */
extern void
turkixir_unit_dump_lexical_env(turkixir_analysis_unit unit);

/* Return the filename this unit is associated to.

   The returned string is dynamically allocated and the caller must free it
   when done with it.  */
extern char *
turkixir_unit_filename(turkixir_analysis_unit unit);

/* Return the number of diagnostics associated to this unit.  */
extern unsigned
turkixir_unit_diagnostic_count(turkixir_analysis_unit unit);

/* Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P. Return
   zero on failure (when N is too big).  */
extern int
turkixir_unit_diagnostic(turkixir_analysis_unit unit,
                                    unsigned n,
                                    turkixir_diagnostic *diagnostic_p);

/* Return the context that owns this unit.  */
extern turkixir_analysis_context
turkixir_unit_context(turkixir_analysis_unit context);

/* Reparse an analysis unit from the associated file.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as decoding, lexing or parsing failure,
   diagnostic are emitted to explain what happened.  */
extern void
turkixir_unit_reparse_from_file(turkixir_analysis_unit unit,
                                           const char *charset);

/* Reparse an analysis unit from a buffer.

   Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
   use the context's default charset.

   If any failure occurs, such as decoding, lexing or parsing failure,
   diagnostic are emitted to explain what happened.  */
extern void
turkixir_unit_reparse_from_buffer (turkixir_analysis_unit unit,
                                              const char *charset,
                                              const char *buffer,
                                              size_t buffer_size);

/* Create lexical environments for this analysis unit, according to the
   specifications given in the language spec.

   If not done before, it will be automatically called during semantic
   analysis. Calling it before enables one to control where the latency occurs.

   Depending on whether errors are discarded (see
   ``Discard_Errors_In_Populate_Lexical_Env``), return 0 on failure and 1 on
   success.  */
extern int
turkixir_unit_populate_lexical_env(turkixir_analysis_unit unit);

/*
 * General AST node primitives
 */

/* Return whether this node is a null node reference.  */
static inline int
turkixir_node_is_null(turkixir_base_entity *node) {
    return node->node == NULL;
}

/* Return the kind of this node.  */
extern turkixir_node_kind_enum
turkixir_node_kind(turkixir_base_entity *node);

/* Helper for textual dump: return the kind name for this node. The returned
   string is a copy and thus must be free'd by the caller.  */
extern void
turkixir_kind_name(turkixir_node_kind_enum kind, turkixir_text *result);

/* Return the analysis unit that owns this node.  */
extern int
turkixir_node_unit(turkixir_base_entity *node,
                              turkixir_analysis_unit *unit_p);

/* Return whether this node is a node that contains only a single token.  */
extern int
turkixir_node_is_token_node(turkixir_base_entity *node);

/* Return whether this node is synthetic.  */
extern int
turkixir_node_is_synthetic(turkixir_base_entity *node);

/* Return a representation of this node as a string.  */
extern void
turkixir_node_image(turkixir_base_entity *node,
                               turkixir_text *result);

/* Return the source buffer slice corresponding to the text that spans between
   the first and the last tokens of this node.

   Note that this returns the empty string for synthetic nodes.  */
extern void
turkixir_node_text(turkixir_base_entity *node,
                              turkixir_text *text);

/* Return the spanning source location range for this node.

   Note that this returns the sloc of the parent for synthetic nodes.  */
extern void
turkixir_node_sloc_range(turkixir_base_entity *node,
                                    turkixir_source_location_range *sloc_range);

/* Return the bottom-most node from in ``Node`` and its children which contains
   ``Sloc``, or ``NULL`` if there is none.  */
extern void
turkixir_lookup_in_node(turkixir_base_entity *node,
                                   const turkixir_source_location *sloc,
                                   turkixir_base_entity *result_p);

/* Return the number of children in this node.  */
extern unsigned
turkixir_node_children_count(turkixir_base_entity *node);

/* Return the Nth child for in this node's fields and store it into *CHILD_P.
   Return zero on failure (when N is too big).  */
extern int
turkixir_node_child(turkixir_base_entity *node,
                               unsigned n,
                               turkixir_base_entity* child_p);

/* Encode some text using the current locale. The result is dynamically
   allocated: it is up to the caller to free it when done with it.

   This is a development helper to make it quick and easy to print token and
   diagnostic text: it ignores errors (when the locale does not support some
   characters). Production code should use real conversion routines such as
   libiconv's in order to deal with UTF-32 texts.  */
extern char *
turkixir_text_to_locale_string(turkixir_text *text);

/* Free dynamically allocated memory.

   This is a helper to free objects from dynamic languages.  */
extern void
turkixir_free(void *address);

/* If this text object owns the buffer it references, free this buffer.

   Note that even though this accepts a pointer to a text object, it does not
   deallocates the text object itself but rather the buffer it references.  */
extern void
turkixir_destroy_text(turkixir_text *text);

/* Return the text associated to this symbol.  */
extern void
turkixir_symbol_text(turkixir_symbol_type *symbol,
                                turkixir_text *text);

/* Create a big integer from its string representation (in base 10).  */
extern turkixir_big_integer
turkixir_create_big_integer(turkixir_text *text);

/* Return the string representation (in base 10) of this big integer.  */
extern void
turkixir_big_integer_text(turkixir_big_integer bigint,
                                     turkixir_text *text);

/* Decrease the reference count for this big integer.  */
extern void
turkixir_big_integer_decref(turkixir_big_integer bigint);

/* Allocate strings to represent the library version number and build date and
   put them in Version/Build_Date. Callers are expected to call free() on the
   returned string once done.  */
extern void
turkixir_get_versions(char **version, char **build_date);

/* Create a string value from its content (UTF32 with native endianity).

   Note that the CONTENT buffer argument is copied: the returned value does not
   contain a reference to it.  */
extern turkixir_string_type
turkixir_create_string(uint32_t *content, int length);

/* Decrease the reference count for this string.  */
extern void
turkixir_string_dec_ref(turkixir_string_type self);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

        



/* Return the syntactic parent for this node. Return null for the root
   node.   */
extern int turkixir_turkixir_node_parent(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* Return an array that contains the lexical parents, this node included iff
   ``with_self`` is True. Nearer parents are first in the list.  */
extern int turkixir_turkixir_node_parents(
    turkixir_base_entity *node,

        
        turkixir_bool
        with_self,

    turkixir_turkixir_node_array *value_p
);


        



/* Return an array that contains the direct lexical children.  */
extern int turkixir_turkixir_node_children(
    turkixir_base_entity *node,


    turkixir_turkixir_node_array *value_p
);


        



/* Return the first token used to parse this node.  */
extern int turkixir_turkixir_node_token_start(
    turkixir_base_entity *node,


    turkixir_token *value_p
);


        



/* Return the last token used to parse this node.  */
extern int turkixir_turkixir_node_token_end(
    turkixir_base_entity *node,


    turkixir_token *value_p
);


        



/* Return the 0-based index for Node in its parent's children.  */
extern int turkixir_turkixir_node_child_index(
    turkixir_base_entity *node,


    int *value_p
);


        



/* Return the node's previous sibling, or null if there is no such sibling.  */
extern int turkixir_turkixir_node_previous_sibling(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* Return the node's next sibling, or null if there is no such sibling.  */
extern int turkixir_turkixir_node_next_sibling(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* Return the analysis unit owning this node.  */
extern int turkixir_turkixir_node_unit(
    turkixir_base_entity *node,


    turkixir_analysis_unit *value_p
);


        



/* Return whether the node is a ghost.

   Unlike regular nodes, ghost nodes cover no token in the input source: they
   are logically located instead between two tokens. Both the ``token_start``
   and the ``token_end`` of all ghost nodes is the token right after this
   logical position.  */
extern int turkixir_turkixir_node_is_ghost(
    turkixir_base_entity *node,


    turkixir_bool *value_p
);


        



/* Return a string containing the filename + the sloc in GNU conformant format.
   Useful to create diagnostics from a node.  */
extern int turkixir_turkixir_node_full_sloc_image(
    turkixir_base_entity *node,


    turkixir_string_type *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_arg_assoc_f_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_arg_assoc_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_arg_gen_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_arg_gen_f_comprehension(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_kw_args_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_var_args_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_as_name_node_f_imported(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_as_name_node_f_as_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_if_f_test(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_comp_if

   * turkixir_comprehension  */
extern int turkixir_comp_if_f_comp(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_for_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_for_f_target(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_comp_for

   * turkixir_comp_if  */
extern int turkixir_comp_for_f_comp(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_forl_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_forl_f_target(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_comp_forl

   * turkixir_comp_if  */
extern int turkixir_comp_forl_f_comp(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_decorator_f_dec_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_decorator_f_arg_list(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_dict_assoc_f_key(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_dict_assoc_f_value(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_else_part_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_except_part_f_as_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_except_part_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_and_expr_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_and_expr_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_and_op_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_and_op_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_bin_op_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_bin_op_f_op(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_arith_expr

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_term

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_bin_op_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_call_expr_f_prefix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_call_expr_f_suffix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_op_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_comp_op_f_op(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_comp_op_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_concat_string_lit_f_first_str(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_concat_string_lit_f_subsequent_str(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_dict_comp_f_assoc(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_dict_comp_f_comprehension(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_dict_lit_f_assocs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_factor_f_op(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_factor_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_if_expr_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_if_expr_f_cond(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_if_expr_f_else_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_inline_eval_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_lambda_def_f_args(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_lambda_def_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_list_comp_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_list_comp_f_comprehension(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_list_gen_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_list_gen_f_comprehension(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_list_lit_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_dotted_name_f_prefix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_dotted_name_f_suffix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_not_op_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_or_expr_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_or_expr_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_or_op_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_or_op_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_power_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_power_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_set_comp_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_set_comp_f_comprehension(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_set_lit_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_slice_expr_f_first(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_slice_expr_f_last(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_ext_slice_expr_f_stride(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_subscript_expr_f_prefix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_ellipsis_expr

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_slice_expr

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_subscript_expr_f_suffix(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_tuple_lit_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_xor_expr_f_left(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_yield_expr  */
extern int turkixir_xor_expr_f_right(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_yield_expr_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_decorated

   * turkixir_def_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_for_stmt

   * turkixir_global_stmt

   * turkixir_if_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_try_stmt

   * turkixir_turkixir_node_list

   * turkixir_while_stmt

   * turkixir_with_stmt

   * turkixir_yield_expr  */
extern int turkixir_file_node_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* Return whether this is an instance of KwArgsFlagPresent  */
extern int turkixir_kw_args_flag_p_as_bool(
    turkixir_base_entity *node,


    turkixir_bool *value_p
);


        




extern int turkixir_params_f_single_params(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_rel_name_f_dots(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_rel_name_f_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_single_param_f_is_varargs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_single_param_f_is_kwargs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_id

   * turkixir_id_list  */
extern int turkixir_single_param_f_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_single_param_f_default_value(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_assert_stmt_f_test_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_assert_stmt_f_msg(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_assign_stmt_f_l_value(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_expr_list

   * turkixir_yield_expr  */
extern int turkixir_assign_stmt_f_r_values(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_aug_assign_stmt_f_l_value(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_aug_assign_stmt_f_op(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_expr_list

   * turkixir_yield_expr  */
extern int turkixir_aug_assign_stmt_f_r_value(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_decorated_f_decorators(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_decorated_f_defn(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_class_def_f_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_class_def_f_bases(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_class_def_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_func_def_f_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_func_def_f_parameters(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_func_def_f_body(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_del_stmt_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_elif_branch_f_cond_test(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_elif_branch_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_exec_stmt_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_exec_stmt_f_in_list(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_inline_eval

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_for_stmt_f_bindings(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_for_stmt_f_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_for_stmt_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_for_stmt_f_else_part(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_global_stmt_f_names(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_if_stmt_f_cond_test(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_if_stmt_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_if_stmt_f_elif_branchs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_if_stmt_f_else_part(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_name

   * turkixir_rel_name  */
extern int turkixir_import_from_f_rel_name(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_import_star

   * turkixir_turkixir_node_list  */
extern int turkixir_import_from_f_imported(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_as_name_node

   * turkixir_name  */
extern int turkixir_import_name_f_imported_names(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_print_stmt_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_raise_stmt_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_return_stmt_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_stream_print_stmt_f_stream_expr(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field contains a list that itself contains one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_stream_print_stmt_f_exprs(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_try_stmt_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_try_stmt_f_except_parts(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_try_stmt_f_else_part(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_try_stmt_f_finally_part(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_and_expr

   * turkixir_and_op

   * turkixir_bin_op

   * turkixir_call_expr

   * turkixir_comp_op

   * turkixir_concat_string_lit

   * turkixir_dict_comp

   * turkixir_dict_lit

   * turkixir_factor

   * turkixir_if_expr

   * turkixir_inline_eval

   * turkixir_lambda_def

   * turkixir_list_comp

   * turkixir_list_gen

   * turkixir_list_lit

   * turkixir_name

   * turkixir_not_op

   * turkixir_number_lit

   * turkixir_or_expr

   * turkixir_or_op

   * turkixir_power

   * turkixir_set_comp

   * turkixir_set_lit

   * turkixir_string_lit

   * turkixir_subscript_expr

   * turkixir_tuple_lit

   * turkixir_xor_expr

   * turkixir_yield_expr  */
extern int turkixir_while_stmt_f_cond_test(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_while_stmt_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_while_stmt_f_else_part(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        




extern int turkixir_with_stmt_f_bindings(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* This field can contain one of the following nodes:

   * turkixir_assert_stmt

   * turkixir_assign_stmt

   * turkixir_aug_assign_stmt

   * turkixir_break_stmt

   * turkixir_continue_stmt

   * turkixir_del_stmt

   * turkixir_exec_stmt

   * turkixir_expr_list

   * turkixir_global_stmt

   * turkixir_import_from

   * turkixir_import_name

   * turkixir_pass_stmt

   * turkixir_print_stmt

   * turkixir_raise_stmt

   * turkixir_return_stmt

   * turkixir_stream_print_stmt

   * turkixir_turkixir_node_list

   * turkixir_yield_expr  */
extern int turkixir_with_stmt_f_statements(
    turkixir_base_entity *node,


    turkixir_base_entity *value_p
);


        



/* Return whether this is an instance of VarArgsFlagPresent  */
extern int turkixir_var_args_flag_p_as_bool(
    turkixir_base_entity *node,


    turkixir_bool *value_p
);



/*
 * Event handlers
 */

/* Create an event handler. When done with it, the result must be passed to
   ``turkixir_dec_ref_event_handler``.

   Pass as ``data`` a pointer to hold your private data: it will be passed to
   all callbacks below.

   ``destroy`` is a callback that is called by
   ``turkixir_dec_ref_event_handler`` to leave a chance to free resources that
   ``data`` may hold.

   ``unit_requested`` is a callback that will be called when a unit is
   requested.

   .. warning:: Please note that the unit requested callback can be called
      *many* times for the same unit, so in all likeliness, those events should
      be filtered if they're used to forward diagnostics to the user.

   ``unit_parsed`` is a callback that will be called when a unit is parsed.  */
extern turkixir_event_handler
turkixir_create_event_handler(
   void *data,
   turkixir_event_handler_destroy_callback destroy_func,
   turkixir_event_handler_unit_requested_callback unit_requested_func,
   turkixir_event_handler_unit_parsed_callback unit_parsed_func
);

/* Release an ownership share for this event handler. This destroys the event
   handler if there are no shares left.  */
extern void
turkixir_dec_ref_event_handler(turkixir_event_handler self);

/*
 * File readers
 */

/* Create a file reader. When done with it, the result must be passed to
   ``turkixir_dec_ref_file_reader``.

   Pass as ``data`` a pointer to hold your private data: it will be passed to
   all callbacks below.

   ``destroy`` is a callback that is called by ``turkixir_dec_ref_file_reader``
   to leave a chance to free resources that ``data`` may hold.

   ``read`` is a callback. For a given filename/charset and whether to read the
   BOM (Byte Order Mark), it tries to fetch the contents of the source file,
   returned in ``Contents``. If there is an error, it must return it in
   ``Diagnostic`` instead.  */
extern turkixir_file_reader
turkixir_create_file_reader(
   void *data,
   turkixir_file_reader_destroy_callback destroy_func,
   turkixir_file_reader_read_callback read_func
);

/* Release an ownership share for this file reader. This destroys the file
   reader if there are no shares left.  */
extern void
turkixir_dec_ref_file_reader(turkixir_file_reader self);




/*
 * Unit providers
 */

/* Create a unit provider. When done with it, the result must be passed to
   ``turkixir_destroy_unit_provider``.

   Pass as ``data`` a pointer to hold your private data: it will be passed to
   all callbacks below.

   ``destroy`` is a callback that is called by
   ``turkixir_destroy_unit_provider`` to leave a chance to free resources that
   ``data`` may hold.

   ``get_unit_from_node`` is a callback. It turns an analysis unit reference
   represented as a node into an analysis unit. It should return ``NULL`` if
   the node is not a valid unit name representation.

   ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
   except it takes an analysis unit reference represented as a string.  */
extern turkixir_unit_provider
turkixir_create_unit_provider(
   void *data,
   turkixir_unit_provider_destroy_callback destroy_func,
   turkixir_unit_provider_get_unit_filename_callback get_unit_filename_func,
   turkixir_unit_provider_get_unit_from_name_callback get_unit_from_name_func
);

/* Release an ownership share for this unit provider. This destroys the unit
   provider if there are no shares left.  */
extern void
turkixir_dec_ref_unit_provider(void *data);




/*
 * Misc
 */

/* Return exception information for the last error that happened in the current
   thread. Will be automatically allocated on error and free'd on the next
   error.  */
extern const turkixir_exception *
turkixir_get_last_exception(void);

/* Return a human-readable name for a token kind.

   The returned string is dynamically allocated and the caller must free it
   when done with it.

   If the given kind is invalid, return ``NULL`` and set the last exception
   accordingly.  */
extern char *
turkixir_token_kind_name(turkixir_token_kind kind);

/* Return a reference to the next token in the corresponding analysis unit.  */
extern void
turkixir_token_next(turkixir_token *token,
                               turkixir_token *next_token);

/* Return a reference to the previous token in the corresponding analysis
   unit.   */
extern void
turkixir_token_previous(turkixir_token *token,
                                   turkixir_token *previous_token);

/* Compute the source buffer slice corresponding to the text that spans between
   the ``First`` and ``Last`` tokens (both included). This yields an empty
   slice if ``Last`` actually appears before ``First``. Put the result in
   ``RESULT``.

   This returns 0 if ``First`` and ``Last`` don't belong to the same analysis
   unit. Return 1 if successful.  */
extern int
turkixir_token_range_text(turkixir_token *first,
                                     turkixir_token *last,
                                     turkixir_text *result);

/* Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   means that their position in the stream won't be taken into account, only
   the kind and text of the token.  */
extern void
turkixir_token_is_equivalent(turkixir_token *left,
                                        turkixir_token *right);

/* Return a representation of this entity as a string.  */
extern void
turkixir_entity_image(turkixir_base_entity ent, turkixir_text *result);

#ifdef __cplusplus
}
#endif

#endif
