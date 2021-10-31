








with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with Libturkixirlang.Common;   use Libturkixirlang.Common;




--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

private package Libturkixirlang.Implementation.C is

   subtype turkixir_analysis_context is Internal_Context;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Libturkixirlang. It will contain
   --  the results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   subtype turkixir_analysis_unit is Internal_Unit;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   type turkixir_base_node is new System.Address;
   --  Data type for all nodes. Nodes are assembled to make up a tree.  See the
   --  node primitives below to inspect such trees.
   --
   --  Unlike for contexts and units, this type has weak-reference semantics:
   --  keeping a reference to a node has no effect on the decision to keep the
   --  unit that it owns allocated. This means that once all references to the
   --  context and units related to a node are dropped, the context and its
   --  units are deallocated and the node becomes a stale reference: most
   --  operations on it will raise a ``Stale_Reference_Error``.
   --
   --  Note that since reparsing an analysis unit deallocates all the nodes it
   --  contains, this operation makes all reference to these nodes stale as
   --  well.

   type turkixir_node_kind_enum is new int;
   --  Kind of AST nodes in parse trees.

   



subtype turkixir_base_entity is Internal_Entity;
type turkixir_base_entity_Ptr is access Internal_Entity;




   type turkixir_symbol_type is record
      Data, Bounds : System.Address;
   end record
      with Convention => C;
   --  Reference to a symbol. Symbols are owned by analysis contexts, so they
   --  must not outlive them. This type exists only in the C API, and roughly
   --  wraps the corresponding Ada type (an array fat pointer).

   subtype turkixir_string_type is String_Type;

   --  Helper data structures for source location handling

   type turkixir_source_location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
     with Convention => C;

   type turkixir_source_location_range is record
      Start_S, End_S : turkixir_source_location;
   end record
     with Convention => C;

   type turkixir_text is record
      Chars  : System.Address;
      --  Address for the content of the string.

      Length : size_t;
      --  Size of the string (in characters).

      Is_Allocated : int;
   end record
     with Convention => C;
   --  String encoded in UTF-32 (native endianness).

   type turkixir_big_integer is new System.Address;
   --  Arbitrarily large integer.

   type turkixir_token is record
      Context                   : turkixir_analysis_context;
      Token_Data                : Token_Data_Handler_Access;
      Token_Index, Trivia_Index : int;

      Kind       : int;
      Text       : turkixir_text;
      Sloc_Range : turkixir_source_location_range;
   end record
     with Convention => C;
   --  Reference to a token in an analysis unit.

   type turkixir_diagnostic is record
      Sloc_Range : turkixir_source_location_range;
      Message    : turkixir_text;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   --  Diagnostic for an analysis unit: cannot open the source file, parsing
   --  error, ...

   type turkixir_exception_kind is (
      Exception_Bad_Type_Error, Exception_Out_Of_Bounds_Error, Exception_Invalid_Input, Exception_Invalid_Symbol_Error, Exception_Invalid_Unit_Name_Error, Exception_Native_Exception, Exception_Precondition_Failure, Exception_Property_Error, Exception_Template_Args_Error, Exception_Template_Format_Error, Exception_Template_Instantiation_Error, Exception_Stale_Reference_Error, Exception_Unknown_Charset
   ) with Convention => C;
   --  Enumerated type describing all possible exceptions that need to be
   --  handled in the C bindings.

   type turkixir_exception is record
      Kind : turkixir_exception_kind;
      --  The kind of this exception.

      Information : chars_ptr;
      --  Message and context information associated with this exception.
   end record;
   --  Holder for native exceptions-related information.  Memory management for
   --  this and all the fields is handled by the library: one just has to make
   --  sure not to keep references to it.
   --
   --  .. todo:: For the moment, this structure contains already formatted
   --     information, but depending on possible future Ada runtime
   --     improvements, this might change.

   type turkixir_exception_Ptr is access turkixir_exception;

   type turkixir_bool is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

      subtype turkixir_analysis_unit_kind is Analysis_Unit_Kind;
      subtype turkixir_lookup_kind is Lookup_Kind;
      subtype turkixir_designated_env_kind is Designated_Env_Kind;
      subtype turkixir_grammar_rule is Grammar_Rule;

   procedure Free (Address : System.Address)
     with Export        => True,
          Convention    => C,
          External_Name => "turkixir_free";
   --  Free dynamically allocated memory.
   --
   --  This is a helper to free objects from dynamic languages.
   --  Helper to free objects in dynamic languages

   procedure turkixir_destroy_text (T : access turkixir_text)
     with Export        => True,
          Convention    => C,
          External_Name => "turkixir_destroy_text";
   --  If this text object owns the buffer it references, free this buffer.
   --
   --  Note that even though this accepts a pointer to a text object, it does
   --  not deallocates the text object itself but rather the buffer it
   --  references.

   procedure turkixir_symbol_text
     (Symbol : access turkixir_symbol_type; Text : access turkixir_text)
      with Export, Convention => C,
           External_Name => "turkixir_symbol_text";
   --  Return the text associated to this symbol.

   function turkixir_create_big_integer
     (Text : access turkixir_text) return turkixir_big_integer
      with Export, Convention => C,
           External_Name => "turkixir_create_big_integer";
   --  Create a big integer from its string representation (in base 10).

   procedure turkixir_big_integer_text
     (Bigint : turkixir_big_integer; Text : access turkixir_text)
      with Export, Convention => C,
           External_Name => "turkixir_big_integer_text";
   --  Return the string representation (in base 10) of this big integer.

   procedure turkixir_big_integer_decref
     (Bigint : turkixir_big_integer)
      with Export, Convention => C,
           External_Name => "turkixir_big_integer_decref";
   --  Decrease the reference count for this big integer.

   procedure turkixir_get_versions
     (Version, Build_Date : access chars_ptr)
      with Export, Convention => C,
           External_Name => "turkixir_get_versions";
   --  Allocate strings to represent the library version number and build date
   --  and put them in Version/Build_Date. Callers are expected to call free()
   --  on the returned string once done.

   function turkixir_create_string
     (Content : System.Address; Length : int) return turkixir_string_type
      with Export, Convention => C,
           External_Name => "turkixir_create_string";
   --  Create a string value from its content (UTF32 with native endianity).
   --
   --  Note that the CONTENT buffer argument is copied: the returned value does
   --  not contain a reference to it.

   procedure turkixir_string_dec_ref (Self : turkixir_string_type)
      with Export, Convention => C,
           External_Name => "turkixir_string_dec_ref";
   --  Decrease the reference count for this string.

   ------------------
   -- File readers --
   ------------------

   type turkixir_file_reader is new System.Address;
   --  Interface to override how source files are fetched and decoded.

   type turkixir_file_reader_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a file
   --  reader.

   type turkixir_file_reader_read_callback is access procedure
     (Data       : System.Address;
      Filename   : chars_ptr;
      Charset    : chars_ptr;
      Read_BOM   : int;
      Buffer     : access turkixir_text;
      Diagnostic : access turkixir_diagnostic)
      with Convention => C;
   --  Callback type for functions that are called to fetch the decoded source
   --  buffer for a requested filename.

   --------------------
   -- Event handlers --
   --------------------

   type turkixir_event_handler is new System.Address;
   --  Interface to handle events sent by the analysis context.

   type turkixir_event_handler_unit_requested_callback is access procedure
     (Data               : System.Address;
      Context            : turkixir_analysis_context;
      Name               : turkixir_text;
      From               : turkixir_analysis_unit;
      Found              : turkixir_bool;
      Is_Not_Found_Error : turkixir_bool)
      with Convention => C;
   --  Callback type for functions that are called when a unit is requested.
   --
   --  ``name`` is the name of the requested unit.
   --
   --  ``from`` is the unit from which the unit was requested.
   --
   --  ``found`` indicates whether the requested unit was found or not.
   --
   --  ``is_not_found_error`` indicates whether the fact that the unit was not
   --  found is an error or not.
   --
   --  .. warning:: The interface of this callback is probably subject to
   --     change, so should be treated as experimental.

   type turkixir_event_handler_unit_parsed_callback is access procedure
     (Data     : System.Address;
      Context  : turkixir_analysis_context;
      Unit     : turkixir_analysis_unit;
      Reparsed : turkixir_bool)
      with Convention => C;
   --  Callback type for functions that are called when a unit is parsed.
   --
   --  ``unit`` is the resulting unit.
   --
   --  ``reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   type turkixir_event_handler_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying an event
   --  handler.

   --------------------
   -- Unit providers --
   --------------------

   type turkixir_unit_provider is new System.Address;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.
   --
   --  See the documentation of each unit provider for the exact semantics of
   --  the unit name/kind information.

   type turkixir_unit_provider_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a unit file
   --  provider type.

   type turkixir_unit_provider_get_unit_filename_callback is access function
     (Data        : System.Address;
      Name        : turkixir_text;
      Kind        : turkixir_analysis_unit_kind) return chars_ptr
      with Convention => C;
   --  Callback type for functions that are called to turn a unit reference
   --  encoded as a unit name into an analysis unit.

   type turkixir_unit_provider_get_unit_from_name_callback is access function
     (Data        : System.Address;
      Context     : turkixir_analysis_context;
      Name        : turkixir_text;
      Kind        : turkixir_analysis_unit_kind;
      Charset     : chars_ptr;
      Reparse     : int) return turkixir_analysis_unit
      with Convention => C;
   --  Callback type for functions that are called to turn a unit reference
   --  encoded as a unit name into an analysis unit.

   -------------------------
   -- Analysis primitives --
   -------------------------

   function turkixir_create_analysis_context
     (Charset       : chars_ptr;
      File_Reader   : turkixir_file_reader;
      Unit_Provider : turkixir_unit_provider;
      Event_Handler : turkixir_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int) return turkixir_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_create_analysis_context";
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources in
   --  analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"utf-8"`` is the
   --  default.
   --
   --  .. todo:: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name that
   --  corresponds to a unit reference during semantic analysis. If it is
   --  ``NULL``, the default one is used instead.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.

   function turkixir_context_incref
     (Context : turkixir_analysis_context)
      return turkixir_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_context_incref";
   --  Increase the reference count to an analysis context. Return the
   --  reference for convenience.

   procedure turkixir_context_decref
     (Context : turkixir_analysis_context)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_context_decref";
   --  Decrease the reference count to an analysis context. Destruction happens
   --  when the ref-count reaches 0.

   function turkixir_context_symbol
     (Context : turkixir_analysis_context;
      Text    : access turkixir_text;
      Symbol  : access turkixir_symbol_type) return int
      with Export, Convention => C,
           External_name => "turkixir_context_symbol";
   --  If the given string is a valid symbol, yield it as a symbol and return
   --  true. Otherwise, return false.

   procedure turkixir_context_discard_errors_in_populate_lexical_env
     (Context : turkixir_analysis_context;
      Discard : int)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_context_discard_errors_in_populate_lexical_env";
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   function turkixir_get_analysis_unit_from_file
     (Context           : turkixir_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : turkixir_grammar_rule)
      return turkixir_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "turkixir_get_analysis_unit_from_file";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function turkixir_get_analysis_unit_from_buffer
     (Context           : turkixir_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : turkixir_grammar_rule)
      return turkixir_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "turkixir_get_analysis_unit_from_buffer";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.


   procedure turkixir_unit_root
     (Unit     : turkixir_analysis_unit;
      Result_P : turkixir_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_root";
   --  Return the root node for this unit, or ``NULL`` if there is none.

   procedure turkixir_unit_first_token
     (Unit  : turkixir_analysis_unit;
      Token : access turkixir_token)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_first_token";
   --  Return a reference to the first token scanned in this unit.

   procedure turkixir_unit_last_token
     (Unit  : turkixir_analysis_unit;
      Token : access turkixir_token)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_last_token";
   --  Return a reference to the last token scanned in this unit.

   function turkixir_unit_token_count
     (Unit : turkixir_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "turkixir_unit_token_count";
   --  Return the number of tokens in this unit.

   function turkixir_unit_trivia_count
     (Unit : turkixir_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "turkixir_unit_trivia_count";
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   procedure turkixir_unit_lookup_token
     (Unit   : turkixir_analysis_unit;
      Sloc   : access turkixir_source_location;
      Result : access turkixir_token)
      with Export        => True,
           Convention    => C,
           External_Name => "turkixir_unit_lookup_token";
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure turkixir_unit_dump_lexical_env
     (Unit : turkixir_analysis_unit)
      with Export        => True,
           Convention    => C,
           External_Name => "turkixir_unit_dump_lexical_env";

   function turkixir_unit_filename
     (Unit : turkixir_analysis_unit)
      return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_filename";
   --  Return the filename this unit is associated to.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.

   function turkixir_unit_diagnostic_count
     (Unit : turkixir_analysis_unit) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_diagnostic_count";
   --  Return the number of diagnostics associated to this unit.

   function turkixir_unit_diagnostic
     (Unit         : turkixir_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access turkixir_diagnostic) return int
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_diagnostic";
   --  Get the Nth diagnostic in this unit and store it into *DIAGNOSTIC_P.
   --  Return zero on failure (when N is too big).

   function turkixir_unit_context
     (Unit : turkixir_analysis_unit)
      return turkixir_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_context";
   --  Return the context that owns this unit.

   procedure turkixir_unit_reparse_from_file
     (Unit : turkixir_analysis_unit; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_reparse_from_file";
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure turkixir_unit_reparse_from_buffer
     (Unit        : turkixir_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_reparse_from_buffer";
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   function turkixir_unit_populate_lexical_env
     (Unit : turkixir_analysis_unit)
      return int
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_unit_populate_lexical_env";
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), return 0 on failure and 1
   --  on success.

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   function turkixir_node_kind
     (Node : turkixir_base_entity_Ptr) return turkixir_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_kind";
   --  Return the kind of this node.

   procedure turkixir_kind_name
     (Kind : turkixir_node_kind_enum; Result : access turkixir_text)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_kind_name";
   --  Helper for textual dump: return the kind name for this node. The
   --  returned string is a copy and thus must be free'd by the caller.

   function turkixir_node_unit
     (Node : turkixir_base_entity_Ptr) return turkixir_analysis_unit
      with Export => True,
           Convention => C,
           External_Name => "turkixir_node_unit";
   --  Return the analysis unit that owns this node.

   function turkixir_is_token_node
     (Node : turkixir_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_is_token_node";
   --  Return whether this node is a node that contains only a single token.

   function turkixir_is_synthetic
     (Node : turkixir_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_is_synthetic";
   --  Return whether this node is synthetic.

   procedure turkixir_node_image
     (Node : turkixir_base_entity_Ptr; Result : access turkixir_text)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_image";
   --  Return a representation of this node as a string.

   procedure turkixir_node_text
     (Node : turkixir_base_entity_Ptr;
      Text : access turkixir_text)
      with Export, Convention => C,
           External_Name      => "turkixir_node_text";
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   procedure turkixir_node_sloc_range
     (Node         : turkixir_base_entity_Ptr;
      Sloc_Range_P : access turkixir_source_location_range)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_sloc_range";
   --  Return the spanning source location range for this node.
   --
   --  Note that this returns the sloc of the parent for synthetic nodes.

   procedure turkixir_lookup_in_node
     (Node   : turkixir_base_entity_Ptr;
      Sloc   : turkixir_source_location;
      Result : turkixir_base_entity_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_lookup_in_node";
   --  Return the bottom-most node from in ``Node`` and its children which
   --  contains ``Sloc``, or ``NULL`` if there is none.

   function turkixir_node_children_count
     (Node : turkixir_base_entity_Ptr) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_children_count";
   --  Return the number of children in this node.

   function turkixir_node_child
     (Node    : turkixir_base_entity_Ptr;
      N       : unsigned;
      Child_P : turkixir_base_entity_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_node_child";
   --  Return the Nth child for in this node's fields and store it into
   --  *CHILD_P.  Return zero on failure (when N is too big).

   function turkixir_text_to_locale_string
     (Text : turkixir_text) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_text_to_locale_string";
   --  Encode some text using the current locale. The result is dynamically
   --  allocated: it is up to the caller to free it when done with it.
   --
   --  This is a development helper to make it quick and easy to print token
   --  and diagnostic text: it ignores errors (when the locale does not support
   --  some characters). Production code should use real conversion routines
   --  such as libiconv's in order to deal with UTF-32 texts.

   ------------------
   -- File readers --
   ------------------

   function turkixir_create_file_reader
     (Data         : System.Address;
      Destroy_Func : turkixir_file_reader_destroy_callback;
      Read_Func    : turkixir_file_reader_read_callback) return turkixir_file_reader
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_create_file_reader";
   --  Create a file reader. When done with it, the result must be passed to
   --  ``turkixir_dec_ref_file_reader``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``turkixir_dec_ref_file_reader`` to leave a chance to free resources
   --  that ``data`` may hold.
   --
   --  ``read`` is a callback. For a given filename/charset and whether to read
   --  the BOM (Byte Order Mark), it tries to fetch the contents of the source
   --  file, returned in ``Contents``. If there is an error, it must return it
   --  in ``Diagnostic`` instead.

   procedure turkixir_dec_ref_file_reader
     (File_Reader : turkixir_file_reader)
      with Export        => True,
           Convention    => C,
           External_name =>
              "turkixir_dec_ref_file_reader";
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.

   


   --------------------
   -- Event handlers --
   --------------------

   function turkixir_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : turkixir_event_handler_destroy_callback;
      Unit_Requested_Func : turkixir_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : turkixir_event_handler_unit_parsed_callback)
      return turkixir_event_handler
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_create_event_handler";
   --  Create an event handler. When done with it, the result must be passed to
   --  ``turkixir_dec_ref_event_handler``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``turkixir_dec_ref_event_handler`` to leave a chance to free resources
   --  that ``data`` may hold.
   --
   --  ``unit_requested`` is a callback that will be called when a unit is
   --  requested.
   --
   --  .. warning:: Please note that the unit requested callback can be called
   --     *many* times for the same unit, so in all likeliness, those events
   --     should be filtered if they're used to forward diagnostics to the
   --     user.
   --
   --  ``unit_parsed`` is a callback that will be called when a unit is parsed.

   procedure turkixir_dec_ref_event_handler
     (Handler : turkixir_event_handler)
      with Export        => True,
           Convention    => C,
           External_name =>
              "turkixir_dec_ref_event_handler";
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.

   


   --------------------
   -- Unit providers --
   --------------------

   function turkixir_create_unit_provider
     (Data                    : System.Address;
      Destroy_Func            : turkixir_unit_provider_destroy_callback;
      Get_Unit_Filename_Func  : turkixir_unit_provider_get_unit_filename_callback;
      Get_Unit_From_Name_Func : turkixir_unit_provider_get_unit_from_name_callback)
      return turkixir_unit_provider
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_create_unit_provider";
   --  Create a unit provider. When done with it, the result must be passed to
   --  ``turkixir_destroy_unit_provider``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``turkixir_destroy_unit_provider`` to leave a chance to free resources
   --  that ``data`` may hold.
   --
   --  ``get_unit_from_node`` is a callback. It turns an analysis unit
   --  reference represented as a node into an analysis unit. It should return
   --  ``NULL`` if the node is not a valid unit name representation.
   --
   --  ``get_unit_from_name`` is a callback similar to ``get_unit_from_node``
   --  except it takes an analysis unit reference represented as a string.

   procedure turkixir_dec_ref_unit_provider
     (Provider : turkixir_unit_provider)
      with Export        => True,
           Convention    => C,
           External_name =>
              "turkixir_dec_ref_unit_provider";
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.

   


   ------------------
   -- Struct types --
   ------------------


   -----------------
   -- Array types --
   -----------------

         



subtype turkixir_turkixir_node_array is Internal_Entity_Array_Access;
type turkixir_turkixir_node_array_Ptr is access Internal_Entity_Array_Access;

function turkixir_turkixir_node_array_create (Length : int) return Internal_Entity_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "turkixir_turkixir_node_array_create";

procedure turkixir_turkixir_node_array_inc_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "turkixir_turkixir_node_array_inc_ref";

procedure turkixir_turkixir_node_array_dec_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "turkixir_turkixir_node_array_dec_ref";



   --------------------
   -- Iterator types --
   --------------------


   ----------
   -- Misc --
   ----------

   function turkixir_get_last_exception return turkixir_exception_Ptr
     with Export        => True,
          Convention    => C,
          External_Name => "turkixir_get_last_exception";
   --  Return exception information for the last error that happened in the
   --  current thread. Will be automatically allocated on error and free'd on
   --  the next error.

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   function turkixir_token_kind_name (Kind : int) return chars_ptr
      with Export => True,
           Convention => C,
           External_Name => "turkixir_token_kind_name";
   --  Return a human-readable name for a token kind.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.
   --
   --  If the given kind is invalid, return ``NULL`` and set the last exception
   --  accordingly.

   procedure turkixir_token_next
     (Token      : turkixir_token;
      Next_Token : access turkixir_token)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_token_next";
   --  Return a reference to the next token in the corresponding analysis unit.

   procedure turkixir_token_previous
     (Token          : turkixir_token;
      Previous_Token : access turkixir_token)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_token_previous";
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function turkixir_token_range_text
     (First, Last : turkixir_token;
      Text        : access turkixir_text) return int
      with Export => True,
           Convention => C,
           External_Name => "turkixir_token_range_text";
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``. Put the
   --  result in ``RESULT``.
   --
   --  This returns 0 if ``First`` and ``Last`` don't belong to the same
   --  analysis unit. Return 1 if successful.

   function turkixir_token_is_equivalent
     (Left  : turkixir_token;
      Right : turkixir_token) return turkixir_bool
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_token_is_equivalent";
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   procedure turkixir_entity_image
     (Ent : turkixir_base_entity_Ptr; Result : access turkixir_text)
      with Export        => True,
           Convention    => C,
           External_name => "turkixir_entity_image";
   --  Return a representation of this entity as a string.

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

           
   

   
   

   function turkixir_turkixir_node_parent
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_parent";
   --  Return the syntactic parent for this node. Return null for the root
   --  node.

           
   

   
   

   function turkixir_turkixir_node_parents
     (Node : turkixir_base_entity_Ptr;

         With_Self :
            
            turkixir_bool;

      Value_P : access turkixir_turkixir_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_parents";
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

           
   

   
   

   function turkixir_turkixir_node_children
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_turkixir_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_children";
   --  Return an array that contains the direct lexical children.

           
   

   
   

   function turkixir_turkixir_node_token_start
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_token_start";
   --  Return the first token used to parse this node.

           
   

   
   

   function turkixir_turkixir_node_token_end
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_token_end";
   --  Return the last token used to parse this node.

           
   

   
   

   function turkixir_turkixir_node_child_index
     (Node : turkixir_base_entity_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_child_index";
   --  Return the 0-based index for Node in its parent's children.

           
   

   
   

   function turkixir_turkixir_node_previous_sibling
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_previous_sibling";
   --  Return the node's previous sibling, or null if there is no such sibling.

           
   

   
   

   function turkixir_turkixir_node_next_sibling
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_next_sibling";
   --  Return the node's next sibling, or null if there is no such sibling.

           
   

   
   

   function turkixir_turkixir_node_unit
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_unit";
   --  Return the analysis unit owning this node.

           
   

   
   

   function turkixir_turkixir_node_is_ghost
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_is_ghost";
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

           
   

   
   

   function turkixir_turkixir_node_full_sloc_image
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_turkixir_node_full_sloc_image";
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.

           
   

   
   

   function turkixir_arg_assoc_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_arg_assoc_f_name";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_arg_assoc_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_arg_assoc_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_arg_gen_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_arg_gen_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_arg_gen_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_arg_gen_f_comprehension";
   

           
   

   
   

   function turkixir_kw_args_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_kw_args_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_var_args_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_var_args_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_as_name_node_f_imported
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_as_name_node_f_imported";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_as_name_node_f_as_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_as_name_node_f_as_name";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_if_f_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_if_f_test";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_if_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_if_f_comp";
   --  This field can contain one of the following nodes:
   --
   --  * Comp_If
   --
   --  * Comprehension

           
   

   
   

   function turkixir_comp_for_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_for_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_for_f_target
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_for_f_target";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_for_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_for_f_comp";
   --  This field can contain one of the following nodes:
   --
   --  * Comp_For
   --
   --  * Comp_If

           
   

   
   

   function turkixir_comp_forl_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_forl_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_forl_f_target
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_forl_f_target";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_forl_f_comp
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_forl_f_comp";
   --  This field can contain one of the following nodes:
   --
   --  * Comp_ForL
   --
   --  * Comp_If

           
   

   
   

   function turkixir_decorator_f_dec_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_decorator_f_dec_name";
   

           
   

   
   

   function turkixir_decorator_f_arg_list
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_decorator_f_arg_list";
   

           
   

   
   

   function turkixir_dict_assoc_f_key
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dict_assoc_f_key";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_dict_assoc_f_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dict_assoc_f_value";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_else_part_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_else_part_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_except_part_f_as_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_except_part_f_as_name";
   

           
   

   
   

   function turkixir_except_part_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_except_part_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_and_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_and_expr_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_and_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_and_expr_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_and_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_and_op_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_and_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_and_op_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_bin_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_bin_op_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_bin_op_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_bin_op_f_op";
   

           
   

   
   

   function turkixir_bin_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_bin_op_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * Arith_Expr
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Term
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_call_expr_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_call_expr_f_prefix";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_call_expr_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_call_expr_f_suffix";
   

           
   

   
   

   function turkixir_comp_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_op_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_comp_op_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_op_f_op";
   

           
   

   
   

   function turkixir_comp_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_comp_op_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_concat_string_lit_f_first_str
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_concat_string_lit_f_first_str";
   

           
   

   
   

   function turkixir_concat_string_lit_f_subsequent_str
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_concat_string_lit_f_subsequent_str";
   

           
   

   
   

   function turkixir_dict_comp_f_assoc
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dict_comp_f_assoc";
   

           
   

   
   

   function turkixir_dict_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dict_comp_f_comprehension";
   

           
   

   
   

   function turkixir_dict_lit_f_assocs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dict_lit_f_assocs";
   

           
   

   
   

   function turkixir_factor_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_factor_f_op";
   

           
   

   
   

   function turkixir_factor_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_factor_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_if_expr_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_expr_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_if_expr_f_cond
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_expr_f_cond";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_if_expr_f_else_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_expr_f_else_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_inline_eval_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_inline_eval_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_lambda_def_f_args
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_lambda_def_f_args";
   

           
   

   
   

   function turkixir_lambda_def_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_lambda_def_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_list_comp_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_list_comp_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_list_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_list_comp_f_comprehension";
   

           
   

   
   

   function turkixir_list_gen_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_list_gen_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_list_gen_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_list_gen_f_comprehension";
   

           
   

   
   

   function turkixir_list_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_list_lit_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_dotted_name_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dotted_name_f_prefix";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_dotted_name_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_dotted_name_f_suffix";
   

           
   

   
   

   function turkixir_not_op_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_not_op_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_or_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_or_expr_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_or_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_or_expr_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_or_op_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_or_op_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_or_op_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_or_op_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_power_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_power_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_power_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_power_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_set_comp_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_set_comp_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_set_comp_f_comprehension
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_set_comp_f_comprehension";
   

           
   

   
   

   function turkixir_set_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_set_lit_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_slice_expr_f_first
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_slice_expr_f_first";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_slice_expr_f_last
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_slice_expr_f_last";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_ext_slice_expr_f_stride
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_ext_slice_expr_f_stride";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_subscript_expr_f_prefix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_subscript_expr_f_prefix";
   --  This field can contain one of the following nodes:
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_subscript_expr_f_suffix
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_subscript_expr_f_suffix";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Ellipsis_Expr
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * Slice_Expr
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_tuple_lit_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_tuple_lit_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_xor_expr_f_left
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_xor_expr_f_left";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_xor_expr_f_right
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_xor_expr_f_right";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_yield_expr_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_yield_expr_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_file_node_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_file_node_f_statements";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Decorated
   --
   --  * Def_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * For_Stmt
   --
   --  * Global_Stmt
   --
   --  * If_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Try_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * While_Stmt
   --
   --  * With_Stmt
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_kw_args_flag_p_as_bool
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_kw_args_flag_p_as_bool";
   --  Return whether this is an instance of KwArgsFlagPresent

           
   

   
   

   function turkixir_params_f_single_params
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_params_f_single_params";
   

           
   

   
   

   function turkixir_rel_name_f_dots
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_rel_name_f_dots";
   

           
   

   
   

   function turkixir_rel_name_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_rel_name_f_name";
   

           
   

   
   

   function turkixir_single_param_f_is_varargs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_single_param_f_is_varargs";
   

           
   

   
   

   function turkixir_single_param_f_is_kwargs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_single_param_f_is_kwargs";
   

           
   

   
   

   function turkixir_single_param_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_single_param_f_name";
   --  This field can contain one of the following nodes:
   --
   --  * Id
   --
   --  * Id_List

           
   

   
   

   function turkixir_single_param_f_default_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_single_param_f_default_value";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_assert_stmt_f_test_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_assert_stmt_f_test_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_assert_stmt_f_msg
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_assert_stmt_f_msg";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_assign_stmt_f_l_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_assign_stmt_f_l_value";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_assign_stmt_f_r_values
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_assign_stmt_f_r_values";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * Expr_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_aug_assign_stmt_f_l_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_aug_assign_stmt_f_l_value";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_aug_assign_stmt_f_op
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_aug_assign_stmt_f_op";
   

           
   

   
   

   function turkixir_aug_assign_stmt_f_r_value
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_aug_assign_stmt_f_r_value";
   --  This field can contain one of the following nodes:
   --
   --  * Expr_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_decorated_f_decorators
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_decorated_f_decorators";
   

           
   

   
   

   function turkixir_decorated_f_defn
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_decorated_f_defn";
   

           
   

   
   

   function turkixir_class_def_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_class_def_f_name";
   

           
   

   
   

   function turkixir_class_def_f_bases
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_class_def_f_bases";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_class_def_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_class_def_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_func_def_f_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_func_def_f_name";
   

           
   

   
   

   function turkixir_func_def_f_parameters
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_func_def_f_parameters";
   

           
   

   
   

   function turkixir_func_def_f_body
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_func_def_f_body";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_del_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_del_stmt_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_elif_branch_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_elif_branch_f_cond_test";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_elif_branch_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_elif_branch_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_exec_stmt_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_exec_stmt_f_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_exec_stmt_f_in_list
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_exec_stmt_f_in_list";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_for_stmt_f_bindings
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_for_stmt_f_bindings";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * Inline_Eval
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_for_stmt_f_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_for_stmt_f_expr";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_for_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_for_stmt_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_for_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_for_stmt_f_else_part";
   

           
   

   
   

   function turkixir_global_stmt_f_names
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_global_stmt_f_names";
   

           
   

   
   

   function turkixir_if_stmt_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_stmt_f_cond_test";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_if_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_stmt_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_if_stmt_f_elif_branchs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_stmt_f_elif_branchs";
   

           
   

   
   

   function turkixir_if_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_if_stmt_f_else_part";
   

           
   

   
   

   function turkixir_import_from_f_rel_name
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_import_from_f_rel_name";
   --  This field can contain one of the following nodes:
   --
   --  * Name
   --
   --  * Rel_Name

           
   

   
   

   function turkixir_import_from_f_imported
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_import_from_f_imported";
   --  This field can contain one of the following nodes:
   --
   --  * Import_Star
   --
   --  * Turkixir_Node_List

           
   

   
   

   function turkixir_import_name_f_imported_names
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_import_name_f_imported_names";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * As_Name_Node
   --
   --  * Name

           
   

   
   

   function turkixir_print_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_print_stmt_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_raise_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_raise_stmt_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_return_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_return_stmt_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_stream_print_stmt_f_stream_expr
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_stream_print_stmt_f_stream_expr";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_stream_print_stmt_f_exprs
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_stream_print_stmt_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_try_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_try_stmt_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_try_stmt_f_except_parts
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_try_stmt_f_except_parts";
   

           
   

   
   

   function turkixir_try_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_try_stmt_f_else_part";
   

           
   

   
   

   function turkixir_try_stmt_f_finally_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_try_stmt_f_finally_part";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_while_stmt_f_cond_test
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_while_stmt_f_cond_test";
   --  This field can contain one of the following nodes:
   --
   --  * And_Expr
   --
   --  * And_Op
   --
   --  * Bin_Op
   --
   --  * Call_Expr
   --
   --  * Comp_Op
   --
   --  * Concat_String_Lit
   --
   --  * Dict_Comp
   --
   --  * Dict_Lit
   --
   --  * Factor
   --
   --  * If_Expr
   --
   --  * Inline_Eval
   --
   --  * Lambda_Def
   --
   --  * List_Comp
   --
   --  * List_Gen
   --
   --  * List_Lit
   --
   --  * Name
   --
   --  * Not_Op
   --
   --  * Number_Lit
   --
   --  * Or_Expr
   --
   --  * Or_Op
   --
   --  * Power
   --
   --  * Set_Comp
   --
   --  * Set_Lit
   --
   --  * String_Lit
   --
   --  * Subscript_Expr
   --
   --  * Tuple_Lit
   --
   --  * Xor_Expr
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_while_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_while_stmt_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_while_stmt_f_else_part
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_while_stmt_f_else_part";
   

           
   

   
   

   function turkixir_with_stmt_f_bindings
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_with_stmt_f_bindings";
   

           
   

   
   

   function turkixir_with_stmt_f_statements
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_base_entity) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_with_stmt_f_statements";
   --  This field can contain one of the following nodes:
   --
   --  * Assert_Stmt
   --
   --  * Assign_Stmt
   --
   --  * Aug_Assign_Stmt
   --
   --  * Break_Stmt
   --
   --  * Continue_Stmt
   --
   --  * Del_Stmt
   --
   --  * Exec_Stmt
   --
   --  * Expr_List
   --
   --  * Global_Stmt
   --
   --  * Import_From
   --
   --  * Import_Name
   --
   --  * Pass_Stmt
   --
   --  * Print_Stmt
   --
   --  * Raise_Stmt
   --
   --  * Return_Stmt
   --
   --  * Stream_Print_Stmt
   --
   --  * Turkixir_Node_List
   --
   --  * Yield_Expr

           
   

   
   

   function turkixir_var_args_flag_p_as_bool
     (Node : turkixir_base_entity_Ptr;


      Value_P : access turkixir_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "turkixir_var_args_flag_p_as_bool";
   --  Return whether this is an instance of VarArgsFlagPresent


   ------------------------
   -- Conversion helpers --
   ------------------------

   --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return turkixir_source_location is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : turkixir_source_location) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap (S : Source_Location_Range) return turkixir_source_location_range is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line),   Unsigned_16 (S.End_Column))));
   function Unwrap (S : turkixir_source_location_range) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line),
       Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column),
       Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return turkixir_text;

   function Wrap_Alloc (S : Text_Type) return turkixir_text;
   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return turkixir_text;

   function Wrap (T : Text_Cst_Access) return turkixir_text is
     (if T = null
      then (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars => T.all'Address, Length => T.all'Length, Is_Allocated => 0));
   function Wrap (T : Text_Access) return turkixir_text is
     (Wrap (Text_Cst_Access (T)));

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types.  All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values.  Thus, strict
   --  aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, turkixir_big_integer);
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (turkixir_big_integer, Big_Integer_Type);

   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, turkixir_symbol_type);
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (turkixir_symbol_type, Symbol_Type);

   function Wrap is new Ada.Unchecked_Conversion
     (Bare_Turkixir_Node, turkixir_base_node);
   function Unwrap is new Ada.Unchecked_Conversion
     (turkixir_base_node, Bare_Turkixir_Node);

   function Wrap (Token : Token_Reference) return turkixir_token;
   function Unwrap (Token : turkixir_token) return Token_Reference;

   function Wrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (Internal_File_Reader_Access, turkixir_file_reader);
   function Unwrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (turkixir_file_reader, Internal_File_Reader_Access);

   function Wrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (Internal_Event_Handler_Access, turkixir_event_handler);
   function Unwrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (turkixir_event_handler, Internal_Event_Handler_Access);

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, turkixir_unit_provider);
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (turkixir_unit_provider, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);



   pragma Warnings (On, "possible aliasing problem for type");

end Libturkixirlang.Implementation.C;
