


package body Libturkixirlang.Lexer_State_Machine is

   Is_Trivia : constant array (Token_Kind) of Boolean := (
      Turkixir_Termination => False, Turkixir_Lexing_Failure => True, Turkixir_T_T__Rsh_Assign => False, Turkixir_T_T__Is => False, Turkixir_T_T__Equals => False, Turkixir_T_T__Def => False, Turkixir_T_T__Lte => False, Turkixir_T_T__Raise => False, Turkixir_T_T__Mod => False, Turkixir_T_T__Yield => False, Turkixir_T_T__Xor_Assign => False, Turkixir_T_T__As => False, Turkixir_T_T__Lambda => False, Turkixir_T_T__Backtick => False, Turkixir_T_T__Try => False, Turkixir_T_T__Divide => False, Turkixir_T_T__Invert => False, Turkixir_T_T__Return => False, Turkixir_T_T__Assert => False, Turkixir_T_T__Xor => False, Turkixir_T_T__Break => False, Turkixir_T_T__Rbrack => False, Turkixir_T_T__Power_Assign => False, Turkixir_T_T__Import => False, Turkixir_T_T__Exec => False, Turkixir_T_T__Comma => False, Turkixir_T_T_L_Par => False, Turkixir_T_T__Dot => False, Turkixir_T_T__Gte => False, Turkixir_T_T__Floordiv_Assign => False, Turkixir_T_T__Multiply => False, Turkixir_T_T__Div_Assign => False, Turkixir_T_T__At => False, Turkixir_T_T__Assign => False, Turkixir_T_T__Floordiv => False, Turkixir_T_T__Notequal => False, Turkixir_T_T__Mult_Assign => False, Turkixir_T_T__Mod_Assign => False, Turkixir_T_T__Gt => False, Turkixir_T_T__Power => False, Turkixir_T_T__Amp => False, Turkixir_T_T__Not => False, Turkixir_T_T__Colon => False, Turkixir_T_T__Diamond => False, Turkixir_T_T__In => False, Turkixir_T_T_L_Curl => False, Turkixir_T_T__Class => False, Turkixir_T_T__Or_Assign => False, Turkixir_T_T__Elif => False, Turkixir_T_T__And => False, Turkixir_T_T__Semicolon => False, Turkixir_T_T__Add_Asign => False, Turkixir_T_T__Print => False, Turkixir_T_T__Lsh => False, Turkixir_T_T__Continue => False, Turkixir_T_T__While => False, Turkixir_T_T__Except => False, Turkixir_T_T__If => False, Turkixir_T_T__Else => False, Turkixir_T_T__Del => False, Turkixir_T_T__Minus_Assign => False, Turkixir_T_T__Or => False, Turkixir_T_T__Minus => False, Turkixir_T_T__Lbrack => False, Turkixir_T_T__And_Assign => False, Turkixir_T_T_R_Par => False, Turkixir_T_T__Global => False, Turkixir_T_T__For => False, Turkixir_T_T__From => False, Turkixir_T_T__Rsh => False, Turkixir_T_T__Finally => False, Turkixir_T_T__Pass => False, Turkixir_T_T__Lsh_Assign => False, Turkixir_T_T__Bin_Or => False, Turkixir_T_T__Rcurl => False, Turkixir_T_T__With => False, Turkixir_T_T__Plus => False, Turkixir_T_T__Lt => False, Turkixir_T_T__Number => False, Turkixir_T_T__String => False, Turkixir_T_T__Comment => True, Turkixir_T_T__Id => False, Turkixir_Indent => False, Turkixir_Dedent => False, Turkixir_Newline => False
   );

   type Character_Range is record
      First, Last : Character_Type;
   end record;

   type Character_Range_Array is array (Positive range <>) of Character_Range;
   --  Sorted list of dijoint character ranges

   pragma Warnings (Off, "referenced");
   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean;
   --  Return whether Char is included in the given ranges
   pragma Warnings (On, "referenced");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : out Lexer_State;
      Input       : Text_Access;
      Input_First : Positive;
      Input_Last  : Natural) is
   begin
      Self.Input := Input;
      Self.Input_First := Input_First;
      Self.Input_Last := Input_Last;
      Self.Has_Next := True;
      Self.Last_Token := (Kind       => Turkixir_Termination,
                          Text_First => Input_First,
                          Text_Last  => Input_First - 1);
      Self.Last_Token_Kind := Turkixir_Termination;
   end Initialize;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Self : Lexer_State) return Lexed_Token is
   begin
      return Self.Last_Token;
   end Last_Token;

   --------------
   -- Has_Next --
   --------------

   function Has_Next (Self : Lexer_State) return Boolean is
   begin
      return Self.Has_Next;
   end Has_Next;

   --------------
   -- Contains --
   --------------

   function Contains
     (Char : Character_Type; Ranges : Character_Range_Array) return Boolean
   is
      Low  : Natural := Ranges'First;
      High : Natural := Ranges'Last;
   begin
      while Low <= High loop
         declare
            Middle : constant Natural := (Low + High) / 2;
            R      : Character_Range renames Ranges (Middle);
         begin
            if Char < R.First then
               High := Middle - 1;
            elsif Char > R.Last then
               Low := Middle + 1;
            else
               return True;
            end if;
         end;
      end loop;
      return False;
   end Contains;



   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Self : in out Lexer_State; Token : out Lexed_Token)
   is
      Input : constant Text_Access := Self.Input;

      First_Index : Positive;
      --  Index of the first input character for the token to return

      Index : Positive;
      --  Index for the next input character to be analyzed

      Match_Index : Natural;
      --  If we found a match, index for its last character. Otherwise, zero.

      Match_Ignore : Boolean;
      --  If we found a match, whether we must ignore it and restart the
      --  automaton after its character range.

      Match_Kind : Token_Kind;
      --  If we found a match and it is not ignored, kind for the token to
      --  emit. Meaningless otherwise.
   begin
      First_Index := Self.Last_Token.Text_Last + 1;

      <<Start>>
      Index := First_Index;
      Match_Index := 0;
      Match_Ignore := False;



         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) | Character_Type'Val (16#d#) | ' ' => goto State_1;
               when Character_Type'Val (16#a#) => goto State_2;
               when '!' => goto State_3;
               when '"' => goto State_4;
               when '#' => goto State_5;
               when '%' => goto State_6;
               when '&' => goto State_7;
               when ''' => goto State_8;
               when '(' => goto State_9;
               when ')' => goto State_10;
               when '*' => goto State_11;
               when '+' => goto State_12;
               when ',' => goto State_13;
               when '-' => goto State_14;
               when '.' => goto State_15;
               when '/' => goto State_16;
               when '0' .. '9' => goto State_17;
               when ':' => goto State_18;
               when ';' => goto State_19;
               when '<' => goto State_20;
               when '=' => goto State_21;
               when '>' => goto State_22;
               when '@' => goto State_23;
               when 'A' .. 'Q' | 'S' .. 'T' | 'V' .. 'Z' | '_' | 'b' .. 'a' | 'c' .. 'b' | 'd' .. 'c' | 'e' .. 'd' | 'f' .. 'e' | 'g' .. 'f' | 'h' | 'j' .. 'k' | 'm' | 'o' .. 'n' | 'p' .. 'o' | 'q' | 's' | 'u' .. 't' | 'v' | 'x' | 'z' => goto State_24;
               when 'R' => goto State_25;
               when 'U' => goto State_26;
               when '[' => goto State_27;
               when '\' => goto State_28;
               when ']' => goto State_29;
               when '^' => goto State_30;
               when '`' => goto State_31;
               when 'a' => goto State_32;
               when 'b' => goto State_33;
               when 'c' => goto State_34;
               when 'd' => goto State_35;
               when 'e' => goto State_36;
               when 'f' => goto State_37;
               when 'g' => goto State_38;
               when 'i' => goto State_39;
               when 'l' => goto State_40;
               when 'n' => goto State_41;
               when 'o' => goto State_42;
               when 'p' => goto State_43;
               when 'r' => goto State_44;
               when 't' => goto State_45;
               when 'u' => goto State_46;
               when 'w' => goto State_47;
               when 'y' => goto State_48;
               when '{' => goto State_49;
               when '|' => goto State_50;
               when '}' => goto State_51;
               when '~' => goto State_52;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_1>>

               Match_Index := Index - 1;
               Match_Ignore := True;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) | Character_Type'Val (16#d#) | ' ' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_2>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_Newline;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_3>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_54;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_4>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when '"' => goto State_56;
               when '\' => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_5>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_58;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_6>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Mod;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_59;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_7>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Amp;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_60;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_8>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_61;
               when ''' => goto State_62;
               when '\' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_9>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T_L_Par;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_10>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T_R_Par;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_11>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Multiply;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '*' => goto State_64;
               when '=' => goto State_65;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_12>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Plus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_66;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_13>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Comma;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_14>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Minus;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_67;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_15>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Dot;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_16>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Divide;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '/' => goto State_68;
               when '=' => goto State_69;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_17>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_70;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_18>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Colon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_19>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Semicolon;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_20>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lt;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '<' => goto State_71;
               when '=' => goto State_72;
               when '>' => goto State_73;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_21>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_74;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_22>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Gt;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_75;
               when '>' => goto State_76;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_23>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__At;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_24>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_25>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_26>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Q' | 'S' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'R' => goto State_78;
               when 'r' => goto State_79;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_27>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lbrack;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_28>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#a#) => goto State_80;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_29>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Rbrack;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_30>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Xor;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_81;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_31>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Backtick;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_32>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'r' | 't' .. 'z' => goto State_77;
               when 'n' => goto State_82;
               when 's' => goto State_83;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_33>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_84;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_34>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'k' | 'm' .. 'n' | 'p' .. 'z' => goto State_77;
               when 'a' => goto State_85;
               when 'l' => goto State_86;
               when 'o' => goto State_87;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_35>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_88;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_36>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'w' | 'y' .. 'z' => goto State_77;
               when 'l' => goto State_89;
               when 'x' => goto State_90;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_37>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'n' | 'p' .. 'q' | 's' .. 'z' => goto State_77;
               when 'i' => goto State_91;
               when 'o' => goto State_92;
               when 'r' => goto State_93;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_38>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_94;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_39>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'e' | 'g' .. 'l' | 'n' .. 'm' | 'o' .. 'r' | 't' .. 'z' => goto State_77;
               when 'f' => goto State_95;
               when 'm' => goto State_96;
               when 'n' => goto State_97;
               when 's' => goto State_98;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_40>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_99;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_41>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 'z' => goto State_77;
               when 'o' => goto State_100;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_42>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_101;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_43>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'q' | 's' .. 'z' => goto State_77;
               when 'a' => goto State_102;
               when 'r' => goto State_103;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_44>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'a' => goto State_104;
               when 'e' => goto State_105;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_45>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_106;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_46>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Q' | 'S' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'R' => goto State_78;
               when 'r' => goto State_79;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_47>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'h' => goto State_107;
               when 'i' => goto State_108;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_48>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'i' => goto State_109;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_49>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T_L_Curl;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_50>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Bin_Or;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_110;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_51>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Rcurl;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_52>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Invert;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_53>>

               Match_Index := Index - 1;
               Match_Ignore := True;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) | Character_Type'Val (16#d#) | ' ' => goto State_53;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_54>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Notequal;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_55>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when '"' => goto State_111;
               when '\' => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_56>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_112;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_57>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when '"' => goto State_113;
               when '\' => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_58>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_114;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_59>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Mod_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_60>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__And_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_61>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_61;
               when ''' => goto State_115;
               when '\' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_62>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when ''' => goto State_116;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_63>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_61;
               when ''' => goto State_117;
               when '\' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_64>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Power;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_118;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_65>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Mult_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_66>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Add_Asign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_67>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Minus_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_68>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Floordiv;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_119;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_69>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Div_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_70>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Number;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' => goto State_70;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_71>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lsh;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_120;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_72>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lte;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_73>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Diamond;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_74>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Equals;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_75>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Gte;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_76>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Rsh;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '=' => goto State_121;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_77>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_78>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_79>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '"' => goto State_4;
               when ''' => goto State_8;
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_80>>

               Match_Index := Index - 1;
               Match_Ignore := True;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) | Character_Type'Val (16#d#) | ' ' => goto State_122;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_81>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Xor_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_82>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_77;
               when 'd' => goto State_123;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_83>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__As;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_124;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_84>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_125;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_85>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_126;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_86>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_127;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_87>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_77;
               when 'n' => goto State_128;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_88>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'e' | 'g' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'f' => goto State_129;
               when 'l' => goto State_130;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_89>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'r' | 't' .. 'z' => goto State_77;
               when 'i' => goto State_131;
               when 's' => goto State_132;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_90>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_133;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_91>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_77;
               when 'n' => goto State_134;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_92>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_135;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_93>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 'z' => goto State_77;
               when 'o' => goto State_136;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_94>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 'z' => goto State_77;
               when 'o' => goto State_137;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_95>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__If;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_96>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'o' | 'q' .. 'z' => goto State_77;
               when 'p' => goto State_138;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_97>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__In;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_98>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Is;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_99>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_77;
               when 'm' => goto State_139;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_100>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_140;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_101>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Or;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_102>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_141;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_103>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'i' => goto State_142;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_104>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'i' => goto State_143;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_105>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_144;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_106>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'x' | 'z' => goto State_77;
               when 'y' => goto State_145;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_107>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'i' => goto State_146;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_108>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_147;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_109>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_148;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_110>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Or_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_111>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_112>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_149;
               when Character_Type'Val (16#a#) => goto State_150;
               when '"' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_113>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_55;
               when '"' => goto State_111;
               when '\' => goto State_57;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_114>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Comment;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. Character_Type'Val (16#10ffff#) => goto State_114;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_115>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_116>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_152;
               when Character_Type'Val (16#a#) => goto State_153;
               when ''' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_117>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. '[' | ']' .. Character_Type'Val (16#10ffff#) => goto State_61;
               when ''' => goto State_115;
               when '\' => goto State_63;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_118>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Power_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_119>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Floordiv_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_120>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lsh_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_121>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Rsh_Assign;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_122>>

               Match_Index := Index - 1;
               Match_Ignore := True;


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#9#) | Character_Type'Val (16#d#) | ' ' => goto State_122;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_123>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__And;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_124>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_155;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_125>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_156;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_126>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_77;
               when 'c' => goto State_157;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_127>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_158;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_128>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_159;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_129>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Def;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_130>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Del;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_131>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'e' | 'g' .. 'z' => goto State_77;
               when 'f' => goto State_160;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_132>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_161;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_133>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'b' | 'd' .. 'z' => goto State_77;
               when 'c' => goto State_162;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_134>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_163;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_135>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__For;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_136>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'l' | 'n' .. 'z' => goto State_77;
               when 'm' => goto State_164;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_137>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' | 'c' .. 'z' => goto State_77;
               when 'b' => goto State_165;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_138>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'n' | 'p' .. 'z' => goto State_77;
               when 'o' => goto State_166;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_139>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' | 'c' .. 'z' => goto State_77;
               when 'b' => goto State_167;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_140>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Not;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_141>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_168;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_142>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_77;
               when 'n' => goto State_169;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_143>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_170;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_144>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_77;
               when 'u' => goto State_171;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_145>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Try;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_146>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_172;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_147>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_77;
               when 'h' => goto State_173;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_148>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_174;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_149>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_149;
               when Character_Type'Val (16#a#) => goto State_150;
               when '"' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_150>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_149;
               when Character_Type'Val (16#a#) => goto State_150;
               when '"' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_151>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_175;
               when '"' => goto State_176;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_152>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_152;
               when Character_Type'Val (16#a#) => goto State_153;
               when ''' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_153>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_152;
               when Character_Type'Val (16#a#) => goto State_153;
               when ''' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_154>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_177;
               when ''' => goto State_178;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_155>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_179;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_156>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'j' | 'l' .. 'z' => goto State_77;
               when 'k' => goto State_180;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_157>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'g' | 'i' .. 'z' => goto State_77;
               when 'h' => goto State_181;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_158>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'r' | 't' .. 'z' => goto State_77;
               when 's' => goto State_182;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_159>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'h' | 'j' .. 'z' => goto State_77;
               when 'i' => goto State_183;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_160>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Elif;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_161>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Else;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_162>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Exec;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_163>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_184;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_164>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__From;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_165>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_185;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_166>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_186;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_167>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_77;
               when 'd' => goto State_187;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_168>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Pass;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_169>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_188;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_170>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_189;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_171>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'q' | 's' .. 'z' => goto State_77;
               when 'r' => goto State_190;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_172>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_191;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_173>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__With;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_174>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'c' | 'e' .. 'z' => goto State_77;
               when 'd' => goto State_192;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_175>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_149;
               when Character_Type'Val (16#a#) => goto State_150;
               when '"' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_176>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_193;
               when '"' => goto State_194;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_177>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_152;
               when Character_Type'Val (16#a#) => goto State_153;
               when ''' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_178>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_195;
               when ''' => goto State_196;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_179>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_197;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_180>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Break;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_181>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Except;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_182>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Class;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_183>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_77;
               when 'n' => goto State_198;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_184>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_199;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_185>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'k' | 'm' .. 'z' => goto State_77;
               when 'l' => goto State_200;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_186>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 's' | 'u' .. 'z' => goto State_77;
               when 't' => goto State_201;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_187>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'b' .. 'z' => goto State_77;
               when 'a' => goto State_202;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_188>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Print;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_189>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Raise;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_190>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'm' | 'o' .. 'z' => goto State_77;
               when 'n' => goto State_203;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_191>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__While;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_192>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Yield;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_193>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '!' | '#' .. Character_Type'Val (16#10ffff#) => goto State_149;
               when Character_Type'Val (16#a#) => goto State_150;
               when '"' => goto State_151;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_194>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_195>>


         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when Character_Type'Val (16#0#) .. Character_Type'Val (16#9#) | Character_Type'Val (16#b#) .. '&' | '(' .. Character_Type'Val (16#10ffff#) => goto State_152;
               when Character_Type'Val (16#a#) => goto State_153;
               when ''' => goto State_154;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_196>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__String;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         Index := Index + 1;
         goto Stop;

            <<State_197>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Assert;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_198>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 't' | 'v' .. 'z' => goto State_77;
               when 'u' => goto State_204;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_199>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'x' | 'z' => goto State_77;
               when 'y' => goto State_205;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_200>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Global;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_201>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Import;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_202>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Lambda;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_203>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Return;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_204>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Id;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'd' | 'f' .. 'z' => goto State_77;
               when 'e' => goto State_206;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_205>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Finally;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;

            <<State_206>>

               Match_Index := Index - 1;
               Match_Kind := Turkixir_T_T__Continue;

         if Index > Self.Input_Last then
            goto Stop;
         end if;

         declare
            Input_Char : constant Character_Type := Input (Index);
         begin
            Index := Index + 1;

            case Input_Char is
               when '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' => goto State_77;

            when others =>

               goto Stop;
            end case;
         end;


      <<Stop>>
      --  We end up here as soon as the currently analyzed character was not
      --  accepted by any transitions from the current state. Two cases from
      --  there:

      if Match_Index = 0 then
         --  We haven't found a match. Just create an error token and plan to
         --  start a new token at the next character.
         if Index > Self.Input_Last then
            Token := (Turkixir_Termination, Index, Index - 1);
            Self.Has_Next := False;
         else
            Token := (Turkixir_Lexing_Failure, First_Index, First_Index);
         end if;

      elsif Match_Ignore then
         --  We found a match. It must be ignored: resume lexing to start right
         --  after the matched text.
         First_Index := Match_Index + 1;
         goto Start;

      else
         --  We found a match for which we must emit a token
         Token := (Match_Kind, First_Index, Match_Index);
      end if;

      Self.Last_Token := Token;
      if not Is_Trivia (Token.Kind) then
         Self.Last_Token_Kind := Token.Kind;
      end if;
   end Next_Token;

end Libturkixirlang.Lexer_State_Machine;
