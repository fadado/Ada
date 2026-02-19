pragma Assertion_Policy(Check); -- Check / Ignore

generic
   with package Instance is new Signature (<>);
   use Instance;
   with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;

package Generics . Tuples . Arrayed is

   procedure Reverse_It
     (t : in out ARRAY_TYPE);

   function Reversed is
      new Tuples.Functional (Instance, Reverse_It);

   procedure Left_Rotate_It
     (n : in     NATURAL;
      t : in out ARRAY_TYPE)
   with Pre => n <= t'Length;

   function Left_Rotated
     (n : in NATURAL;
      t : in ARRAY_TYPE) return ARRAY_TYPE;

   procedure Right_Rotate_It
     (n : in     NATURAL;
      t : in out ARRAY_TYPE)
   with Inline,
        Pre => n <= t'Length;

   function Right_Rotated
     (n : in NATURAL;
      t : in ARRAY_TYPE) return ARRAY_TYPE
   is (Left_Rotated(t'Length - n, t)) with Inline;

   function Member
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return BOOLEAN
   with Inline;

   function Search
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return INDEX_TYPE
   with Pre => t'Length > 0;

   function Contains_Duplicates
     (t : in ARRAY_TYPE) return BOOLEAN;

   function Remove_Duplicates
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline,
        Post => not Contains_Duplicates(Remove_Duplicates'Result);

end Generics . Tuples . Arrayed;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
