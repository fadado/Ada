pragma Assertion_Policy(Check); -- Check / Ignore

------------------------------------------------------------------------
package Generics.Tuples is
------------------------------------------------------------------------

   generic
      type INDEX_TYPE is (<>);
      type ELEMENT_TYPE is private;
      type ARRAY_TYPE is array(INDEX_TYPE range <>) of ELEMENT_TYPE;
   package Signature is private end;

   generic
      with package Signature_Package is new Signature (<>);
      use Signature_Package;
      with procedure Do_It(t: in out ARRAY_TYPE);
   function Applied
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline;

   ---------------------------------------------------------------------
   generic
      with package Signature_Package is new Signature (<>);
      use Signature_Package;
   package Place is
   ---------------------------------------------------------------------

      function "=" (a, b: ELEMENT_TYPE) return BOOLEAN
      is (raise Not_Implemented) with Inline;
      -- Forbid private type equality!

      procedure Reverse_It
        (t : in out ARRAY_TYPE);

      function Reversed is
         new Applied (Signature_Package, Reverse_It);

      procedure Rotate_It
        (n : in     INDEX_TYPE;
         t : in out ARRAY_TYPE);
      -- Rotate left!
      -- Rotate right: Rotate_It(t'Length-n, t);

     function Rotated
        (n : in INDEX_TYPE;
         t : in ARRAY_TYPE) return ARRAY_TYPE;

      -- TODO: shuffle, shuffled, take

      --------------
      -- Functors --
      --------------

      generic
         with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
      procedure Apply_To
        (t : in out ARRAY_TYPE);

      generic
         with function Map (x: in ELEMENT_TYPE) return ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return ARRAY_TYPE;

      generic
         with function Operation (L, R: in ELEMENT_TYPE) return ELEMENT_TYPE;
      function Reducer
        (t : in ARRAY_TYPE) return ELEMENT_TYPE
      with Pre => t'Length > 0;

      generic
         with function Better (L, R: in ELEMENT_TYPE) return BOOLEAN;
      function Chooser
        (t : in ARRAY_TYPE) return ELEMENT_TYPE
      with Pre => t'Length > 0;

      -- TODO: filter (test)
   end Place;

   ---------------------------------------------------------------------
   generic
      with package Signature_Package is new Signature (<>);
      use Signature_Package;
      with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package Equiv is
   ---------------------------------------------------------------------

      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN;

      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      is (for some i in t'Range => x = t(i))
      with Inline;

      function Search
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      with Pre => t'Length > 0;

      -- TODO: squashed
   end Equiv;

   ---------------------------------------------------------------------
   generic
      with package Signature_Package is new Signature (<>);
      use Signature_Package;
      with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package Order is
   ---------------------------------------------------------------------

      function Is_Sorted
        (t : in ARRAY_TYPE) return BOOLEAN;

      pragma Assertion_Policy(Ignore);

      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN
      with Pre => Is_Sorted(t);

      pragma Assertion_Policy(Check);

      procedure Sort_It
        (t : in out ARRAY_TYPE)
      with Post => Is_Sorted(t);

      function Sorted is
         new Applied (Signature_Package, Sort_It);

      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN;

      function Search
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      with Pre => Is_Sorted(t);

   end Order;

end Generics.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
