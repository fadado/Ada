package Generics is

   generic
      type T(<>) is private;
   procedure Swap
     (x, y: in out T)
   with Inline;

   generic
      type A(<>) is limited private;
      type B(<>) is limited private;
      type C(<>) is limited private;
      with function F(x: A) return B;
      with function G(x: B) return C;
   function Compose
     (x: A) return C
   with Inline;

   generic
      type INDEX_TYPE is (<>);
      type ELEMENT_TYPE is private;
      type ARRAY_TYPE is array(INDEX_TYPE range <>) of ELEMENT_TYPE;
   package Tuple_Signature is private end;

   generic
      with package Tuple_Signature_Package is new Tuple_Signature (<>);
      use Tuple_Signature_Package;
   ---------------------------------------------------------------------
   package Any_Tuples is
   ---------------------------------------------------------------------

      function Reversed
        (t : ARRAY_TYPE) return ARRAY_TYPE;

      function Rotated
        (n : INDEX_TYPE;
         t : ARRAY_TYPE) return ARRAY_TYPE;

      generic
         with function map (x: ELEMENT_TYPE) return ELEMENT_TYPE;
      function Mapper
        (t : ARRAY_TYPE) return ARRAY_TYPE;

      generic
         with function reduce (L, R: ELEMENT_TYPE) return ELEMENT_TYPE;
      function Reducer
        (t : ARRAY_TYPE) return ELEMENT_TYPE;

      generic
         with function better (L, R: ELEMENT_TYPE) return BOOLEAN;
      function Chooser
        (t : ARRAY_TYPE) return ELEMENT_TYPE;

   end Any_Tuples;

   generic
      with package Tuple_Signature_Package is new Tuple_Signature (<>);
      use Tuple_Signature_Package;
   ---------------------------------------------------------------------
   package Eq_Tuples is
   ---------------------------------------------------------------------

      function Member
        (x : ELEMENT_TYPE;
         t : ARRAY_TYPE) return BOOLEAN
      with Inline;

      function Position
        (x : ELEMENT_TYPE;
         t : ARRAY_TYPE) return INDEX_TYPE;

      function Is_Unique
        (t : ARRAY_TYPE) return BOOLEAN;

   end Eq_Tuples;

   generic
      with package Tuple_Signature_Package is new Tuple_Signature (<>);
      use Tuple_Signature_Package;
      with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   ---------------------------------------------------------------------
   package Ord_Tuples is
   ---------------------------------------------------------------------

      procedure Sort
        (t : in out ARRAY_TYPE);

      function Is_Sorted
        (t : ARRAY_TYPE) return BOOLEAN;

      function Sorted
        (t : ARRAY_TYPE) return ARRAY_TYPE;

      function Search
        (t : ARRAY_TYPE; x : ELEMENT_TYPE) return INDEX_TYPE;

   end Ord_Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
