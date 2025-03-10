with Ada.Containers.Generic_Array_Sort;

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

   -- TODO: split Tuples / Orders
   generic
      with package T_S_P is new Tuple_Signature(<>);
      use T_S_P;
      with function "<"(a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package Tuples is

      function Map
        (s : ARRAY_TYPE;
         f : access function (x: ELEMENT_TYPE) return ELEMENT_TYPE)
        return ARRAY_TYPE;

      function Member
        (x : ELEMENT_TYPE;
         s : ARRAY_TYPE) return BOOLEAN
      with Inline;

      function Ordered
        (s : ARRAY_TYPE) return BOOLEAN;

      function Reversed
        (s : ARRAY_TYPE) return ARRAY_TYPE;

      function Rotated
        (n : INDEX_TYPE;
         s : ARRAY_TYPE) return ARRAY_TYPE;

      procedure Sort is
         new Ada.Containers.Generic_Array_Sort (
            Index_Type   => INDEX_TYPE,
            Element_Type => ELEMENT_TYPE,
            Array_Type   => ARRAY_TYPE
      );

      function Sorted
        (s : ARRAY_TYPE) return ARRAY_TYPE;

      function Unique
        (s : ARRAY_TYPE) return BOOLEAN;

   end Tuples;

end Generics;
-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
