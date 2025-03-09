with Ada.Containers.Generic_Array_Sort;

package Generics is

   generic
      type T(<>) is private;
   procedure Swap(x, y: in out T) with Inline;

   generic
      type A(<>) is limited private;
      type B(<>) is limited private;
      type C(<>) is limited private;
      with function F(x: A) return B;
      with function G(x: B) return C;
   function Compose(x: A) return C with Inline;

   generic
      type INDEX_TYPE is (<>);
      type COMPONENT_TYPE is private;
      type TUPLE_TYPE is array(INDEX_TYPE range <>) of COMPONENT_TYPE;
      with function "<"(a, b: COMPONENT_TYPE) return BOOLEAN is <>;
   package Tuples is

      procedure Sort is
         new Ada.Containers.Generic_Array_Sort (
            Index_Type   => INDEX_TYPE,
            Element_Type => COMPONENT_TYPE,
            Array_Type   => TUPLE_TYPE
      );
   end Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
