with Ada.Containers.Generic_Array_Sort;

generic

   type INDEX_TYPE   is (<>);
   type ELEMENT_TYPE is (<>);
   type TUPLE_TYPE   is array (INDEX_TYPE range <>) of ELEMENT_TYPE;

package Music.Tuples is

   procedure Sort is
      new Ada.Containers.Generic_Array_Sort (
         Index_Type   => INDEX_TYPE,
         Element_Type => ELEMENT_TYPE,
         Array_Type   => TUPLE_TYPE
   );

   function Sorted
     (s : TUPLE_TYPE) return TUPLE_TYPE;

   procedure Swap
     (s : in out TUPLE_TYPE;
      i : in INDEX_TYPE;
      j : in INDEX_TYPE);

end Music.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
