pragma Assertion_Policy(Check); -- Check / Ignore

package Generics is
   pragma Pure(Generics);

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

   ---------------------------------------------------------------------
   package Tuples is
   ---------------------------------------------------------------------

      generic
         type INDEX_TYPE is (<>);
         type ELEMENT_TYPE is private;
         type ARRAY_TYPE is array(INDEX_TYPE range <>) of ELEMENT_TYPE;
      package Signature is private end;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
      ------------------------------------------------------------------
      package Functors is
      ------------------------------------------------------------------

         generic
            with function Map (x: ELEMENT_TYPE) return ELEMENT_TYPE;
         function Mapper
           (t : ARRAY_TYPE) return ARRAY_TYPE;

         generic
            with function Operation (L, R: ELEMENT_TYPE) return ELEMENT_TYPE;
         function Reducer
           (t : ARRAY_TYPE) return ELEMENT_TYPE
         with Pre => t'Length > 0;

         generic
            with function Better (L, R: ELEMENT_TYPE) return BOOLEAN;
         function Chooser
           (t : ARRAY_TYPE) return ELEMENT_TYPE
         with Pre => t'Length > 0;

         -- TODO: filter (test)
      end Functors;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
      ------------------------------------------------------------------
      package Location is
      ------------------------------------------------------------------

         function Reversed
           (t : ARRAY_TYPE) return ARRAY_TYPE;

         function Rotated
           (n : INDEX_TYPE;
            t : ARRAY_TYPE) return ARRAY_TYPE;

         -- TODO: shuffle, shuffled, take
      end Location;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
         with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      ------------------------------------------------------------------
      package Uniquity is
      ------------------------------------------------------------------

         function Member
           (x : ELEMENT_TYPE;
            t : ARRAY_TYPE) return BOOLEAN
         with Inline;

         function Position
           (x : ELEMENT_TYPE;
            t : ARRAY_TYPE) return INDEX_TYPE
         with Pre => t'Length > 0;

         function Is_Unique
           (t : ARRAY_TYPE) return BOOLEAN;

         -- TODO: squashed
      end Uniquity;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
         with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
         with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      ------------------------------------------------------------------
      package Order is
      ------------------------------------------------------------------

         procedure Sort
           (t : in out ARRAY_TYPE)
         with Post => Is_Sorted(t);

         function Is_Sorted
           (t : ARRAY_TYPE) return BOOLEAN
         with Inline;

         function Sorted
           (t : ARRAY_TYPE) return ARRAY_TYPE
         with Inline;

         function Search
           (t : ARRAY_TYPE; x : ELEMENT_TYPE) return INDEX_TYPE
         with Pre => Is_Sorted(t);

           -- TODO: merge
      end Order;

   end Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
