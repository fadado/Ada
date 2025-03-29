pragma Assertion_Policy(Check); -- Check / Ignore

package Generics is
   pragma Pure(Generics);

   generic
      type T(<>) is private;
   procedure Swap
     (x, y : in out T)
   with Inline;

   generic
      type A(<>) is limited private;
      type B(<>) is limited private;
      type C(<>) is limited private;
      with function F(x: in A) return B;
      with function G(x: in B) return C;
   function Compose
     (x : in A) return C
   with Inline;

   ---------------------------------------------------------------------
   package Tuples is
   ---------------------------------------------------------------------

      Not_Found : exception;

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
      end Functors;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
      ------------------------------------------------------------------
      package Location is
      ------------------------------------------------------------------

         function Reversed
           (t : in ARRAY_TYPE) return ARRAY_TYPE;

         function Rotated
           (n : in INDEX_TYPE;
            t : in ARRAY_TYPE) return ARRAY_TYPE;

         -- TODO: shuffle, shuffled, take
      end Location;

      generic
         with package Signature_Package is new Signature (<>);
         use Signature_Package;
         with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      ------------------------------------------------------------------
      package Uniquity is
      ------------------------------------------------------------------

         function Is_Unique
           (t : in ARRAY_TYPE) return BOOLEAN;

         function Member
           (x : in ELEMENT_TYPE;
            t : in ARRAY_TYPE) return BOOLEAN;

         function Position
           (x : in ELEMENT_TYPE;
            t : in ARRAY_TYPE) return INDEX_TYPE
         with Pre => t'Length > 0;

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

         function Is_Sorted
           (t : in ARRAY_TYPE) return BOOLEAN;

         pragma Assertion_Policy(Ignore);

         function Is_Unique
           (t : in ARRAY_TYPE) return BOOLEAN
         with Pre => Is_Sorted(t);

         pragma Assertion_Policy(Check);

       --function Member
       --  (x : in ELEMENT_TYPE;
       --   t : in ARRAY_TYPE) return BOOLEAN
       --with Pre => Is_Sorted(t);

         procedure Sort_It
           (t : in out ARRAY_TYPE)
         with Post => Is_Sorted(t);

         function Sorted
           (t : in ARRAY_TYPE) return ARRAY_TYPE
         with Inline;

         function Search
           (t : in ARRAY_TYPE; x : ELEMENT_TYPE) return INDEX_TYPE
         with Pre => Is_Sorted(t);

           -- TODO: merge
      end Order;

   end Tuples;

end Generics;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
