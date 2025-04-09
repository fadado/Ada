pragma Assertion_Policy(Check); -- Check / Ignore

------------------------------------------------------------------------
package Generics.Tuples is
------------------------------------------------------------------------

   generic
      type INDEX_TYPE is (<>);
      type ELEMENT_TYPE is private;
      type ARRAY_TYPE is array (INDEX_TYPE range <>) of ELEMENT_TYPE;
   package Signature is private end;

   generic
      with package Source is new Signature (<>);
      use Source;
      with procedure Do_It(t: in out ARRAY_TYPE);
   function Functor
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline;

   -- In place application:
   --    for e of t(i..j) loop e := f(e); end loop;

   ---------------------------------------------------------------------
   generic
      with package Source is new Signature (<>);
      use Source;
   package Place is
   ---------------------------------------------------------------------

      function "=" (a, b: ELEMENT_TYPE) return BOOLEAN
      is (raise Not_Implemented) with Inline;
      -- Forbid private type equality!

      procedure Reverse_It
        (t : in out ARRAY_TYPE);

      function Reversed is
         new Functor (Source, Reverse_It);

      procedure Rotate_It
        (n : in     INDEX_TYPE;
         t : in out ARRAY_TYPE);
      -- Rotate left!
      -- Rotate right: Rotate_It(t'Length-n, t);

      function Rotated
        (n : in INDEX_TYPE;
         t : in ARRAY_TYPE) return ARRAY_TYPE;

      --------------
      -- Functors --
      --------------

      generic
         with package Target is new Signature (<>);
         with function Map (X: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Mapper
        (t : in ARRAY_TYPE) return Target.ARRAY_TYPE;

      generic
         with package Target is new Signature (<>);
         with function Zip (X, Y: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
      function Zipper
        (s, t : in ARRAY_TYPE) return Target.ARRAY_TYPE
      with Pre => s'Length = t'Length and then s'First = t'First;

      generic
         with function Test (X: in ELEMENT_TYPE) return BOOLEAN;
      function Filter
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

   end Place;

   ---------------------------------------------------------------------
   generic
      with package Source is new Signature (<>);
      use Source;
      with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package Equiv is
   ---------------------------------------------------------------------

      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN;

      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      with Inline;

      function Search
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      with Pre => t'Length > 0;

      function Squashed
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      with Post => Is_Unique(Squashed'Result);

   end Equiv;

   ---------------------------------------------------------------------
   generic
      with package Source is new Signature (<>);
      use Source;
      with function "<" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      with function ">" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
      with function "=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   package Order is
   ---------------------------------------------------------------------

      function Is_Sorted
        (t : in ARRAY_TYPE) return BOOLEAN;

      function Is_Unique
        (t : in ARRAY_TYPE) return BOOLEAN
      with Pre => Is_Sorted(t);

      procedure Sort_It
        (t : in out ARRAY_TYPE)
      with Post => Is_Sorted(t);

      function Sorted is
         new Functor (Source, Sort_It);

      function Member
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return BOOLEAN
      with Pre => Is_Sorted(t);

      function Search
        (x : in ELEMENT_TYPE;
         t : in ARRAY_TYPE) return INDEX_TYPE
      with Pre => Is_Sorted(t);

      function Squashed
        (t : in ARRAY_TYPE) return ARRAY_TYPE
      with Pre  => Is_Sorted(t),
           Post => Is_Unique(Squashed'Result);

   end Order;

end Generics.Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
