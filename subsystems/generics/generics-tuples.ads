------------------------------------------------------------------------------
--  Generics . Tuples specification
------------------------------------------------------------------------------

package Generics . Tuples is

   pragma Assertion_Policy (Check); -- Check / Ignore

   generic
      type ELEMENT_TYPE is private;
      type INDEX_TYPE   is (<>);
      type ARRAY_TYPE   is array (INDEX_TYPE range <>) of ELEMENT_TYPE;
   package Tuple_Signature is private end Tuple_Signature;
   --  Declares the signature for a tuple.

   generic
      with package TupleInstance is new Tuple_Signature (<>);
      use TupleInstance;
      -- type ELEMENT_TYPE is private;
      -- type INDEX_TYPE   is (<>);
      -- type ARRAY_TYPE   is array (INDEX_TYPE range <>) of ELEMENT_TYPE;
      with procedure Do_It(t: in out ARRAY_TYPE);
   function Functional
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline,
        Post => t'Length = Functional'Result'Length;
   --  Generates a functional version for an imperative procedure that
   --  transforms a tuple.

private

   generic
      with package TupleInstance is new Tuple_Signature (<>);
      use TupleInstance;
      -- type ELEMENT_TYPE is private;
      -- type INDEX_TYPE   is (<>);
      -- type ARRAY_TYPE   is array (INDEX_TYPE range <>) of ELEMENT_TYPE;
      with function Member(x: ELEMENT_TYPE; t: in ARRAY_TYPE) return BOOLEAN;
   function Squasher
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Post => t'Length >= Squasher'Result'Length;
   --  Generates a function returning a tuple without duplicates as determined
   --  by a boolean test.

end Generics . Tuples;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
