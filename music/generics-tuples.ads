pragma Assertion_Policy(Check); -- Check / Ignore

------------------------------------------------------------------------
package Generics . Tuples is
------------------------------------------------------------------------

   generic
      type ELEMENT_TYPE is private;
      type INDEX_TYPE   is (<>);
      type ARRAY_TYPE   is array (INDEX_TYPE range <>) of ELEMENT_TYPE;
   package Signature is private end;

   generic
      with package Instance is new Signature (<>);
      use Instance;
      with procedure Do_It(t: in out ARRAY_TYPE);
   function Functional
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline;

private

   generic
      with package Instance is new Signature (<>);
      use Instance;
      with function Member(x: ELEMENT_TYPE; t: in ARRAY_TYPE) return BOOLEAN;
   function remdups
     (t : in ARRAY_TYPE) return ARRAY_TYPE;

end Generics . Tuples;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
