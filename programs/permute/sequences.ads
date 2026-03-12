pragma Assertion_Policy(Check); -- Check / Ignore

with Control.Generators;
with Generics.Tuples;

generic
   with package TupleInstance is
      new Generics.Tuples.Tuple_Signature (<>);
   use TupleInstance;

   Objects  : in ARRAY_TYPE;
   N        : in NATURAL := 0;
   Repeated : in BOOLEAN := FALSE;

package Sequences
is
   use Control;

   Bits : constant := 8;

   pragma Assert (Objects'Length > 0);
   pragma Assert (INDEX_TYPE'Pos(Objects'First) >= 0);
   pragma Assert (INDEX_TYPE'Pos(Objects'Last)  <= 2**Bits-1);

   ------------------------------------------------------------------
   --
   ------------------------------------------------------------------

   type TINY is range 0..2**Bits-1;

   First : constant TINY := TINY(INDEX_TYPE'Pos(Objects'First));
   Last  : constant TINY := TINY(INDEX_TYPE'Pos(Objects'Last));

   subtype VALUES is TINY range First..Last;

   subtype INDICES is TINY
      range First..(if N = 0 then Last else First+TINY(N)-1);

   type ORDERING is array (INDICES) of VALUES
   with Pack,
        Component_Size => Bits;

   package DeclarePermutationsTypes is
      new Generators (
         Element_Type => ORDERING,
         Context_Type => VOID
   );
   use DeclarePermutationsTypes;

   procedure Generate
     (generator : in out GENERATOR_INTERFACE'Class;
      context   : access VOID);

   subtype ITERABLE_TYPE is GENERATOR_TYPE (Generate'Access, NULL);
   -- the only exported name!

end Sequences;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
