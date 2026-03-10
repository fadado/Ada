-- Generates permutations

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;

with Control.Generators;
with Generics.Depth_First_Search;

procedure Permute
is
   use Control;

   procedure Permute_Set   -- TODO add k- parameter ???
     (Set : in STRING)
   is
      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      Bits : constant := 8;

      type TINY is range 0..2**Bits-1;

      subtype INDICES is TINY range TINY(Set'First)..TINY(Set'Last);

      type BIJECTION is array (INDICES) of INDICES
      with Pack,
           Component_Size => Bits;

      package DeclarePermutationsTypes is new Generators (
         Element_Type => BIJECTION,
         Context_Type => VOID
      );
      use DeclarePermutationsTypes;

      type GENERATOR_PROXY is access all GENERATOR_INTERFACE'Class;

      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      Shared_Generator : GENERATOR_PROXY := NULL;

      procedure Goal
        (solution : BIJECTION)
      with Inline,
           Pre => Shared_Generator /= NULL
      is
      begin
         Shared_Generator.Yield(solution);
      end Goal;

      Used_Items : array (INDICES) of BOOLEAN := (others => FALSE);

      function Rejected
        (candidate : BIJECTION;
         index     : INDICES;
         item      : INDICES) return BOOLEAN
      with Inline,
           Pre => index > INDICES'First
      is
      begin
         return Used_Items(item);
      end;

      procedure Enter
        (candidate : BIJECTION;
         index     : INDICES;
         item      : INDICES)
      with Inline,
           Pre  => not Used_Items(item),
           Post => Used_Items(item)
      is
      begin
         Used_Items(item) := TRUE;
      end;

      procedure Leave
        (candidate : BIJECTION;
         index     : INDICES;
         item      : INDICES)
      with Inline,
           Pre  => Used_Items(item),
           Post => not Used_Items(item)
      is
      begin
         Used_Items(item) := FALSE;
      end;

      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      procedure Generate
        (generator : in out GENERATOR_INTERFACE'Class;
         context   : access VOID)
      with Pre  => Shared_Generator = NULL,
           Post => Shared_Generator = NULL
      is
         package permuter is
            new Generics.Depth_First_Search (
               INDEX_TYPE   => INDICES,
               ELEMENT_TYPE => INDICES,
               ARRAY_TYPE   => BIJECTION
            );
      begin
         Shared_Generator := generator'Unchecked_Access;
         permuter.Seek;
         Shared_Generator := NULL;
      end Generate;

      subtype ITERABLE is GENERATOR_TYPE (Generate'Access, NULL);

   begin
      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      declare
         use Ada.Text_IO;

         permutations : ITERABLE;
      begin

      each_permutation:
         for permutation of permutations loop

         each_item:
            for item of permutation loop
               Put(' ');
               Put(Set(POSITIVE(item)));
            end loop each_item;

            New_Line;
         end loop each_permutation;

      end;
   end Permute_Set;

begin
   -- main
   Permute_Set("ABC");

   -- Output for RGB:
   --    R G B
   --    R B G
   --    G R B
   --    G B R
   --    B R G
   --    B G R

end Permute;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
