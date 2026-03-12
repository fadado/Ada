-- Generates permutations

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Text_IO;

with Control.Generators;
with Generics.Depth_First_Search;

procedure Permute
is
   generic
      Objects  : in STRING;   -- make generic as tuples signature
      N        : in NATURAL := 0;
      Repeated : in BOOLEAN := FALSE;
   package Sequences
   is
      use Control;

      Bits : constant := 8;

      pragma Assert(N <= Objects'Length);
      pragma Assert(Objects'First >= 0);
      pragma Assert(Objects'Last  <= 2**Bits-1);

      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      type TINY is range 0..2**Bits-1;

      First : constant TINY := TINY(Objects'First);
      Last  : constant TINY := TINY(Objects'Last);

      subtype VALUES is TINY range First..Last;

      subtype INDICES is TINY
         range First..(if N = 0 then Last else First+TINY(N)-1);

      type SOLUTION is array (INDICES) of VALUES
      with Pack,
           Component_Size => Bits;

      package DeclarePermutationsTypes is new Generators (
         Element_Type => SOLUTION,
         Context_Type => VOID
      );
      use DeclarePermutationsTypes;

      procedure Generate
        (generator : in out GENERATOR_INTERFACE'Class;
         context   : access VOID);

      subtype ITERABLE is GENERATOR_TYPE (Generate'Access, NULL);
      -- the only exported name!

   end Sequences;

   package body Sequences
   is
      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      type GENERATOR_PROXY is access all GENERATOR_INTERFACE'Class;

      Shared_Generator : GENERATOR_PROXY := NULL;

      procedure Goal
        (vector : SOLUTION)
      with Inline,
           Pre => Shared_Generator /= NULL
      is
      begin
         Shared_Generator.Yield(vector);
      end Goal;

      Used_Items : array (VALUES) of BOOLEAN := (others => FALSE);

      function Rejected
        (candidate : SOLUTION;
         index     : INDICES;
         item      : VALUES) return BOOLEAN
      with Inline,
           Pre => index > INDICES'First
      is
      begin
         return not Repeated and then Used_Items(item);
      end;

      procedure Enter
        (candidate : SOLUTION;
         index     : INDICES;
         item      : VALUES)
      with Inline,
           Pre  => Repeated or else not Used_Items(item),
           Post => Repeated or else Used_Items(item)
      is
      begin
         if not Repeated then Used_Items(item) := TRUE; end if;
      end;

      procedure Leave
        (candidate : SOLUTION;
         index     : INDICES;
         item      : VALUES)
      with Inline,
           Pre  => Repeated or else Used_Items(item),
           Post => Repeated or else not Used_Items(item)
      is
      begin
         if not Repeated then Used_Items(item) := FALSE; end if;
      end;

      ------------------------------------------------------------------
      --
      ------------------------------------------------------------------

      procedure Generate
        (generator : in out GENERATOR_INTERFACE'Class;
         context   : access VOID)
      is
         package permuter is
            new Generics.Depth_First_Search (
               INDEX_TYPE   => INDICES,
               ELEMENT_TYPE => VALUES,
               ARRAY_TYPE   => SOLUTION
            );
      begin
         pragma Assert(Shared_Generator = NULL);

         Shared_Generator := generator'Unchecked_Access;
         permuter.Seek;
         Shared_Generator := NULL;
      end Generate;

   end Sequences;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

main:
   declare
      use Ada.Text_IO;

      str : constant STRING := "ABC";

      package P is new Sequences (
         Objects  => str,
         N        => 0, -- = Objects'Length
         Repeated => FALSE);

      permutations : P.ITERABLE;
   begin

   each_permutation:
      for permutation of permutations loop

      each_index:
         for index of permutation loop
            Put(' ');
            Put(str(POSITIVE(index)));
         end loop each_index;

         New_Line;
      end loop each_permutation;

   end main;

   -- Output for "RGB":
   --    R G B
   --    R B G
   --    G R B
   --    G B R
   --    B R G
   --    B G R

end Permute;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
