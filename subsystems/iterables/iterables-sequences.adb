------------------------------------------------------------------------------
--  Iterables . Sequences implementation
------------------------------------------------------------------------------

with Generics.Depth_First_Search;
with Generics.Tuples.Arrayed;

package body Iterables . Sequences
is
   package GTA is
      new Generics.Tuples.Arrayed (TupleInstance);

   pragma Assert (not GTA.Contains_Duplicates(Objects));

   ------------------------------------------------------------------
   --
   ------------------------------------------------------------------

   type GENERATOR_PROXY is access all GENERATOR_INTERFACE'Class;

   Shared_Generator : GENERATOR_PROXY := NULL;

   procedure Goal
     (solution : ORDERING)
   with Inline,
        Pre => Shared_Generator /= NULL
   is
   begin
      Shared_Generator.Yield(solution);
   end Goal;

   Used_Items : array (VALUES) of BOOLEAN := (others => FALSE);

   function Rejected
     (candidate : ORDERING;
      index     : INDICES;
      item      : VALUES) return BOOLEAN
   with Inline,
        Pre => index > INDICES'First
   is
   begin
      return not Repeated and then Used_Items(item);
   end Rejected;

   procedure Enter
     (candidate : ORDERING;
      index     : INDICES;
      item      : VALUES)
   with Inline,
        Pre  => Repeated or else not Used_Items(item),
        Post => Repeated or else Used_Items(item)
   is
   begin
      if not Repeated then Used_Items(item) := TRUE; end if;
   end Enter;

   procedure Leave
     (candidate : ORDERING;
      index     : INDICES;
      item      : VALUES)
   with Inline,
        Pre  => Repeated or else Used_Items(item),
        Post => Repeated or else not Used_Items(item)
   is
   begin
      if not Repeated then Used_Items(item) := FALSE; end if;
   end Leave;

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
            ARRAY_TYPE   => ORDERING
         );
   begin
      pragma Assert (Shared_Generator = NULL);

      Shared_Generator := generator'Unchecked_Access;
      permuter.Seek;
      Shared_Generator := NULL;
   end Generate;

end Iterables . Sequences;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
