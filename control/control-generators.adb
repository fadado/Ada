------------------------------------------------------------------------------
--  Control . Generators implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   -----------
   -- Yield --
   -----------

   overriding procedure Yield
     (generator : in out GENERATOR_TYPE;
      value     : in ELEMENT_TYPE)
   is
      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(generator);
   begin
      generator.output := value;
      parent.Yield;
   end Yield;

   ------------
   -- Resume --
   ------------

   not overriding function Resume
     (generator : in out GENERATOR_TYPE) return ELEMENT_TYPE
   is
   begin
      if generator.state = DEAD then
         raise Stop_Iteration;
      end if;

      Dispatch(generator, generator.dispatcher);

      if generator.state = DEAD then
         raise Stop_Iteration;
      end if;

      return generator.output;
   end Resume;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (generator : in out GENERATOR_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (generator.runner'Terminated);

      parent : SEMI_CONTROLLER_TYPE renames SEMI_CONTROLLER_TYPE(generator);
   begin
      if generator.state /= DEAD then
         parent.Close;
         Spin_Until(runner_terminated'Access);
      end if;
   end Close;

   ----------------------
   -- Generator_Runner --
   ----------------------

   task body Generator_Runner
   is
   begin
      generator.Commence;
      generator.main(generator.all, generator.context);
      generator.Quit;
   exception
      when Exit_Controller => null;
      when X: others       => generator.Quit(X);
   end Generator_Runner;

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods
   ---------------------------------------------------------------------------

   -----------
   -- First --
   -----------

   function First
     (generator : in out GENERATOR_TYPE) return CURSOR_TYPE
   is
      function generator_initiated return BOOLEAN
         is (generator.state /= EXPECTANT);
   begin
      if generator.state = EXPECTANT then
         Spin_Until(generator_initiated'Access);
      end if;
      generator.output := generator.Resume;
      return (source => generator'Unchecked_Access);
   exception
      when Stop_Iteration => return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   function Next
     (cursor : in CURSOR_TYPE) return CURSOR_TYPE
   is
      generator : GENERATOR_TYPE renames cursor.source.all;
   begin
      if cursor /= No_Element then
         begin
            generator.output := generator.Resume;
         exception
            when Stop_Iteration => return No_Element;
         end;
      end if;
      return cursor;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (cursor : in CURSOR_TYPE) return BOOLEAN
   is
   begin
      return cursor /= No_Element;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (cursor : in CURSOR_TYPE) return ELEMENT_TYPE
   is
      generator : GENERATOR_TYPE renames cursor.source.all;
   begin
      if cursor = No_Element then
         raise Control_Error with "Cursor has no element";
      end if;
      return generator.output;
   end Element;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (
      generator : in out GENERATOR_TYPE;
      callback  : not null access procedure (value: ELEMENT_TYPE))
   is
      cursor : CURSOR_TYPE;
   begin
      cursor := First(generator);
      loop
         exit when cursor = No_Element;
         callback(generator.output); -- hack: bypass Element(cursor)
         cursor := Next(cursor);
      end loop;
   end For_Each;

   -------------------
   -- Element_Value --
   -------------------

   function Element_Value
     (generator : in out GENERATOR_TYPE;
      cursor    : in CURSOR_TYPE) return ELEMENT_TYPE
   is
      type A is not null access all GENERATOR_TYPE;
   begin
      pragma Assert (GENERATOR_TYPE(generator)'Access = A(cursor.source));
      return Element(cursor);
   end Element_Value;

   ---------------------------------------------------------------------------
   --  ITERATOR_TYPE
   ---------------------------------------------------------------------------

   type ITERATOR_TYPE is limited new ITERATOR_INTERFACE with
      record
         source : not null access GENERATOR_TYPE;
      end record;

   overriding function First
     (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
   with Inline;

   overriding function Next
     (iterator : in ITERATOR_TYPE;
      cursor   : in CURSOR_TYPE) return CURSOR_TYPE
   with Inline;

   -----------
   -- First --
   -----------

   function First
     (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
   is
      generator : GENERATOR_TYPE renames iterator.source.all;
   begin
      return cursor : constant CURSOR_TYPE := generator.First do
         pragma Assert (iterator.source = cursor.source);
      end return;
   end First;

   ----------
   -- Next --
   ----------

   function Next
     (iterator : in ITERATOR_TYPE;
      cursor   : in CURSOR_TYPE) return CURSOR_TYPE
   is
   begin
      pragma Assert (iterator.source = cursor.source);
      return Next(cursor);
   end Next;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (generator : in out GENERATOR_TYPE) return ITERATOR_INTERFACE'Class
   is
   begin
      return ITERATOR_TYPE'(source => generator'Unchecked_Access);
   end Iterate;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
