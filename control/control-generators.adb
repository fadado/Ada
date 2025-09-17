------------------------------------------------------------------------------
--  Control . Generators implementation (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Spin_Until;

package body Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   ------------
   -- Resume --
   ------------

   function Resume
     (generator : in out GENERATOR_TYPE) return OUTPUT_TYPE
   is
   begin
      if generator.state = DEAD then
         raise Stop_Iteration;
      end if;

      generator.dispatcher.Resume(generator);

      if generator.state = DEAD then
         raise Stop_Iteration;
      end if;

      return generator.output;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (generator : in out GENERATOR_TYPE;
      value     : in OUTPUT_TYPE)
   is
      controller : CONTROLLER_TYPE renames CONTROLLER_TYPE(generator);
   begin
      generator.output := value;
      controller.Yield;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close
     (generator : in out GENERATOR_TYPE)
   is
      function runner_terminated return BOOLEAN
         is (generator.runner'Terminated);
   begin
      if generator.state /= DEAD then
         generator.Request_To_Exit;
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
      generator.main(generator);
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
     (cursor : in CURSOR_TYPE) return OUTPUT_TYPE
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
      callback  : not null access procedure (value: OUTPUT_TYPE))
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

   ---------------------------------------------------------------------------
   --  ITERATOR_TYPE
   ---------------------------------------------------------------------------

   type ITERATOR_TYPE is limited new GII.Forward_Iterator with
      record
         source : not null GENERATOR_ACCESS;
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
     (generator : in out GENERATOR_TYPE) return GII.Forward_Iterator'Class
   is
   begin
      return ITERATOR_TYPE'(source => generator'Unchecked_Access);
   end Iterate;

   -------------------
   -- Generator_C_I --
   -------------------

   function Generator_C_I
     (g : in out GENERATOR_TYPE'Class;
      c : in CURSOR_TYPE) return OUTPUT_TYPE
   is
   begin
      pragma Assert (GENERATOR_TYPE(g)'Unchecked_Access = c.source);
      return Element(c);
   end Generator_C_I;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
