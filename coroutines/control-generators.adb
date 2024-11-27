------------------------------------------------------------------------------
--  Control . Generators implementation (generic)
------------------------------------------------------------------------------

with Control.Spin_Until;

package body Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   -- type ROUTINE_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is ...

   ------------
   -- Resume --
   ------------

   function Resume(self: in out GENERATOR_TYPE) return ELEMENT_TYPE is
   begin
      pragma Assert(self.runner'Callable);

      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      if self.state = DEAD then
         raise Stop_Iterator;
      end if;

      return self.value;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out GENERATOR_TYPE; value: ELEMENT_TYPE) is
   begin
      pragma Assert(self.runner'Callable);

      self.value := value;
      self.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out GENERATOR_TYPE)
   is
      function runner_terminated return BOOLEAN is
         (self.runner'Terminated);
   begin
      self.Request_To_Exit;

      pragma Assert(self.state = DEAD);

      Spin_Until(runner_terminated'Access);
   end Close;

   ----------------
   -- Run_Method --
   ----------------

   -- task type Run_Method (self: not null GENERATOR_ACCESS);

   task body Run_Method is
   begin
      self.Attach;
      self.main(self);
      self.Detach;
   exception
      when Exit_Controller => self.Die;
      when X: others       => self.Detach(X);
   end Run_Method;

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods
   ---------------------------------------------------------------------------

   -----------
   -- First --
   -----------

   function First(generator: in out GENERATOR_TYPE) return CURSOR_TYPE is
   begin
      if generator.state = EXPECTANT then
         delay 0.01; -- give an oportunity to attach
         if generator.state = EXPECTANT then
            raise Constraint_Error
               with "Generator must be elaborated in a outer frame";
         end if;
      end if;
      generator.value := generator.Resume;
      return (source => generator'Unchecked_Access);
   exception
      when Stop_Iterator => return No_Element;
   end First;

   ----------
   -- Next --
   ----------

   function Next(cursor: CURSOR_TYPE) return CURSOR_TYPE is
      generator : GENERATOR_TYPE renames cursor.source.all;
   begin
      if cursor /= No_Element then
         begin
         generator.value := generator.Resume;
         exception
            when Stop_Iterator => return No_Element;
         end;
      end if;
      return cursor;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element(cursor: CURSOR_TYPE) return BOOLEAN is
   begin
      return cursor /= No_Element;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element(cursor: CURSOR_TYPE) return ELEMENT_TYPE is
      generator : GENERATOR_TYPE renames cursor.source.all;
   begin
      if cursor = No_Element then
         raise Constraint_Error with "Cursor has no element";
      end if;
      return generator.value;
   end Element;

   ----------------
   -- Element_CI --
   ----------------

   function Element_CI(generator: in out GENERATOR_TYPE; cursor: CURSOR_TYPE)
      return ELEMENT_TYPE is
   begin
      pragma Assert(generator'Unchecked_Access = cursor.source);
      return Element(cursor);
   end Element_CI;

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
         callback(generator.value); -- hack: bypass Element(cursor)
         cursor := Next(cursor);
      end loop;
   end For_Each;

   ---------------------------------------------------------------------------
   --  ITERATOR_TYPE methods
   ---------------------------------------------------------------------------

   type ITERATOR_TYPE is 
      limited new Generator_Iterator_Interfaces.Forward_Iterator with
      record
         source : not null GENERATOR_ACCESS;
      end record;

   overriding function First(iterator: ITERATOR_TYPE)
      return CURSOR_TYPE with Inline;

   overriding function Next(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return CURSOR_TYPE with Inline;

   -------------
   -- Iterate --
   -------------

   function Iterate(generator: in out GENERATOR_TYPE)
      return ITERABLE_TYPE is
   begin
      return ITERATOR_TYPE'(source => generator'Unchecked_Access);
   end Iterate;

   -----------
   -- First --
   -----------

   function First(iterator: ITERATOR_TYPE) return CURSOR_TYPE is
      generator : GENERATOR_TYPE renames iterator.source.all;
   begin
      return cursor : constant CURSOR_TYPE := generator.First do
         pragma Assert(iterator.source = cursor.source);
      end return;
   end First;

   ----------
   -- Next --
   ----------

   function Next(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return CURSOR_TYPE is
   begin
      pragma Assert(iterator.source = cursor.source);
      return Next(cursor);
   end Next;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
