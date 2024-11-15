------------------------------------------------------------------------------
--  Generic Control . Generators (implementation)
------------------------------------------------------------------------------

with Control.Spin_Until;

package body Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods
   ---------------------------------------------------------------------------

   -- type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS)...

   ------------
   -- Resume --
   ------------

   procedure Resume(self: in out GENERATOR_TYPE; value: out ELEMENT_TYPE) is
   begin
      pragma Assert(self.runner'Callable);

      self.head.Transfer(ASYMMETRIC_CONTROLLER(self));

      if self.state = DEAD then
         raise Co_Op.Stop_Iterator;
      end if;

      value := self.value;
   end Resume;

   -----------
   -- Yield --
   -----------

   procedure Yield(self: in out GENERATOR_TYPE; value: ELEMENT_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);
   begin
      pragma Assert(self.runner'Callable);

      self.value := value;
      super.Suspend;
   end Yield;

   -----------
   -- Close --
   -----------

   procedure Close(self: in out GENERATOR_TYPE) is
      super : ASYMMETRIC_CONTROLLER renames ASYMMETRIC_CONTROLLER(self);

      function runner_terminated return BOOLEAN is
         (self.runner'Terminated);
   begin
      super.Request_To_Exit;

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
   --  Wrapper for program with optional context
   ---------------------------------------------------------------------------

   ----------
   -- Wrap --
   ----------

   -- generic
   --    Main    : PROGRAM_ACCESS;
   --    Context : CONTEXT_ACCESS := NULL;

   package body Wrap is
      generator : GENERATOR_TYPE (Main, Context);

      procedure Call(value: out ELEMENT_TYPE) is
         function runner_terminated return BOOLEAN is
            (generator.runner'Terminated);
      begin
         generator.Resume(value);

      exception
      --  Exceptions raised again and propagated to the caller after cleanup
         when Co_Op.Stop_Iterator =>
            Spin_Until(runner_terminated'Access);
            raise;
         when others =>
            generator.Close; -- Just in case...
            raise;
      end Call;
   end Wrap;

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods
   ---------------------------------------------------------------------------

   -- type CURSOR_TYPE is new GENERATOR_ACCESS;

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

   ----------
   -- Next --
   ----------

   function Next(cursor: CURSOR_TYPE) return CURSOR_TYPE is
      generator : GENERATOR_TYPE renames cursor.source.all;
   begin
      if cursor /= No_Element then
         begin
            generator.Resume(generator.value);
         exception
            when Co_Op.Stop_Iterator => return No_Element;
         end;
      end if;
      return cursor;
   end Next;

   procedure Next(cursor: in out CURSOR_TYPE) is
   begin
      cursor := Next(Cursor); -- call the function
   end Next;

   -----------
   -- First --
   -----------

   function First(generator: in out GENERATOR_TYPE) return CURSOR_TYPE
   is
   begin
      if generator.state = EXPECTANT then
         delay 0.1; -- give an oportunity to attach
         if generator.state = EXPECTANT then
            raise Constraint_Error
               with "Generator must be elaborated in a outer frame";
         end if;
      end if;
      generator.Resume(generator.value);
      return (source => generator'Unchecked_Access);
   exception
      when Co_Op.Stop_Iterator => return No_Element;
   end First;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (
      generator : in out GENERATOR_TYPE;
      process   : not null access procedure (value: ELEMENT_TYPE))
   is
      cursor : CURSOR_TYPE;
   begin
      cursor := First(generator);
      loop
         exit when not Has_Element(cursor);
         process(generator.value); -- hack: bypass Element(cursor)
         Next(cursor);
      end loop;
   end Iterate;

   procedure Iterate (
      generator : in out GENERATOR_TYPE;
      process   : not null access procedure (cursor: CURSOR_TYPE))
   is
      cursor : CURSOR_TYPE;
   begin
      cursor := First(generator);
      loop
         exit when not Has_Element(cursor);
         process(cursor);
         Next(cursor);
      end loop;
   end Iterate;

   ---------------------------------------------------------------------------
   --  ITERATOR_TYPE methods
   ---------------------------------------------------------------------------

   type ITERATOR_TYPE is
      new Generator_Iterator_Interfaces.Forward_Iterator with
         record
            source : not null GENERATOR_ACCESS;
         end record;

   overriding
   function First(iterator: ITERATOR_TYPE) return CURSOR_TYPE;

   overriding
   function Next(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return CURSOR_TYPE;

   -------------
   -- Iterate --
   -------------

   function Iterate(generator: in out GENERATOR_TYPE) return ITERABLE is
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
         pragma Assert(GENERATOR_ACCESS(iterator.source)
                     = GENERATOR_ACCESS(cursor.source));
      end return;
   end First;

   ----------
   -- Next --
   ----------

   function Next(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return CURSOR_TYPE is
   begin
      pragma Assert(GENERATOR_ACCESS(iterator.source)
                  = GENERATOR_ACCESS(cursor.source));
      return Next(cursor);
   end Next;

   -------------------
   -- element_value --
   -------------------

   -- TODO aspect
   function element_value(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return ELEMENT_TYPE is
   begin
      pragma Assert(GENERATOR_ACCESS(iterator.source)
                  = GENERATOR_ACCESS(cursor.source));
      return Element(cursor);
   end element_value;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
