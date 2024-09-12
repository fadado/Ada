-- test_hello.adb

pragma Restrictions (
   No_Select_Statements,
   No_Task_Allocators,
   No_Protected_Type_Allocators,
   No_Requeue_Statements,
   No_Local_Protected_Objects,
   No_Abort_Statements
);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

procedure test_hello is

   procedure report_exception(X: EXCEPTION_OCCURRENCE; S: STRING) is
      message : STRING := Exception_Message(X);
   begin
      Put_Line(Standard_Error, S);
      Put_Line(Standard_Error, Exception_Name(X));
      Put_Line(Standard_Error, Exception_Information(X));
      if message /= "" then
         Put_Line(Standard_Error, message);
      end if;
   end report_exception;

begin
   ---------------------------------------------------------------------
   -- Test 1 - Simple hello world
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);

      task body HELLO_RUN is
      begin
         self.Attach;

         Put_Line("Test 1-Hello, world!");

         self.Detach;
      end HELLO_RUN;

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      main.Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- Test 2 - Asymmetric hello world
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);

      task body HELLO_RUN is
      begin
         self.Attach;

         Put("Test 2-"); self.Yield;
         Put("Hello");   self.Yield;
         Put(", world"); self.Yield;
         Put_Line("!");

         self.Detach;
      end HELLO_RUN;

      main : ASYMMETRIC_CONTROLLER;
      hello_control : aliased ASYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);
   begin
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- Test 3 - Symmetric hello world
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self, invoker: not null access SYMMETRIC_CONTROLLER);

      task body HELLO_RUN is
      begin
         self.Attach;

         Put("Test 3-"); self.Resume(invoker);
         Put("Hello");   self.Resume(invoker);
         Put(", world"); self.Resume(invoker);
         Put_Line("!");

         self.Detach;
      end HELLO_RUN;

      main : aliased SYMMETRIC_CONTROLLER;
      hello_control : aliased SYMMETRIC_CONTROLLER;
      hello_runner  : HELLO_RUN (hello_control'Unchecked_Access, main'Unchecked_Access);
   begin
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
      main.Resume(hello_control);
   end;

   ---------------------------------------------------------------------
   -- Test 4 - Mixin inheritance
   ---------------------------------------------------------------------
   declare
      task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER'Class);

      task body HELLO_RUN is
      begin
         self.Attach;

         Put_Line("Test 4-Hello, world!");

         self.Detach;
      end HELLO_RUN;

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   end;

   ---------------------------------------------------------------------
   -- Test 5 - Mixin inheritance
   ---------------------------------------------------------------------
   declare
      type HELLO_COROUTINE is tagged;

      task type HELLO_RUN (self: not null access HELLO_COROUTINE);

      type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
         record
            run : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
         end record;

      task body HELLO_RUN is
      begin
         self.Attach;

         Put_Line("Test 5-Hello, world!");

         self.Detach;
      end HELLO_RUN;

      main  : ASYMMETRIC_CONTROLLER;
      hello : HELLO_COROUTINE;
   begin
      main.Resume(ASYMMETRIC_CONTROLLER(hello));
   end;

   ---------------------------------------------------------------------
   -- Test 6 - Mixin inheritance
   ---------------------------------------------------------------------
   declare
      package Hello_Application is
         type HELLO_COROUTINE is tagged limited private;

         procedure Start(self: in out HELLO_COROUTINE);

         task type HELLO_RUN (self: not null access HELLO_COROUTINE);
      private
         type HELLO_COROUTINE is limited new ASYMMETRIC_CONTROLLER with
            record
               run  : HELLO_RUN (HELLO_COROUTINE'Unchecked_Access);
            end record;
      end Hello_Application;

      package body Hello_Application is
         procedure Start(self: in out HELLO_COROUTINE) is
            main : ASYMMETRIC_CONTROLLER;
         begin
            main.Resume(ASYMMETRIC_CONTROLLER(self));
         end Start;

         task body HELLO_RUN is
         begin
            self.Attach;

            Put_Line("Test 6-Hello, world!");

            self.Detach;
         end HELLO_RUN;
      end Hello_Application;

      use Hello_Application;

      hello : HELLO_COROUTINE;
   begin
      hello.Start;
   end;

-- ---------------------------------------------------------------------
-- -- Test 7 - generator prototype (TODO)
-- ---------------------------------------------------------------------
-- declare
--    message : STRING := "Test 7-Hello, world!";
--    value   : CHARACTER;

--    ------------------------------------------------------------------
--    -- Generator
--    ------------------------------------------------------------------

--    subtype GENERATOR    is STRING;
--    subtype ELEMENT_TYPE is CHARACTER;
--    subtype CURSOR       is NATURAL;

--    No_Element : constant CURSOR := 0;

--    function First(G: GENERATOR) return CURSOR is
--    begin
--       if G'Length > 0 then
--          value := message(1);
--          return 1;
--       else
--          return No_Element;
--       end if;
--    end First;

--    function Next(C: CURSOR) return CURSOR is
--    begin 
--       if C < message'Length then
--          value := message(C + 1);
--          return C + 1;
--       else
--          return No_Element;
--       end if;
--    end Next;

--    function Element(C: CURSOR) return ELEMENT_TYPE is
--    begin
--       pragma Assert(C /= No_Element);
--       return value;
--    end Element;

--    function Has_Element(C: CURSOR) return BOOLEAN is
--       (C /= No_Element);

--    procedure Iterate(G: GENERATOR; P: not null access procedure(C: CURSOR)) is
--       C : CURSOR;
--    begin
--       C := First(G);
--       loop
--          exit when C = No_Element;
--          P(C);
--          C := Next(C);
--       end loop;
--    end Iterate;

--    ------------------------------------------------------------------
--    --
--    ------------------------------------------------------------------

--    task type HELLO_RUN (self: not null access ASYMMETRIC_CONTROLLER);

--    task body HELLO_RUN is
--       len : POSITIVE := message'Length;
--    begin
--       self.Attach;

--       for i in 1..len loop
--          value := message(i);
--          if i < len then
--             self.Yield;
--          else
--             self.Detach;
--          end if;
--       end loop;

--    exception
--       when X: others =>
--          report_exception(X, "Oops at HELLO_RUN! Use ^C to kill me!");
--    end HELLO_RUN;

--    main : ASYMMETRIC_CONTROLLER;
--    hello_control : aliased ASYMMETRIC_CONTROLLER;
--    hello_runner  : HELLO_RUN (hello_control'Unchecked_Access);

--    procedure put_char(C: CURSOR) is
--    begin
--       Put(Element(C));
--    end put_char;

-- begin
--    Iterate(message, put_char'Access);

--    New_Line;

-- exception
--    when X: others =>
--       report_exception(X, "Oops at MAIN TASK! Use ^C to kill me!");
-- end;

end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
