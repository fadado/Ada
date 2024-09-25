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

with Control; use Control;
with Gotcha;

procedure test_hello is

begin
   --Gotcha.Set_Handlers;

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
   -- Test 4 - "Multiple" inheritance
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
   -- Test 5 - "Multiple" inheritance
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
   -- Test 6 - "Multiple" inheritance
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

   ---------------------------------------------------------------------
   -- Test 7 - generator prototype (TODO)
   ---------------------------------------------------------------------
---declare
---   message : STRING := "Test 7-Hello, world!";
---   value   : CHARACTER;

---   ------------------------------------------------------------------
---   -- Generator
---   ------------------------------------------------------------------

---   subtype CURSOR is NATURAL;

---   No_Element : constant CURSOR := 0;

---   function Has_Element(C: CURSOR) return BOOLEAN is
---      (C /= No_Element);

---   function First(G: STRING) return CURSOR is
---   begin
---      if G'Length > 0 then
---         value := message(1);
---         return CURSOR'(1);
---      else
---         return No_Element;
---      end if;
---   end First;

---   function Next(C: CURSOR) return CURSOR is
---   begin 
---      if C = No_Element or C = message'Length then
---         return No_Element;
---      else
---         value := message(C+1);
---         return C+1;
---      end if;
---   end Next;

---   function Element(C: CURSOR) return CHARACTER is
---   begin
---      pragma Assert(C /= No_Element);
---      return message(C);
---   end Element;

---   procedure Iterate(G: STRING; P: not null access procedure(C: CURSOR)) is
---      C : CURSOR;
---   begin
---      C := First(G);
---      loop
---         exit when C = No_Element;
---         P(C);
---         C := Next(C);
---      end loop;
---   end Iterate;

---   ------------------------------------------------------------------
---   --
---   ------------------------------------------------------------------

---   package Hello_Application is
---      type HELLO_GENERATOR is tagged limited private;

---      function First(self: in out HELLO_GENERATOR) return CURSOR;

---      task type HELLO_RUN (self: not null access HELLO_GENERATOR);

---   private
---      type HELLO_GENERATOR is limited new ASYMMETRIC_CONTROLLER with
---         record
---            run   : HELLO_RUN (HELLO_GENERATOR'Unchecked_Access);
---            here  : ASYMMETRIC_CONTROLLER;
---         end record;
---   end Hello_Application;

---   package body Hello_Application is

---      function First(self: in out HELLO_GENERATOR) return CURSOR is
---      begin
---         --??self.here.Resume(ASYMMETRIC_CONTROLLER(self));
---         return First(message);
---      end First;

---      task body HELLO_RUN is
---         len : POSITIVE := message'Length;
---      begin
---         self.Attach;

---         for i in 1..len loop
---            value := message(i);
---            if i < len then
---               self.Yield;
---            else
---               null; --exit;
---            end if;
---         end loop;

---         self.Detach;
---      end HELLO_RUN;
---   end Hello_Application;

---   use Hello_Application;

---begin
---   declare
---      hello : HELLO_GENERATOR;
---      position : CURSOR;
---   begin
---      position := hello.First;
---      loop
---         exit when position = No_Element;
---         Put(Element(position));
---         position := Next(position);
---      end loop;
---   end;
---end;

   --Gotcha.Die;
end test_hello;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
