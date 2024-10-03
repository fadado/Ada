-- control-cancel.adb

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Exceptions;
with Ada.Task_Identification;
with Signals;

use Ada.Exceptions;
use Ada.Task_Identification;

separate(control)
procedure Cancel(self: in out BASE_CONTROLLER; X: Ada.Exceptions.EXCEPTION_OCCURRENCE)
is
   invoker : BASE_CONTROLLER renames BASE_CONTROLLER(self.invoker.all);

   function is_head(co: in out BASE_CONTROLLER'Class) return BOOLEAN with Inline is
      type PTR is not null access all BASE_CONTROLLER'Class;
   begin
      return PTR'(co.invoker) = PTR'(co'Unchecked_Access);
   end is_head;

begin
   if self.id = Null_Task_Id then
      pragma Assert(self.invoker = NULL);
   else
      pragma Assert(self.invoker /= NULL);
      self.id := Null_Task_Id;

      if is_head(self) then
         self.invoker := NULL;
      else
         self.invoker := NULL;
         Signals.Notify(invoker.flag);
      end if;
   end if;

   -- migrate exception
   self.invoker.migrant := Save_Occurrence(X);
end Cancel;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
