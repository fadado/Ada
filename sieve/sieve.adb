-- sieve.adb
-- require -gnat2020

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

use Ada;

procedure sieve is
   ------------------------------------------------------------
   -- Set of numbers to search for primes
   ------------------------------------------------------------

   subtype NUMBER is INTEGER range 1 .. INTEGER'Last;

   Close_Filter : constant NUMBER := 1;
   -- We use 1 as a token to signal task's ending

   ------------------------------------------------------------
   -- Channels between tasks
   ------------------------------------------------------------

   Queue_Size : constant := 2;
   -- A greater size does not improve performance

   package SQI is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => Queue_Size
      );

   subtype monitor_QUEUE is BSQ.Queue;
   type QUEUE is access monitor_QUEUE;

   ------------------------------------------------------------
   -- Task to generate odd numbers starting at 3
   ------------------------------------------------------------

   task type task_GENERATOR (
      Limit        : NUMBER;
      Output_Queue : QUEUE
   );
   type GENERATOR is access task_GENERATOR;

   task body task_GENERATOR is
      candidate : NUMBER := 3;
   begin
      while candidate <= Limit loop
         Output_Queue.Enqueue(candidate);
         candidate := @ + 2;
      end loop;
      Output_Queue.Enqueue(Close_Filter);
   end task_GENERATOR;

   ------------------------------------------------------------
   -- Task to reject (or pass) prime cadidates
   ------------------------------------------------------------

   task type task_FILTER (
      Input_Queue  : QUEUE;
      Output_Queue : QUEUE;
      Prime        : NUMBER
   );
   type FILTER is access task_FILTER;

   task body task_FILTER is
      candidate : NUMBER;
   begin
      loop
         Input_Queue.Dequeue(candidate);
         exit when candidate = Close_Filter;
         if (candidate rem Prime) /= 0 then
            Output_Queue.Enqueue(candidate);
         end if;
      end loop;
      Output_Queue.Enqueue(Close_Filter);
   end task_FILTER;

   ------------------------------------------------------------
   -- Output utilities
   ------------------------------------------------------------

   Count : NATURAL := 0;

   procedure Print with Inline is
      use Text_IO;
   begin
      New_Line;
   end;

   procedure Print(N: NUMBER) with Inline is
      use Text_IO;
      use Integer_Text_IO;
      Field_Size : constant := 7;
      Columns    : constant := 10;
   begin
      Count := @ + 1;
      Put(N, Width => Field_Size);
      if (Count rem Columns) = 0 then
         New_Line;
      end if;
   end;

   ------------------------------------------------------------
   -- Build a chain of filters
   ------------------------------------------------------------

   procedure Build_Sieve(Limit: NUMBER) is
      prime         : NUMBER;
      input, output : QUEUE;
      odds          : GENERATOR;
      layer         : FILTER;
   begin
      input := new monitor_QUEUE;
      odds  := new task_GENERATOR (Limit, input);
      Print(2);
      loop
         input.Dequeue(prime);
         exit when prime = Close_Filter;
         Print(prime);
         output := new monitor_QUEUE;
         layer  := new task_FILTER (input, output, prime);
         input  := output;
      end loop;
      Print;
   end Build_Sieve;

------------------------------------------------------------------------
-- Manage command line and start the sieve
------------------------------------------------------------------------

begin
   declare
      use Command_Line;
      procedure Usage is
         use Text_IO;
      begin
         Put_Line(Standard_Error, "Usage: ./sieve [LIMIT]");
         Put_Line(Standard_Error, "LIMIT must by a number greater than 1");
      end;
      limit : NUMBER;
   begin
      Set_Exit_Status(Failure);
      if Argument_Count = 0 then
         limit := 541; -- first 100 primes
      else
         begin
            limit := NUMBER'Value(Argument(1));
            if limit = 1 then raise Constraint_Error; end if;
         exception
            when Constraint_Error => Usage; return;
         end;
      end if;
      Build_Sieve(Limit => limit);
      Set_Exit_Status(Success);
   end;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
