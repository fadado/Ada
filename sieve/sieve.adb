-- sieve.adb
-- require -gnat2020

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;

use Ada;

procedure sieve is
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   subtype NUMBER is INTEGER range 1 .. INTEGER'Last;

   Close_Filter : constant NUMBER := 1;
   Queue_Size   : constant := 2;

   package SQI is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );
   package BSQ is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => Queue_Size
      );

   subtype QUEUE is BSQ.Queue;
   type access_QUEUE is access QUEUE;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Odds_Generator (
      Limit        : NUMBER;
      Output_Queue : access_QUEUE
   );
   type access_GENERATOR is access Odds_Generator;

   task body Odds_Generator is
      candidate : NUMBER := 3; -- first odd prime
   begin
      while candidate <= Limit loop
         Output_Queue.Enqueue(candidate);
         candidate := @ + 2;
      end loop;
      Output_Queue.Enqueue(Close_Filter);
   end Odds_Generator;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Prime_Filter(
      Input_Queue  : access_QUEUE;
      Output_Queue : access_QUEUE;
      Prime        : NUMBER
   );
   type access_FILTER is access Prime_Filter;

   task body Prime_Filter is
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
   end Prime_Filter;

   ------------------------------------------------------------
   --
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
   --
   ------------------------------------------------------------
   procedure Main(Limit: NUMBER) is
      prime         : NUMBER;
      input, output : access_QUEUE;
      odds          : access_GENERATOR;
      layer         : access_FILTER;
   begin
      input := new QUEUE;
      odds  := new Odds_Generator (Limit, input);

      Print(2);
      loop
         input.Dequeue(prime);
         exit when prime = Close_Filter;

         Print(prime);

         output := new QUEUE;
         layer  := new Prime_Filter (input, output, prime);
         input  := output;
      end loop;
      Print;
   end Main;

------------------------------------------------------------------------
--
------------------------------------------------------------------------
begin
   declare
      use Command_Line;
      use IO_Exceptions;

      limit : NUMBER;
      last  : POSITIVE;
   begin
      if Argument_Count = 0 then
         limit := 541; -- first 100 primes
      else
         begin
            Integer_Text_IO.Get(Argument(1), limit, last);
            if Limit = 1 then return; end if;
         exception
            when Data_Error | Constraint_Error
               => return;
         end;
      end if;

      Main(Limit => limit);
   end;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
