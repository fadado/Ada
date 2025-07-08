-- sieve.adb

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

   subtype NUMBER is POSITIVE;

   Number_One : constant NUMBER := 1;
   -- We use 1 as a token to signal task's ending

   ------------------------------------------------------------
   -- Channels between tasks
   ------------------------------------------------------------

   Queue_Size : constant := 2;
   -- A greater size does not improve performance

   package Synchronized_Queue_Interface is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );

   package Bounded_Synchronized_Queue is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => Synchronized_Queue_Interface,
         Default_Capacity => Queue_Size
      );

   subtype NUMERIC_CHANNEL is Bounded_Synchronized_Queue.Queue;
   type CHANNEL is access NUMERIC_CHANNEL;

   ------------------------------------------------------------
   -- Task to generate odd numbers starting at 3
   ------------------------------------------------------------

   task type ODD_NUMBERS_GENERATOR (
      Output : CHANNEL;
      Limit  : NUMBER
   );
   type GENERATOR is access ODD_NUMBERS_GENERATOR;

   task body ODD_NUMBERS_GENERATOR is
      odd : NUMBER := 3;
   begin
      while odd <= Limit loop
         Output.Enqueue(odd);
         odd := odd + 2;
      end loop;

      Output.Enqueue(Number_One);
   end ODD_NUMBERS_GENERATOR;

   ------------------------------------------------------------
   -- Task to reject (or pass) prime cadidates
   ------------------------------------------------------------

   task type FILTER_PRIME_MULTIPLES (
      Input  : CHANNEL;
      Output : CHANNEL;
      Prime  : NUMBER
   );
   type FILTER is access FILTER_PRIME_MULTIPLES;

   task body FILTER_PRIME_MULTIPLES is
      function Is_Multiple(n : NUMBER) return BOOLEAN
         is (n rem Prime = 0) with Inline;

      candidate : NUMBER;
   begin
      loop
         Input.Dequeue(candidate);
         exit when candidate = Number_One;

         if Is_Multiple(candidate) then
            null; -- reject candidate
         else
            Output.Enqueue(candidate);
         end if;
      end loop;

      Output.Enqueue(Number_One);
   end FILTER_PRIME_MULTIPLES;

   ------------------------------------------------------------
   -- Build the sieve as a chain of filters
   ------------------------------------------------------------

   task type SIEVE_GENERATOR (
      Output : CHANNEL;
      Limit  : NUMBER
   );

   task body SIEVE_GENERATOR is
      prime       : NUMBER := 2; -- the only even prime
      left, right : CHANNEL;
   begin
      Output.Enqueue(prime);

      right := new NUMERIC_CHANNEL;
      declare
         G : GENERATOR;
      begin
         G := new ODD_NUMBERS_GENERATOR (right, Limit);
      end;

      loop
         left := right;

         left.Dequeue(prime);
         exit when prime = Number_One;
         Output.Enqueue(prime);

         right := new NUMERIC_CHANNEL;
         declare
            F : FILTER;
         begin
            F := new FILTER_PRIME_MULTIPLES (left, right, prime);
         end;
      end loop;

      Output.Enqueue(Number_One);
   end SIEVE_GENERATOR;

   ------------------------------------------------------------
   -- Output utilities
   ------------------------------------------------------------

   Count : NATURAL := 0;

   procedure Print with Inline is
   begin
      Text_IO.New_Line;
   end;

   procedure Print(N: NUMBER) with Inline is
      use Text_IO;
      use Integer_Text_IO;

      Field_Size : constant := 7;
      Columns    : constant := 10;
   begin
      Count := Count + 1;
      Put(N, Width => Field_Size);
      if (Count rem Columns) = 0 then
         New_Line;
      end if;
   end;

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

      M : NUMBER := 541; -- default limit: the first 100 primes
   begin
      Set_Exit_Status(Failure);

      if Argument_Count > 0 then
         begin
            M := NUMBER'Value(Argument(1));
            if M = Number_One then
               raise Constraint_Error;
            end if;
         exception
            when Constraint_Error =>
               Usage;
               return;
         end;
      end if;

      declare
         primes : CHANNEL;
         sieve  : access SIEVE_GENERATOR;
         N      : NUMBER;
      begin
         primes := new NUMERIC_CHANNEL;
         sieve  := new SIEVE_GENERATOR (Output => primes, Limit => M);
         loop
            primes.Dequeue(N);
            exit when N = Number_One;
            Print(N);
         end loop;
         Print;
      end;

      Set_Exit_Status(Success);
   end;
end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
