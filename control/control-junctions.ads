------------------------------------------------------------------------------
--  Junctions specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.Generators;
with Control.Collectors;

package Control . Junctions is

   generic
      type IO_TYPE is private;

      type GENERATOR_CONTEXT (<>) is limited private;

      with package Generator_Package is new Generators (
         Output_Type  => IO_TYPE,
         Context_Type => GENERATOR_CONTEXT
      );

      type COLLECTOR_CONTEXT (<>) is limited private;

      with package Collector_Package is new Collectors (
         Input_Type   => IO_TYPE,
         Context_Type => COLLECTOR_CONTEXT
      );

      use Generator_Package;
      use Collector_Package;

   procedure Junction
      (generator : in out GENERATOR_TYPE;
       collector : in out COLLECTOR_TYPE);

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
