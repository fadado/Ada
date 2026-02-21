pragma Assertion_Policy(Check); -- Check / Ignore

generic
   with package Instance is new Tuple_Signature (<>);
   use Instance;

package Generics . Tuples . Lifted is

   generic
      with package  Target is new Tuple_Signature (<>);
      with function Map (X: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
   function Mapper
     (t : in ARRAY_TYPE) return Target.ARRAY_TYPE;

   generic
      with package  Target is new Tuple_Signature (<>);
      with function Zip (X, Y: in ELEMENT_TYPE) return Target.ELEMENT_TYPE;
   function Zipper
     (s, t : in ARRAY_TYPE) return Target.ARRAY_TYPE
   with Pre => s'Length = t'Length and then s'First = t'First;

   generic
      with function Test (X: in ELEMENT_TYPE) return BOOLEAN;
   function Filter
     (t : in ARRAY_TYPE) return ARRAY_TYPE;

   generic
      with function Operation (L, R: in ELEMENT_TYPE) return ELEMENT_TYPE;
   function Reducer
     (t : in ARRAY_TYPE) return ELEMENT_TYPE
   with Pre => t'Length > 0;

   generic
      with function Better (L, R: in ELEMENT_TYPE) return BOOLEAN;
   function Chooser
     (t : in ARRAY_TYPE) return ELEMENT_TYPE
   with Pre => t'Length > 0;

end Generics . Tuples . Lifted;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
