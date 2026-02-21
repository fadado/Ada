pragma Assertion_Policy(Check); -- Check / Ignore

generic
   with package Instance is new Tuple_Signature (<>);
   use Instance;
   with function "="  (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   with function "<"  (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   with function ">"  (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   with function "<=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;
   with function ">=" (a, b: ELEMENT_TYPE) return BOOLEAN is <>;

package Generics . Tuples . Ordered is

   function Is_Sorted
     (t : in ARRAY_TYPE) return BOOLEAN;

   procedure Sort_It
     (t : in out ARRAY_TYPE)
   with Post => Is_Sorted(t);

   function Sorted is
      new Tuples.Functional (Instance, Sort_It);

   function Member
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return BOOLEAN
   with Pre => Is_Sorted(t);

   function Search
     (x : in ELEMENT_TYPE;
      t : in ARRAY_TYPE) return INDEX_TYPE
   with Pre => Is_Sorted(t);

   function Contains_Duplicates
     (t : in ARRAY_TYPE) return BOOLEAN
   with Pre => Is_Sorted(t);

   function Remove_Duplicates
     (t : in ARRAY_TYPE) return ARRAY_TYPE
   with Inline,
        Pre  => Is_Sorted(t),
        Post => not Contains_Duplicates(Remove_Duplicates'Result);

end Generics . Tuples . Ordered;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
