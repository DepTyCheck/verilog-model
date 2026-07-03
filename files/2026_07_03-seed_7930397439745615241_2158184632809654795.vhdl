-- Seed: 7930397439745615241,2158184632809654795

use std.reflection.all;

entity ojcelzpj is
  port (fdzmbjk : inout enumeration_subtype_mirror);
end ojcelzpj;

architecture hb of ojcelzpj is
  
begin
  
end hb;

use std.reflection.all;

entity ggtpncnm is
  port (th : inout boolean_vector(0 to 3); qcjz : inout physical_subtype_mirror);
end ggtpncnm;

use std.reflection.all;

architecture odqa of ggtpncnm is
  shared variable mfq : enumeration_subtype_mirror;
  shared variable fobmh : enumeration_subtype_mirror;
  shared variable hw : enumeration_subtype_mirror;
begin
  f : entity work.ojcelzpj
    port map (fdzmbjk => hw);
  eemgj : entity work.ojcelzpj
    port map (fdzmbjk => fobmh);
  ixkladr : entity work.ojcelzpj
    port map (fdzmbjk => mfq);
  
  -- Single-driven assignments
  th <= (FALSE, TRUE, TRUE, TRUE);
end odqa;



-- Seed after: 14934717805073297272,2158184632809654795
