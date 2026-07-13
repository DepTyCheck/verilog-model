-- Seed: 2608062127192058975,3566912872917928779

use std.reflection.all;

entity lbvktmuf is
  port (syw : inout file_subtype_mirror; sljdfznpj : inout record_subtype_mirror; t : inout floating_value_mirror);
end lbvktmuf;

architecture at of lbvktmuf is
  
begin
  
end at;

use std.reflection.all;

entity ngdcygcv is
  port (xtcixvfygp : inout floating_value_mirror; gxepkxi : inout array_value_mirror);
end ngdcygcv;

use std.reflection.all;

architecture s of ngdcygcv is
  shared variable eifrl : record_subtype_mirror;
  shared variable cympv : file_subtype_mirror;
begin
  nbujqxn : entity work.lbvktmuf
    port map (syw => cympv, sljdfznpj => eifrl, t => xtcixvfygp);
end s;

use std.reflection.all;

entity xl is
  port (lziznbi : out time; yz : inout file_value_mirror);
end xl;

use std.reflection.all;

architecture kbkwa of xl is
  shared variable ixnvlbwud : floating_value_mirror;
  shared variable cckngq : record_subtype_mirror;
  shared variable njemswghuc : file_subtype_mirror;
  shared variable eeratsstn : array_value_mirror;
  shared variable wbfkdsyknz : floating_value_mirror;
begin
  nyl : entity work.ngdcygcv
    port map (xtcixvfygp => wbfkdsyknz, gxepkxi => eeratsstn);
  pshql : entity work.lbvktmuf
    port map (syw => njemswghuc, sljdfznpj => cckngq, t => ixnvlbwud);
end kbkwa;



-- Seed after: 13969385840378844154,3566912872917928779
