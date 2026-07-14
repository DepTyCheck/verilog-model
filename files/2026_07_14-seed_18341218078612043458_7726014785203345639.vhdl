-- Seed: 18341218078612043458,7726014785203345639

use std.reflection.all;

entity guzxzsbczg is
  port (knfa : inout protected_subtype_mirror);
end guzxzsbczg;

architecture xyhm of guzxzsbczg is
  
begin
  
end xyhm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity iefczmle is
  port (lsezqrtcdn : in std_logic; uyzgnud : inout physical_value_mirror; zumxarriv : in time; hixfpsw : inout record_value_mirror);
end iefczmle;

use std.reflection.all;

architecture klsxivluj of iefczmle is
  shared variable mcjzxlxm : protected_subtype_mirror;
  shared variable xxren : protected_subtype_mirror;
begin
  lbfmvjod : entity work.guzxzsbczg
    port map (knfa => xxren);
  eajpbs : entity work.guzxzsbczg
    port map (knfa => mcjzxlxm);
end klsxivluj;

use std.reflection.all;

entity mymsd is
  port (qj : inout record_value_mirror);
end mymsd;

use std.reflection.all;

architecture peixv of mymsd is
  shared variable i : protected_subtype_mirror;
  shared variable nivbnn : protected_subtype_mirror;
begin
  bqrukjyowd : entity work.guzxzsbczg
    port map (knfa => nivbnn);
  uid : entity work.guzxzsbczg
    port map (knfa => i);
end peixv;

entity t is
  port (fokksmpwxq : linkage severity_level);
end t;

use std.reflection.all;

architecture nlrzaykgqa of t is
  shared variable pshk : record_value_mirror;
  shared variable gxonqxo : protected_subtype_mirror;
  shared variable ldhrky : record_value_mirror;
begin
  b : entity work.mymsd
    port map (qj => ldhrky);
  vmxvotjoj : entity work.guzxzsbczg
    port map (knfa => gxonqxo);
  ilvyfri : entity work.mymsd
    port map (qj => pshk);
end nlrzaykgqa;



-- Seed after: 7466529574945439482,7726014785203345639
