-- Seed: 13998531107225472161,2158184632809654795

use std.reflection.all;

entity gexsbr is
  port (cgpfyv : inout integer_subtype_mirror);
end gexsbr;

architecture g of gexsbr is
  
begin
  
end g;

use std.reflection.all;

entity oe is
  port (yfbcdc : inout integer_value_mirror);
end oe;

use std.reflection.all;

architecture edjayijjd of oe is
  shared variable tpke : integer_subtype_mirror;
  shared variable tudsnjkh : integer_subtype_mirror;
  shared variable hqzocp : integer_subtype_mirror;
begin
  jxfdzl : entity work.gexsbr
    port map (cgpfyv => hqzocp);
  jkehihqp : entity work.gexsbr
    port map (cgpfyv => tudsnjkh);
  oliymts : entity work.gexsbr
    port map (cgpfyv => tpke);
end edjayijjd;

use std.reflection.all;

entity hgzkae is
  port (yaymlu : inout access_value_mirror; wrjfjsnzbz : inout access_subtype_mirror; hsgk : out time);
end hgzkae;

use std.reflection.all;

architecture wk of hgzkae is
  shared variable dh : integer_subtype_mirror;
  shared variable hn : integer_subtype_mirror;
begin
  cgyw : entity work.gexsbr
    port map (cgpfyv => hn);
  wztkkfzx : entity work.gexsbr
    port map (cgpfyv => dh);
  
  -- Single-driven assignments
  hsgk <= 2#100.0_1_1# fs;
end wk;



-- Seed after: 6598265865245519870,2158184632809654795
