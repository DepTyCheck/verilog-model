-- Seed: 13192656076216823694,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nfom is
  port ( vircm : inout record_value_mirror
  ; wgfv : inout file_value_mirror
  ; vwkysegpj : buffer boolean_vector(3 to 0)
  ; xaciohxl : inout std_logic_vector(3 to 0)
  );
end nfom;

architecture rm of nfom is
  
begin
  -- Single-driven assignments
  vwkysegpj <= vwkysegpj;
end rm;

use std.reflection.all;

entity v is
  port (gguwenog : inout integer_value_mirror; ysnrgnhubx : buffer integer; z : inout subtype_mirror; eak : inout floating_subtype_mirror);
end v;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture htrj of v is
  signal qf : std_logic_vector(3 to 0);
  signal jhg : boolean_vector(3 to 0);
  shared variable p : file_value_mirror;
  shared variable jxjybmne : record_value_mirror;
  signal cfojtzjdee : std_logic_vector(3 to 0);
  signal ewd : boolean_vector(3 to 0);
  shared variable y : file_value_mirror;
  shared variable cibtcxd : record_value_mirror;
  signal uy : std_logic_vector(3 to 0);
  signal bhnlnxrwu : boolean_vector(3 to 0);
  shared variable jeed : file_value_mirror;
  shared variable vryfaeu : record_value_mirror;
begin
  eclgwrkx : entity work.nfom
    port map (vircm => vryfaeu, wgfv => jeed, vwkysegpj => bhnlnxrwu, xaciohxl => uy);
  wkgz : entity work.nfom
    port map (vircm => cibtcxd, wgfv => y, vwkysegpj => ewd, xaciohxl => cfojtzjdee);
  nfv : entity work.nfom
    port map (vircm => jxjybmne, wgfv => p, vwkysegpj => jhg, xaciohxl => qf);
  
  -- Single-driven assignments
  ysnrgnhubx <= ysnrgnhubx;
end htrj;

library ieee;
use ieee.std_logic_1164.all;

entity fvebfueszn is
  port (ywdq : buffer std_logic);
end fvebfueszn;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mqzmqog of fvebfueszn is
  signal nuxoorgnnh : boolean_vector(3 to 0);
  shared variable lv : file_value_mirror;
  shared variable tjn : record_value_mirror;
  signal ywpexeba : std_logic_vector(3 to 0);
  signal wdslvxmxw : boolean_vector(3 to 0);
  shared variable nfzgnel : file_value_mirror;
  shared variable hm : record_value_mirror;
begin
  ld : entity work.nfom
    port map (vircm => hm, wgfv => nfzgnel, vwkysegpj => wdslvxmxw, xaciohxl => ywpexeba);
  pcs : entity work.nfom
    port map (vircm => tjn, wgfv => lv, vwkysegpj => nuxoorgnnh, xaciohxl => ywpexeba);
  
  -- Multi-driven assignments
  ywdq <= 'U';
  ywdq <= ywdq;
  ywdq <= 'Z';
end mqzmqog;

use std.reflection.all;

entity dcblukfpbm is
  port (dbztseximh : inout physical_subtype_mirror; whqglixng : inout value_mirror; iono : buffer real; uhtdgq : inout access_subtype_mirror);
end dcblukfpbm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture otgnr of dcblukfpbm is
  signal ug : boolean_vector(3 to 0);
  shared variable rdeyccnco : file_value_mirror;
  shared variable zpxxdvmi : record_value_mirror;
  signal hrtpyvggpm : boolean_vector(3 to 0);
  shared variable nkvju : file_value_mirror;
  shared variable dlmhpa : record_value_mirror;
  signal da : std_logic_vector(3 to 0);
  signal ue : boolean_vector(3 to 0);
  shared variable kadxfe : file_value_mirror;
  shared variable tjxugmrrlu : record_value_mirror;
begin
  brnqq : entity work.nfom
    port map (vircm => tjxugmrrlu, wgfv => kadxfe, vwkysegpj => ue, xaciohxl => da);
  lycvkqrtqm : entity work.nfom
    port map (vircm => dlmhpa, wgfv => nkvju, vwkysegpj => hrtpyvggpm, xaciohxl => da);
  fnwmfxw : entity work.nfom
    port map (vircm => zpxxdvmi, wgfv => rdeyccnco, vwkysegpj => ug, xaciohxl => da);
  
  -- Multi-driven assignments
  da <= (others => '0');
end otgnr;



-- Seed after: 5932665977423493668,7726014785203345639
