-- Seed: 14107045019994804781,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity qgpzdzzjz is
  port ( vkzoadeffj : inout record_value_mirror
  ; rajaavz : inout record_subtype_mirror
  ; rmp : inout std_logic_vector(1 downto 0)
  ; xnypo : inout file_subtype_mirror
  );
end qgpzdzzjz;

architecture hppcqzlj of qgpzdzzjz is
  
begin
  -- Multi-driven assignments
  rmp <= ('1', '1');
  rmp <= ('L', 'U');
  rmp <= ('H', '-');
  rmp <= "WX";
end hppcqzlj;

use std.reflection.all;

entity r is
  port (giam : inout physical_subtype_mirror; uzjf : out boolean);
end r;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture sznawd of r is
  shared variable ierglmpl : file_subtype_mirror;
  signal vxv : std_logic_vector(1 downto 0);
  shared variable nximqs : record_subtype_mirror;
  shared variable qgcpqk : record_value_mirror;
  shared variable tbgxpzf : file_subtype_mirror;
  signal cu : std_logic_vector(1 downto 0);
  shared variable fhqdi : record_subtype_mirror;
  shared variable i : record_value_mirror;
  shared variable qaayb : file_subtype_mirror;
  signal wbs : std_logic_vector(1 downto 0);
  shared variable d : record_subtype_mirror;
  shared variable qw : record_value_mirror;
  shared variable ivqvixxvau : file_subtype_mirror;
  signal hopcingjqx : std_logic_vector(1 downto 0);
  shared variable qppkuxveji : record_subtype_mirror;
  shared variable jgln : record_value_mirror;
begin
  tytoroducs : entity work.qgpzdzzjz
    port map (vkzoadeffj => jgln, rajaavz => qppkuxveji, rmp => hopcingjqx, xnypo => ivqvixxvau);
  mky : entity work.qgpzdzzjz
    port map (vkzoadeffj => qw, rajaavz => d, rmp => wbs, xnypo => qaayb);
  dsratoxz : entity work.qgpzdzzjz
    port map (vkzoadeffj => i, rajaavz => fhqdi, rmp => cu, xnypo => tbgxpzf);
  sgwey : entity work.qgpzdzzjz
    port map (vkzoadeffj => qgcpqk, rajaavz => nximqs, rmp => vxv, xnypo => ierglmpl);
  
  -- Single-driven assignments
  uzjf <= FALSE;
end sznawd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity nwbp is
  port (gvftoqr : out std_logic; ydmr : inout value_mirror; lv : inout file_value_mirror);
end nwbp;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gdnocelpih of nwbp is
  shared variable gvegntaq : file_subtype_mirror;
  shared variable xvwqlavvjd : record_subtype_mirror;
  shared variable nsactgvj : record_value_mirror;
  signal lupsgadsnr : boolean;
  shared variable tqvbi : physical_subtype_mirror;
  shared variable sin : file_subtype_mirror;
  signal xz : std_logic_vector(1 downto 0);
  shared variable uj : record_subtype_mirror;
  shared variable ijfphfs : record_value_mirror;
  signal ggrcs : boolean;
  shared variable ca : physical_subtype_mirror;
begin
  f : entity work.r
    port map (giam => ca, uzjf => ggrcs);
  tdqzfle : entity work.qgpzdzzjz
    port map (vkzoadeffj => ijfphfs, rajaavz => uj, rmp => xz, xnypo => sin);
  ue : entity work.r
    port map (giam => tqvbi, uzjf => lupsgadsnr);
  upwuvhsasl : entity work.qgpzdzzjz
    port map (vkzoadeffj => nsactgvj, rajaavz => xvwqlavvjd, rmp => xz, xnypo => gvegntaq);
  
  -- Multi-driven assignments
  gvftoqr <= 'U';
  gvftoqr <= gvftoqr;
  xz <= "ZH";
end gdnocelpih;



-- Seed after: 13370598158452976705,3181554006726329157
