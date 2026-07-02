-- Seed: 12362103123207117279,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vbum is
  port (llb : inout subtype_mirror; xnzzz : inout physical_value_mirror; duwo : inout protected_value_mirror; sej : buffer std_logic);
end vbum;

architecture dkgya of vbum is
  
begin
  -- Multi-driven assignments
  sej <= sej;
  sej <= '1';
end dkgya;

use std.reflection.all;

entity ndxzejnd is
  port (onuaikpsoz : inout array_value_mirror; trdzzleh : inout array_value_mirror);
end ndxzejnd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture um of ndxzejnd is
  signal polmunkw : std_logic;
  shared variable ntxtuxl : protected_value_mirror;
  shared variable zdyoeqf : physical_value_mirror;
  shared variable bxurxeqaik : subtype_mirror;
  signal hertxbxns : std_logic;
  shared variable wpjgtgzxf : protected_value_mirror;
  shared variable op : physical_value_mirror;
  shared variable tpsfn : subtype_mirror;
begin
  jqxpbxp : entity work.vbum
    port map (llb => tpsfn, xnzzz => op, duwo => wpjgtgzxf, sej => hertxbxns);
  nvtjzus : entity work.vbum
    port map (llb => bxurxeqaik, xnzzz => zdyoeqf, duwo => ntxtuxl, sej => polmunkw);
  
  -- Multi-driven assignments
  hertxbxns <= hertxbxns;
  polmunkw <= hertxbxns;
end um;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity qvwpbwaw is
  port ( pg : linkage integer_vector(2 to 3)
  ; v : linkage std_logic_vector(2 downto 3)
  ; f : inout protected_subtype_mirror
  ; hywdxg : inout access_value_mirror
  );
end qvwpbwaw;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture woksbprjfz of qvwpbwaw is
  signal qreycmtk : std_logic;
  shared variable bgz : protected_value_mirror;
  shared variable yhhr : physical_value_mirror;
  shared variable hfjhuolwv : subtype_mirror;
begin
  tvzwesiij : entity work.vbum
    port map (llb => hfjhuolwv, xnzzz => yhhr, duwo => bgz, sej => qreycmtk);
  
  -- Multi-driven assignments
  qreycmtk <= qreycmtk;
end woksbprjfz;

use std.reflection.all;

entity zrlrtymanb is
  port (d : inout protected_subtype_mirror);
end zrlrtymanb;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture g of zrlrtymanb is
  signal yscd : std_logic;
  shared variable lcybxeh : protected_value_mirror;
  shared variable rgkl : physical_value_mirror;
  shared variable gw : subtype_mirror;
  shared variable p : access_value_mirror;
  shared variable jatqhjvdsl : protected_subtype_mirror;
  signal bhun : std_logic_vector(2 downto 3);
  signal qlwldoetki : integer_vector(2 to 3);
  shared variable bohiwh : access_value_mirror;
  shared variable shogkfs : protected_subtype_mirror;
  signal ucommxsu : std_logic_vector(2 downto 3);
  signal x : integer_vector(2 to 3);
  signal giveqyjqmy : std_logic;
  shared variable mujo : protected_value_mirror;
  shared variable qgwqu : physical_value_mirror;
  shared variable awlf : subtype_mirror;
begin
  xjprummd : entity work.vbum
    port map (llb => awlf, xnzzz => qgwqu, duwo => mujo, sej => giveqyjqmy);
  werdr : entity work.qvwpbwaw
    port map (pg => x, v => ucommxsu, f => shogkfs, hywdxg => bohiwh);
  s : entity work.qvwpbwaw
    port map (pg => qlwldoetki, v => bhun, f => jatqhjvdsl, hywdxg => p);
  trcbni : entity work.vbum
    port map (llb => gw, xnzzz => rgkl, duwo => lcybxeh, sej => yscd);
end g;



-- Seed after: 3928104359951615378,14426950258250697445
