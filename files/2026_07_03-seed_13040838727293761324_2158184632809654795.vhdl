-- Seed: 13040838727293761324,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity hsgyeotu is
  port (vmiuqhh : out std_logic; fyt : inout time; pfhuz : inout access_subtype_mirror; bhedybypzb : buffer std_logic_vector(1 downto 0));
end hsgyeotu;

architecture bog of hsgyeotu is
  
begin
  -- Single-driven assignments
  fyt <= 16#9B.E# ns;
  
  -- Multi-driven assignments
  bhedybypzb <= ('Z', '0');
  bhedybypzb <= "ZU";
  bhedybypzb <= bhedybypzb;
end bog;

use std.reflection.all;

entity pwl is
  port (l : inout access_subtype_mirror; xykjzfpm : inout floating_subtype_mirror);
end pwl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mn of pwl is
  signal k : std_logic_vector(1 downto 0);
  shared variable cbdppqf : access_subtype_mirror;
  signal ovbcsjnh : time;
  signal knaecij : std_logic_vector(1 downto 0);
  signal aohry : time;
  signal sp : std_logic_vector(1 downto 0);
  shared variable uogdlb : access_subtype_mirror;
  signal qbed : time;
  signal qxwwo : std_logic;
begin
  mklzj : entity work.hsgyeotu
    port map (vmiuqhh => qxwwo, fyt => qbed, pfhuz => uogdlb, bhedybypzb => sp);
  itzfq : entity work.hsgyeotu
    port map (vmiuqhh => qxwwo, fyt => aohry, pfhuz => l, bhedybypzb => knaecij);
  v : entity work.hsgyeotu
    port map (vmiuqhh => qxwwo, fyt => ovbcsjnh, pfhuz => cbdppqf, bhedybypzb => k);
end mn;

use std.reflection.all;

entity dg is
  port (mquqmmd : out integer; sbdfnhls : inout protected_subtype_mirror; qpsqkw : inout file_subtype_mirror; yncuk : inout boolean_vector(1 to 2));
end dg;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture vpnf of dg is
  shared variable xcxkbxvl : access_subtype_mirror;
  signal oqqvtrlf : time;
  signal ter : std_logic_vector(1 downto 0);
  shared variable kozxzo : access_subtype_mirror;
  signal z : time;
  signal osjo : std_logic;
begin
  jao : entity work.hsgyeotu
    port map (vmiuqhh => osjo, fyt => z, pfhuz => kozxzo, bhedybypzb => ter);
  qe : entity work.hsgyeotu
    port map (vmiuqhh => osjo, fyt => oqqvtrlf, pfhuz => xcxkbxvl, bhedybypzb => ter);
  
  -- Single-driven assignments
  yncuk <= (TRUE, TRUE);
end vpnf;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity enqytka is
  port (sqmu : inout record_subtype_mirror; nz : inout std_logic_vector(4 to 0));
end enqytka;

use std.reflection.all;

architecture ydzvobd of enqytka is
  shared variable i : floating_subtype_mirror;
  shared variable xsmclgguw : access_subtype_mirror;
  signal bwqqzo : boolean_vector(1 to 2);
  shared variable lvoruhm : file_subtype_mirror;
  shared variable yrgrk : protected_subtype_mirror;
  signal afi : integer;
begin
  upwwlar : entity work.dg
    port map (mquqmmd => afi, sbdfnhls => yrgrk, qpsqkw => lvoruhm, yncuk => bwqqzo);
  jhbbkom : entity work.pwl
    port map (l => xsmclgguw, xykjzfpm => i);
  
  -- Multi-driven assignments
  nz <= "";
end ydzvobd;



-- Seed after: 11992251194011348887,2158184632809654795
