-- Seed: 4910977861577427713,3181554006726329157

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity voeo is
  port (bwvlqd : in std_logic_vector(3 downto 4); ncwoi : inout integer; a : inout subtype_mirror);
end voeo;

architecture mspuk of voeo is
  
begin
  -- Single-driven assignments
  ncwoi <= 4_0_4_2;
end mspuk;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity gcxd is
  port (jqnuq : in std_logic_vector(3 downto 0); k : in std_logic_vector(2 to 3); bczgwf : inout array_value_mirror; gg : inout value_mirror);
end gcxd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture pdkcqw of gcxd is
  shared variable zzegzqgbkk : subtype_mirror;
  signal w : integer;
  signal szhlx : std_logic_vector(3 downto 4);
begin
  kw : entity work.voeo
    port map (bwvlqd => szhlx, ncwoi => w, a => zzegzqgbkk);
  
  -- Multi-driven assignments
  szhlx <= "";
  szhlx <= szhlx;
end pdkcqw;

use std.reflection.all;

entity krqh is
  port (plqm : linkage time; mzg : buffer integer; m : inout access_subtype_mirror; qmsqjgcu : inout record_value_mirror);
end krqh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture gklgucqslm of krqh is
  shared variable j : value_mirror;
  shared variable uwtkar : array_value_mirror;
  signal lho : std_logic_vector(2 to 3);
  signal kvdcgndztw : std_logic_vector(3 downto 0);
  shared variable ycj : subtype_mirror;
  shared variable iuiakicg : subtype_mirror;
  signal wwpxqgzryx : integer;
  signal d : std_logic_vector(3 downto 4);
begin
  slapiwr : entity work.voeo
    port map (bwvlqd => d, ncwoi => wwpxqgzryx, a => iuiakicg);
  fhhex : entity work.voeo
    port map (bwvlqd => d, ncwoi => mzg, a => ycj);
  w : entity work.gcxd
    port map (jqnuq => kvdcgndztw, k => lho, bczgwf => uwtkar, gg => j);
  
  -- Multi-driven assignments
  kvdcgndztw <= kvdcgndztw;
  kvdcgndztw <= kvdcgndztw;
  kvdcgndztw <= ('Z', '0', '0', '0');
end gklgucqslm;

use std.reflection.all;

entity yjpz is
  port (arucy : linkage integer; bynf : out real; wzilv : inout record_value_mirror; vhvnueq : buffer integer);
end yjpz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture lfz of yjpz is
  shared variable r : record_value_mirror;
  shared variable vfmorhwb : access_subtype_mirror;
  signal cvbni : time;
  shared variable ojainoje : subtype_mirror;
  signal gbsl : integer;
  signal vyat : std_logic_vector(3 downto 4);
begin
  szj : entity work.voeo
    port map (bwvlqd => vyat, ncwoi => gbsl, a => ojainoje);
  t : entity work.krqh
    port map (plqm => cvbni, mzg => vhvnueq, m => vfmorhwb, qmsqjgcu => r);
  
  -- Single-driven assignments
  bynf <= 8#10564.1_1_2#;
  
  -- Multi-driven assignments
  vyat <= vyat;
  vyat <= "";
end lfz;



-- Seed after: 5005166971928308060,3181554006726329157
