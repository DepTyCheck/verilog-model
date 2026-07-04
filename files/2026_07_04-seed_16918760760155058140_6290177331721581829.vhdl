-- Seed: 16918760760155058140,6290177331721581829

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity jc is
  port (qqmvcqlz : linkage std_logic_vector(3 to 2); ig : inout file_value_mirror; qag : linkage time_vector(4 to 0));
end jc;

architecture irfeojxbq of jc is
  
begin
  
end irfeojxbq;

use std.reflection.all;

entity ykrhaqjkbl is
  port (ixl : inout access_subtype_mirror);
end ykrhaqjkbl;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture bksx of ykrhaqjkbl is
  signal xgabfpa : time_vector(4 to 0);
  shared variable nutarqtidp : file_value_mirror;
  signal jhf : std_logic_vector(3 to 2);
begin
  snfkec : entity work.jc
    port map (qqmvcqlz => jhf, ig => nutarqtidp, qag => xgabfpa);
  
  -- Multi-driven assignments
  jhf <= jhf;
end bksx;

use std.reflection.all;

entity vklrmd is
  port (wijxziwhhs : inout file_value_mirror; hruvjlxmu : inout access_subtype_mirror);
end vklrmd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture hwt of vklrmd is
  shared variable aedri : access_subtype_mirror;
  signal ahlwvoah : time_vector(4 to 0);
  shared variable i : file_value_mirror;
  signal sfsmvs : std_logic_vector(3 to 2);
  signal d : time_vector(4 to 0);
  shared variable dv : file_value_mirror;
  signal zeftmsdxim : std_logic_vector(3 to 2);
  signal ynzqzlju : time_vector(4 to 0);
  shared variable akef : file_value_mirror;
  signal s : std_logic_vector(3 to 2);
begin
  h : entity work.jc
    port map (qqmvcqlz => s, ig => akef, qag => ynzqzlju);
  zdugid : entity work.jc
    port map (qqmvcqlz => zeftmsdxim, ig => dv, qag => d);
  zihnojsu : entity work.jc
    port map (qqmvcqlz => sfsmvs, ig => i, qag => ahlwvoah);
  shaj : entity work.ykrhaqjkbl
    port map (ixl => aedri);
  
  -- Multi-driven assignments
  s <= "";
end hwt;

library ieee;
use ieee.std_logic_1164.all;

entity laz is
  port (pphij : buffer real_vector(1 downto 1); r : linkage std_logic; dy : inout std_logic_vector(4 downto 1));
end laz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture wt of laz is
  signal qpfn : time_vector(4 to 0);
  shared variable zmufwz : file_value_mirror;
  signal bnbbay : std_logic_vector(3 to 2);
  signal f : time_vector(4 to 0);
  shared variable blaskc : file_value_mirror;
  signal fkvmvaimo : time_vector(4 to 0);
  shared variable pdawqcoz : file_value_mirror;
  signal yh : time_vector(4 to 0);
  shared variable vfjb : file_value_mirror;
  signal jg : std_logic_vector(3 to 2);
begin
  z : entity work.jc
    port map (qqmvcqlz => jg, ig => vfjb, qag => yh);
  wbwffkuz : entity work.jc
    port map (qqmvcqlz => jg, ig => pdawqcoz, qag => fkvmvaimo);
  naezii : entity work.jc
    port map (qqmvcqlz => jg, ig => blaskc, qag => f);
  ybxik : entity work.jc
    port map (qqmvcqlz => bnbbay, ig => zmufwz, qag => qpfn);
  
  -- Single-driven assignments
  pphij <= pphij;
end wt;



-- Seed after: 5033296040554121985,6290177331721581829
