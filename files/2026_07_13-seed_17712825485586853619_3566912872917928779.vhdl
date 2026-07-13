-- Seed: 17712825485586853619,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity oim is
  port (aqfi : inout integer; omoxsrms : out std_logic_vector(4 to 4); mfi : inout bit);
end oim;

architecture y of oim is
  
begin
  -- Multi-driven assignments
  omoxsrms <= omoxsrms;
end y;

use std.reflection.all;

entity wbzksaxolb is
  port (mbjzbbkfg : buffer time; j : inout subtype_mirror; oklugmbww : inout enumeration_value_mirror; wpnxytjrg : out integer);
end wbzksaxolb;

library ieee;
use ieee.std_logic_1164.all;

architecture welgbxec of wbzksaxolb is
  signal yimapvye : bit;
  signal g : std_logic_vector(4 to 4);
  signal ncjul : integer;
  signal ptzmhqps : bit;
  signal nwvuc : integer;
  signal yinkdumw : bit;
  signal nfczhunr : std_logic_vector(4 to 4);
begin
  pgxws : entity work.oim
    port map (aqfi => wpnxytjrg, omoxsrms => nfczhunr, mfi => yinkdumw);
  wcglo : entity work.oim
    port map (aqfi => nwvuc, omoxsrms => nfczhunr, mfi => ptzmhqps);
  isbqt : entity work.oim
    port map (aqfi => ncjul, omoxsrms => g, mfi => yimapvye);
  
  -- Single-driven assignments
  mbjzbbkfg <= mbjzbbkfg;
  
  -- Multi-driven assignments
  nfczhunr <= (others => 'Z');
  g <= nfczhunr;
  g <= nfczhunr;
end welgbxec;

use std.reflection.all;

entity iikjce is
  port (ru : inout time; fylajwlo : inout record_subtype_mirror);
end iikjce;

use std.reflection.all;

architecture vxfiqp of iikjce is
  signal muxgbxjuho : integer;
  shared variable klih : enumeration_value_mirror;
  shared variable oyfet : subtype_mirror;
  signal paypiphbi : time;
begin
  mndqsvhfi : entity work.wbzksaxolb
    port map (mbjzbbkfg => paypiphbi, j => oyfet, oklugmbww => klih, wpnxytjrg => muxgbxjuho);
  
  -- Single-driven assignments
  ru <= ru;
end vxfiqp;



-- Seed after: 17881226362790916893,3566912872917928779
