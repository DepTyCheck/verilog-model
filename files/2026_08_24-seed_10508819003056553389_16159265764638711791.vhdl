-- Seed: 10508819003056553389,16159265764638711791

entity dksegru is
  port (v : buffer real_vector(2 downto 3); k : buffer real; kjix : inout bit; skkrtjjtkn : linkage bit_vector(0 to 4));
end dksegru;

architecture xozyt of dksegru is
  
begin
  -- Single-driven assignments
  kjix <= kjix;
  v <= (others => 0.0);
  k <= k;
end xozyt;

library ieee;
use ieee.std_logic_1164.all;

entity lvjrif is
  port (irllnldmh : out severity_level; ipcsfpvg : inout std_logic);
end lvjrif;

architecture dtjio of lvjrif is
  signal dmbst : bit_vector(0 to 4);
  signal va : bit;
  signal pctreumzb : real;
  signal opzyuh : real_vector(2 downto 3);
begin
  glnaircax : entity work.dksegru
    port map (v => opzyuh, k => pctreumzb, kjix => va, skkrtjjtkn => dmbst);
  
  -- Single-driven assignments
  irllnldmh <= ERROR;
end dtjio;

entity tdjaoi is
  port (jmatbvxw : in integer);
end tdjaoi;

library ieee;
use ieee.std_logic_1164.all;

architecture vcv of tdjaoi is
  signal ftlaqhuaa : std_logic;
  signal hzqxh : severity_level;
  signal uaauvel : bit_vector(0 to 4);
  signal dlwzawwoc : bit;
  signal kzwul : real;
  signal yymjn : real_vector(2 downto 3);
  signal mksgepsan : bit_vector(0 to 4);
  signal lofw : bit;
  signal vpvmtz : real;
  signal ggmjfmh : real_vector(2 downto 3);
begin
  ynrqko : entity work.dksegru
    port map (v => ggmjfmh, k => vpvmtz, kjix => lofw, skkrtjjtkn => mksgepsan);
  qwysisu : entity work.dksegru
    port map (v => yymjn, k => kzwul, kjix => dlwzawwoc, skkrtjjtkn => uaauvel);
  jbf : entity work.lvjrif
    port map (irllnldmh => hzqxh, ipcsfpvg => ftlaqhuaa);
  
  -- Multi-driven assignments
  ftlaqhuaa <= 'U';
  ftlaqhuaa <= 'W';
  ftlaqhuaa <= 'L';
end vcv;



-- Seed after: 12513687987513350439,16159265764638711791
