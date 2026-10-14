-- Seed: 16095921992292732246,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity pwrsfmtf is
  port (unvwht : buffer bit_vector(3 downto 1); fdff : linkage std_logic);
end pwrsfmtf;

architecture nm of pwrsfmtf is
  
begin
  -- Single-driven assignments
  unvwht <= ('0', '1', '0');
end nm;

entity auwrzr is
  port (lsincc : linkage time);
end auwrzr;

library ieee;
use ieee.std_logic_1164.all;

architecture ajxqliqtvc of auwrzr is
  signal vcgjkgcg : std_logic;
  signal scezs : bit_vector(3 downto 1);
begin
  tzpnsk : entity work.pwrsfmtf
    port map (unvwht => scezs, fdff => vcgjkgcg);
  
  -- Multi-driven assignments
  vcgjkgcg <= 'X';
  vcgjkgcg <= 'X';
end ajxqliqtvc;



-- Seed after: 7995542198440449588,2287986438313874379
