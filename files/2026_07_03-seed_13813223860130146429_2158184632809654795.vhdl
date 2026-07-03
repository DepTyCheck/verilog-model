-- Seed: 13813223860130146429,2158184632809654795

library ieee;
use ieee.std_logic_1164.all;

entity j is
  port (rfjwscw : out std_logic_vector(1 downto 0); xallmw : in time);
end j;

architecture bwwixkyqnh of j is
  
begin
  -- Multi-driven assignments
  rfjwscw <= rfjwscw;
  rfjwscw <= "Z1";
end bwwixkyqnh;

entity hstjlabvyk is
  port (jqdkwp : inout time; dxswdzgi : out character);
end hstjlabvyk;

library ieee;
use ieee.std_logic_1164.all;

architecture jifsscorj of hstjlabvyk is
  signal vfvef : time;
  signal n : time;
  signal lam : std_logic_vector(1 downto 0);
begin
  bdpxgvbr : entity work.j
    port map (rfjwscw => lam, xallmw => jqdkwp);
  aqno : entity work.j
    port map (rfjwscw => lam, xallmw => n);
  onbasqh : entity work.j
    port map (rfjwscw => lam, xallmw => jqdkwp);
  wduana : entity work.j
    port map (rfjwscw => lam, xallmw => vfvef);
  
  -- Single-driven assignments
  n <= 412 us;
  vfvef <= 16#C981B.E0D0# ps;
  dxswdzgi <= 'u';
  jqdkwp <= 2#1_1_1.101# fs;
  
  -- Multi-driven assignments
  lam <= lam;
end jifsscorj;



-- Seed after: 16720086754293814080,2158184632809654795
