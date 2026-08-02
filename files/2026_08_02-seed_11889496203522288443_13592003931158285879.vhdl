-- Seed: 11889496203522288443,13592003931158285879

entity ejssi is
  port (mvmcjbmt : linkage bit; nwiurndoa : buffer boolean_vector(4 downto 3));
end ejssi;

architecture npqllojikq of ejssi is
  
begin
  -- Single-driven assignments
  nwiurndoa <= (TRUE, FALSE);
end npqllojikq;

library ieee;
use ieee.std_logic_1164.all;

entity aztacf is
  port (raxvum : linkage std_logic_vector(0 to 3); vkwrpd : out severity_level; yu : buffer time; nqudb : out integer);
end aztacf;

architecture fvdvpnh of aztacf is
  
begin
  -- Single-driven assignments
  yu <= 1 hr;
end fvdvpnh;

library ieee;
use ieee.std_logic_1164.all;

entity wctlf is
  port (abehzeke : inout real_vector(0 to 1); rcr : in std_logic; uhmtcl : inout std_logic_vector(4 to 3); f : buffer time_vector(2 downto 2));
end wctlf;

architecture nj of wctlf is
  signal tqeyfarmh : boolean_vector(4 downto 3);
  signal joszyg : bit;
begin
  t : entity work.ejssi
    port map (mvmcjbmt => joszyg, nwiurndoa => tqeyfarmh);
  
  -- Single-driven assignments
  abehzeke <= (4_2_3_4.200, 2#1_1_0_0.1_0_0_0#);
  f <= (others => 0_4_4.0_4_2_4 ms);
  
  -- Multi-driven assignments
  uhmtcl <= uhmtcl;
  uhmtcl <= "";
end nj;

library ieee;
use ieee.std_logic_1164.all;

entity gazteloezq is
  port (z : buffer time; aqe : in std_logic);
end gazteloezq;

architecture fgyz of gazteloezq is
  signal uwinjf : boolean_vector(4 downto 3);
  signal vl : bit;
  signal dcvm : boolean_vector(4 downto 3);
  signal azllmss : bit;
begin
  co : entity work.ejssi
    port map (mvmcjbmt => azllmss, nwiurndoa => dcvm);
  kgryrbcb : entity work.ejssi
    port map (mvmcjbmt => vl, nwiurndoa => uwinjf);
  
  -- Single-driven assignments
  z <= z;
end fgyz;



-- Seed after: 9695046589025629085,13592003931158285879
