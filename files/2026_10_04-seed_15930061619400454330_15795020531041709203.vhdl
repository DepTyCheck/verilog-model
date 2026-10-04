-- Seed: 15930061619400454330,15795020531041709203

library ieee;
use ieee.std_logic_1164.all;

entity imamk is
  port (lvhevnfsl : buffer boolean_vector(1 downto 2); gfpekowj : linkage std_logic; cy : in time; jrcwsvkqg : inout character);
end imamk;

architecture vuwwdh of imamk is
  
begin
  -- Single-driven assignments
  lvhevnfsl <= lvhevnfsl;
  jrcwsvkqg <= jrcwsvkqg;
end vuwwdh;

entity xyqehv is
  port (t : linkage character);
end xyqehv;

library ieee;
use ieee.std_logic_1164.all;

architecture mthzwrmc of xyqehv is
  signal y : character;
  signal uyepf : time;
  signal zgteetppm : boolean_vector(1 downto 2);
  signal umddtdewy : character;
  signal cg : time;
  signal htrnlp : std_logic;
  signal dhuoggih : boolean_vector(1 downto 2);
begin
  giki : entity work.imamk
    port map (lvhevnfsl => dhuoggih, gfpekowj => htrnlp, cy => cg, jrcwsvkqg => umddtdewy);
  pt : entity work.imamk
    port map (lvhevnfsl => zgteetppm, gfpekowj => htrnlp, cy => uyepf, jrcwsvkqg => y);
  
  -- Single-driven assignments
  uyepf <= cg;
  
  -- Multi-driven assignments
  htrnlp <= 'U';
  htrnlp <= htrnlp;
  htrnlp <= 'Z';
  htrnlp <= '0';
end mthzwrmc;



-- Seed after: 18220800961864985942,15795020531041709203
