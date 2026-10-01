-- Seed: 14381221016753998947,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity fadfdoow is
  port (z : in string(2 downto 2); ec : linkage std_logic; ssfbydri : inout std_logic_vector(3 downto 4));
end fadfdoow;

architecture tvpkcsdvw of fadfdoow is
  
begin
  -- Multi-driven assignments
  ssfbydri <= "";
  ssfbydri <= "";
  ssfbydri <= (others => '0');
end tvpkcsdvw;

library ieee;
use ieee.std_logic_1164.all;

entity qcdvf is
  port (ecsd : in std_logic_vector(4 to 4); xo : buffer std_logic_vector(3 downto 3));
end qcdvf;

library ieee;
use ieee.std_logic_1164.all;

architecture hym of qcdvf is
  signal tfreusd : std_logic;
  signal mkdijyi : std_logic_vector(3 downto 4);
  signal grcrj : std_logic;
  signal zsyrhpj : string(2 downto 2);
begin
  efxsob : entity work.fadfdoow
    port map (z => zsyrhpj, ec => grcrj, ssfbydri => mkdijyi);
  gjjhuyt : entity work.fadfdoow
    port map (z => zsyrhpj, ec => tfreusd, ssfbydri => mkdijyi);
  
  -- Multi-driven assignments
  mkdijyi <= (others => '0');
  xo <= "Z";
  xo <= "-";
  mkdijyi <= mkdijyi;
end hym;

library ieee;
use ieee.std_logic_1164.all;

entity jhhqu is
  port (gdrgq : in std_logic_vector(2 downto 4); cfehw : linkage time);
end jhhqu;

library ieee;
use ieee.std_logic_1164.all;

architecture ph of jhhqu is
  signal yqlq : std_logic_vector(3 downto 4);
  signal murh : string(2 downto 2);
  signal fjh : std_logic_vector(3 downto 4);
  signal fsnzsksc : std_logic;
  signal pzisp : string(2 downto 2);
  signal tli : std_logic_vector(3 downto 4);
  signal q : std_logic;
  signal zfa : string(2 downto 2);
begin
  svnzcqupw : entity work.fadfdoow
    port map (z => zfa, ec => q, ssfbydri => tli);
  jdpanerz : entity work.fadfdoow
    port map (z => pzisp, ec => q, ssfbydri => tli);
  lwurmqftvd : entity work.fadfdoow
    port map (z => pzisp, ec => fsnzsksc, ssfbydri => fjh);
  wvcs : entity work.fadfdoow
    port map (z => murh, ec => fsnzsksc, ssfbydri => yqlq);
  
  -- Single-driven assignments
  zfa <= (others => 'i');
  murh <= zfa;
  pzisp <= murh;
  
  -- Multi-driven assignments
  q <= q;
  fsnzsksc <= 'U';
end ph;



-- Seed after: 16886648519105959446,15025465285671019065
