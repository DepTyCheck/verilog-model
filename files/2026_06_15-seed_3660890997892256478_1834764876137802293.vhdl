-- Seed: 3660890997892256478,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity lm is
  port (pnpox : linkage integer; vhhgfjrem : out std_logic_vector(2 downto 2); jpbcdbcnnq : in character);
end lm;

architecture krbtjiv of lm is
  
begin
  -- Multi-driven assignments
  vhhgfjrem <= "L";
  vhhgfjrem <= (others => '0');
  vhhgfjrem <= "1";
  vhhgfjrem <= "-";
end krbtjiv;

entity tyxye is
  port (zeform : in severity_level);
end tyxye;

architecture vmrsrqmlvm of tyxye is
  
begin
  
end vmrsrqmlvm;

library ieee;
use ieee.std_logic_1164.all;

entity bgtzh is
  port (gzrzzyono : inout boolean; ypn : out std_logic);
end bgtzh;

library ieee;
use ieee.std_logic_1164.all;

architecture yxfxvqzn of bgtzh is
  signal vryarktvdq : severity_level;
  signal uaxnokidnt : severity_level;
  signal ha : character;
  signal n : std_logic_vector(2 downto 2);
  signal ydgsp : integer;
begin
  vftyrzj : entity work.lm
    port map (pnpox => ydgsp, vhhgfjrem => n, jpbcdbcnnq => ha);
  nxbvmr : entity work.tyxye
    port map (zeform => uaxnokidnt);
  je : entity work.tyxye
    port map (zeform => vryarktvdq);
  
  -- Single-driven assignments
  uaxnokidnt <= WARNING;
  vryarktvdq <= ERROR;
  gzrzzyono <= FALSE;
  ha <= 'k';
  
  -- Multi-driven assignments
  ypn <= '1';
end yxfxvqzn;

entity njyfta is
  port (oez : in string(3 to 4));
end njyfta;

architecture fvlvba of njyfta is
  
begin
  
end fvlvba;



-- Seed after: 2021323706375365399,1834764876137802293
