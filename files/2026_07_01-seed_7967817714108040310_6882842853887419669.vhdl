-- Seed: 7967817714108040310,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity kc is
  port (hbrtxj : inout std_logic_vector(4 downto 3); gtagesvuep : in integer_vector(1 to 4));
end kc;

architecture xwqekelci of kc is
  
begin
  -- Multi-driven assignments
  hbrtxj <= "U-";
  hbrtxj <= ('L', '0');
  hbrtxj <= "WZ";
  hbrtxj <= "H-";
end xwqekelci;

library ieee;
use ieee.std_logic_1164.all;

entity iptr is
  port (dtcu : in std_logic);
end iptr;

architecture cvwvw of iptr is
  
begin
  
end cvwvw;

library ieee;
use ieee.std_logic_1164.all;

entity w is
  port (tlh : out std_logic);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture df of w is
  signal vzl : integer_vector(1 to 4);
  signal qgeamydnzd : integer_vector(1 to 4);
  signal xpfxkeb : std_logic_vector(4 downto 3);
  signal at : integer_vector(1 to 4);
  signal gazhcz : std_logic_vector(4 downto 3);
begin
  hngkop : entity work.kc
    port map (hbrtxj => gazhcz, gtagesvuep => at);
  ftukbmaui : entity work.kc
    port map (hbrtxj => xpfxkeb, gtagesvuep => qgeamydnzd);
  nslkto : entity work.kc
    port map (hbrtxj => gazhcz, gtagesvuep => vzl);
  
  -- Single-driven assignments
  at <= (3_2, 0, 2, 0_2_3_0_3);
  vzl <= (8#51#, 22242, 8#3_3#, 8#0#);
  qgeamydnzd <= (8#3#, 8#2#, 0, 32041);
  
  -- Multi-driven assignments
  xpfxkeb <= "0-";
  gazhcz <= ('-', 'Z');
  gazhcz <= "UH";
  tlh <= 'Z';
end df;

library ieee;
use ieee.std_logic_1164.all;

entity fep is
  port (d : linkage real; bziby : linkage std_logic; viqlqz : inout std_logic);
end fep;

library ieee;
use ieee.std_logic_1164.all;

architecture ymx of fep is
  signal q : integer_vector(1 to 4);
  signal kppwrcuojc : std_logic_vector(4 downto 3);
begin
  ppvv : entity work.kc
    port map (hbrtxj => kppwrcuojc, gtagesvuep => q);
  
  -- Single-driven assignments
  q <= (2_0, 344, 12, 20221);
  
  -- Multi-driven assignments
  viqlqz <= '-';
  viqlqz <= 'Z';
end ymx;



-- Seed after: 2596254406600724052,6882842853887419669
