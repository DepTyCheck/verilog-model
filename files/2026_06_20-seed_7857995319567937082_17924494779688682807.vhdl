-- Seed: 7857995319567937082,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity mxzzl is
  port (jkrlz : linkage std_logic; rdghlrh : buffer real_vector(1 downto 0); bgqeo : out integer);
end mxzzl;

architecture fxkay of mxzzl is
  
begin
  -- Single-driven assignments
  rdghlrh <= (8#4_2_1_0_3.1_6_0_2_5#, 16#4_0_9_B.5_B#);
  bgqeo <= 1_1_1_1;
end fxkay;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (jmsc : buffer time_vector(1 downto 0); kmjf : linkage real; foy : linkage std_logic_vector(1 downto 4); sit : out integer);
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture ja of x is
  signal o : integer;
  signal savgl : real_vector(1 downto 0);
  signal ttwow : real_vector(1 downto 0);
  signal mjuuxkguk : integer;
  signal sk : real_vector(1 downto 0);
  signal eratltzte : integer;
  signal mloyirdhpw : real_vector(1 downto 0);
  signal g : std_logic;
begin
  xzviuwcr : entity work.mxzzl
    port map (jkrlz => g, rdghlrh => mloyirdhpw, bgqeo => eratltzte);
  euvrjpp : entity work.mxzzl
    port map (jkrlz => g, rdghlrh => sk, bgqeo => mjuuxkguk);
  pmjq : entity work.mxzzl
    port map (jkrlz => g, rdghlrh => ttwow, bgqeo => sit);
  ciizsqt : entity work.mxzzl
    port map (jkrlz => g, rdghlrh => savgl, bgqeo => o);
  
  -- Single-driven assignments
  jmsc <= (4 min, 2#0_0# us);
  
  -- Multi-driven assignments
  g <= '0';
  g <= '-';
end ja;



-- Seed after: 6294465878562857484,17924494779688682807
