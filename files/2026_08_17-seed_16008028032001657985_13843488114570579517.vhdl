-- Seed: 16008028032001657985,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (srswqwqg : out std_logic_vector(0 downto 2); wdjlhm : in integer_vector(1 to 3));
end n;

architecture ewgqlfa of n is
  
begin
  -- Multi-driven assignments
  srswqwqg <= "";
  srswqwqg <= srswqwqg;
end ewgqlfa;

entity dqdm is
  port (dllzn : inout integer);
end dqdm;

library ieee;
use ieee.std_logic_1164.all;

architecture zrcagmf of dqdm is
  signal nsjjfeolb : integer_vector(1 to 3);
  signal o : std_logic_vector(0 downto 2);
begin
  swmqruzzdv : entity work.n
    port map (srswqwqg => o, wdjlhm => nsjjfeolb);
  uziinptshb : entity work.n
    port map (srswqwqg => o, wdjlhm => nsjjfeolb);
  
  -- Single-driven assignments
  dllzn <= dllzn;
  nsjjfeolb <= (0_1_4_1, 3_1, 3_1_1_2);
end zrcagmf;

library ieee;
use ieee.std_logic_1164.all;

entity bjqwj is
  port (sjqtkulhve : out integer; htwrm : buffer std_logic);
end bjqwj;

library ieee;
use ieee.std_logic_1164.all;

architecture gdmrrhtqu of bjqwj is
  signal jsbfmyqiag : integer;
  signal ddduzpmooc : integer_vector(1 to 3);
  signal rsorgh : std_logic_vector(0 downto 2);
begin
  svkifgfjm : entity work.n
    port map (srswqwqg => rsorgh, wdjlhm => ddduzpmooc);
  mzdsqc : entity work.dqdm
    port map (dllzn => jsbfmyqiag);
  
  -- Single-driven assignments
  ddduzpmooc <= (16#6_8_C#, 3_2, 8#7_0_2_7#);
  
  -- Multi-driven assignments
  htwrm <= 'W';
  rsorgh <= "";
  htwrm <= 'W';
  htwrm <= 'X';
end gdmrrhtqu;



-- Seed after: 1719966607138260642,13843488114570579517
