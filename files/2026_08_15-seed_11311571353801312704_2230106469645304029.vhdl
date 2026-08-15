-- Seed: 11311571353801312704,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity mwtowmb is
  port (ilnxtvmrin : buffer std_logic; xajdbftxt : out std_logic_vector(4 downto 4); euedicwl : linkage real);
end mwtowmb;

architecture ugbkqyhbp of mwtowmb is
  
begin
  -- Multi-driven assignments
  xajdbftxt <= "X";
  xajdbftxt <= xajdbftxt;
  ilnxtvmrin <= '0';
end ugbkqyhbp;

library ieee;
use ieee.std_logic_1164.all;

entity rah is
  port (y : buffer integer; njybccxhvz : inout std_logic_vector(3 to 4); tcsoqcpff : linkage time);
end rah;

library ieee;
use ieee.std_logic_1164.all;

architecture vkphk of rah is
  signal osxy : real;
  signal r : std_logic;
  signal fi : real;
  signal nl : real;
  signal js : std_logic_vector(4 downto 4);
  signal hbv : std_logic;
begin
  dwdsy : entity work.mwtowmb
    port map (ilnxtvmrin => hbv, xajdbftxt => js, euedicwl => nl);
  f : entity work.mwtowmb
    port map (ilnxtvmrin => hbv, xajdbftxt => js, euedicwl => fi);
  gjsnhd : entity work.mwtowmb
    port map (ilnxtvmrin => r, xajdbftxt => js, euedicwl => osxy);
  
  -- Single-driven assignments
  y <= 8#2_4_4#;
  
  -- Multi-driven assignments
  njybccxhvz <= ('Z', 'U');
  js <= "1";
  r <= '1';
end vkphk;



-- Seed after: 4093706649223967081,2230106469645304029
