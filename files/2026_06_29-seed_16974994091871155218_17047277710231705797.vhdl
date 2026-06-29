-- Seed: 16974994091871155218,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity cnwott is
  port (vhodr : inout real; vexqoqnsi : buffer std_logic; uztfcyomep : out boolean; qttsutk : out std_logic_vector(4 downto 1));
end cnwott;

architecture symdpu of cnwott is
  
begin
  -- Single-driven assignments
  uztfcyomep <= TRUE;
  vhodr <= 8#5.3_5_2_6#;
  
  -- Multi-driven assignments
  qttsutk <= "W1-L";
  qttsutk <= ('U', 'L', 'X', 'X');
  qttsutk <= ('-', 'U', 'Z', 'X');
end symdpu;

library ieee;
use ieee.std_logic_1164.all;

entity fbhdoamleu is
  port (udbk : linkage std_logic_vector(3 downto 4); jstg : linkage integer; qv : in std_logic_vector(0 to 2); jvfks : out std_logic);
end fbhdoamleu;

library ieee;
use ieee.std_logic_1164.all;

architecture z of fbhdoamleu is
  signal siuow : std_logic_vector(4 downto 1);
  signal xemc : boolean;
  signal wleciqaqe : real;
  signal hslryvu : std_logic_vector(4 downto 1);
  signal j : boolean;
  signal jmhrq : std_logic;
  signal iimxawxopb : real;
  signal wrx : std_logic_vector(4 downto 1);
  signal xbtt : boolean;
  signal knuj : real;
  signal g : std_logic_vector(4 downto 1);
  signal ol : boolean;
  signal bsptst : std_logic;
  signal gtgalibx : real;
begin
  bbwaxrt : entity work.cnwott
    port map (vhodr => gtgalibx, vexqoqnsi => bsptst, uztfcyomep => ol, qttsutk => g);
  rpqlrwhbav : entity work.cnwott
    port map (vhodr => knuj, vexqoqnsi => jvfks, uztfcyomep => xbtt, qttsutk => wrx);
  gsnkz : entity work.cnwott
    port map (vhodr => iimxawxopb, vexqoqnsi => jmhrq, uztfcyomep => j, qttsutk => hslryvu);
  mcrxtnn : entity work.cnwott
    port map (vhodr => wleciqaqe, vexqoqnsi => jvfks, uztfcyomep => xemc, qttsutk => siuow);
  
  -- Multi-driven assignments
  siuow <= ('-', 'H', '0', '-');
  bsptst <= '0';
end z;



-- Seed after: 12151646339706718716,17047277710231705797
