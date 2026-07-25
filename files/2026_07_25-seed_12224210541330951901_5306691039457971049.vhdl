-- Seed: 12224210541330951901,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (yainxou : out std_logic; qslozkwp : out std_logic);
end o;

architecture miu of o is
  
begin
  -- Multi-driven assignments
  qslozkwp <= qslozkwp;
  yainxou <= 'U';
  qslozkwp <= 'Z';
end miu;

entity yimpwsas is
  port (gro : linkage bit; ligudj : buffer integer_vector(3 to 4));
end yimpwsas;

library ieee;
use ieee.std_logic_1164.all;

architecture ombiwdn of yimpwsas is
  signal jcjkzppc : std_logic;
  signal sw : std_logic;
  signal doarp : std_logic;
  signal zpwsvndq : std_logic;
begin
  zcydidauc : entity work.o
    port map (yainxou => zpwsvndq, qslozkwp => zpwsvndq);
  zszwd : entity work.o
    port map (yainxou => zpwsvndq, qslozkwp => doarp);
  fjlohlpcvg : entity work.o
    port map (yainxou => sw, qslozkwp => jcjkzppc);
  
  -- Single-driven assignments
  ligudj <= (2_0_3_4, 2#10010#);
end ombiwdn;

library ieee;
use ieee.std_logic_1164.all;

entity sl is
  port (eowfkhlsmh : in std_logic_vector(0 downto 2));
end sl;

library ieee;
use ieee.std_logic_1164.all;

architecture roy of sl is
  signal zpc : std_logic;
begin
  aizmchs : entity work.o
    port map (yainxou => zpc, qslozkwp => zpc);
  
  -- Multi-driven assignments
  zpc <= zpc;
end roy;

entity mfkdzqcphk is
  port (bwqla : linkage real; h : linkage time);
end mfkdzqcphk;

architecture qtsxjoaru of mfkdzqcphk is
  signal zfvjwi : integer_vector(3 to 4);
  signal acjk : bit;
begin
  exjgyqwqwc : entity work.yimpwsas
    port map (gro => acjk, ligudj => zfvjwi);
end qtsxjoaru;



-- Seed after: 3895797303769566655,5306691039457971049
