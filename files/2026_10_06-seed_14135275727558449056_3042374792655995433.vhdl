-- Seed: 14135275727558449056,3042374792655995433

library ieee;
use ieee.std_logic_1164.all;

entity hpcgvx is
  port (txznlhzr : in std_logic_vector(1 downto 2));
end hpcgvx;

architecture ofgupmr of hpcgvx is
  
begin
  
end ofgupmr;

library ieee;
use ieee.std_logic_1164.all;

entity mrtstf is
  port (iunphqls : buffer std_logic);
end mrtstf;

library ieee;
use ieee.std_logic_1164.all;

architecture us of mrtstf is
  signal fo : std_logic_vector(1 downto 2);
  signal nzs : std_logic_vector(1 downto 2);
begin
  xc : entity work.hpcgvx
    port map (txznlhzr => nzs);
  zaaoznv : entity work.hpcgvx
    port map (txznlhzr => fo);
  
  -- Multi-driven assignments
  iunphqls <= '1';
  iunphqls <= 'L';
  iunphqls <= iunphqls;
  fo <= (others => '0');
end us;

entity moy is
  port (bzyvyovhz : out integer; rckmuvsln : in real);
end moy;

library ieee;
use ieee.std_logic_1164.all;

architecture gkca of moy is
  signal epefwiekkl : std_logic;
begin
  fpplvpus : entity work.mrtstf
    port map (iunphqls => epefwiekkl);
  
  -- Single-driven assignments
  bzyvyovhz <= bzyvyovhz;
  
  -- Multi-driven assignments
  epefwiekkl <= epefwiekkl;
  epefwiekkl <= epefwiekkl;
  epefwiekkl <= 'H';
end gkca;

entity iil is
  port (fpaqhg : linkage boolean; tu : inout string(2 to 3));
end iil;

architecture yurpeythmq of iil is
  
begin
  -- Single-driven assignments
  tu <= tu;
end yurpeythmq;



-- Seed after: 3046048461750012811,3042374792655995433
