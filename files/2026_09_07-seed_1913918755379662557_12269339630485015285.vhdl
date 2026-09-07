-- Seed: 1913918755379662557,12269339630485015285

library ieee;
use ieee.std_logic_1164.all;

entity lwmzu is
  port (qux : inout std_logic_vector(3 to 4));
end lwmzu;

architecture u of lwmzu is
  
begin
  -- Multi-driven assignments
  qux <= qux;
  qux <= ('0', 'W');
end u;

library ieee;
use ieee.std_logic_1164.all;

entity ramb is
  port (gybli : inout integer; miyniatfi : in time; iwj : linkage std_logic_vector(4 to 1));
end ramb;

library ieee;
use ieee.std_logic_1164.all;

architecture jxefmfbr of ramb is
  signal yoti : std_logic_vector(3 to 4);
  signal cn : std_logic_vector(3 to 4);
begin
  chcszzmkh : entity work.lwmzu
    port map (qux => cn);
  aluiaj : entity work.lwmzu
    port map (qux => cn);
  a : entity work.lwmzu
    port map (qux => yoti);
  
  -- Single-driven assignments
  gybli <= gybli;
  
  -- Multi-driven assignments
  cn <= "XZ";
  cn <= cn;
  cn <= cn;
  cn <= ('X', 'H');
end jxefmfbr;

entity bhrqsyi is
  port (pehuckae : buffer real; gr : out time);
end bhrqsyi;

library ieee;
use ieee.std_logic_1164.all;

architecture nxaqirt of bhrqsyi is
  signal h : std_logic_vector(3 to 4);
  signal mob : std_logic_vector(3 to 4);
  signal prseaujg : std_logic_vector(3 to 4);
begin
  lufmdqs : entity work.lwmzu
    port map (qux => prseaujg);
  gfkhqih : entity work.lwmzu
    port map (qux => mob);
  ses : entity work.lwmzu
    port map (qux => h);
  
  -- Single-driven assignments
  gr <= 2_2_3_4.21100 us;
  pehuckae <= 0_4_0.0;
  
  -- Multi-driven assignments
  prseaujg <= h;
  h <= h;
  prseaujg <= "UL";
end nxaqirt;



-- Seed after: 14322744437542115445,12269339630485015285
