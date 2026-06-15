-- Seed: 12043698005614259694,15300320181035395489

entity zsr is
  port (d : out boolean; fwri : buffer time; jhgqkxrh : buffer time; fn : out real);
end zsr;

architecture hwfzwxgb of zsr is
  
begin
  -- Single-driven assignments
  d <= TRUE;
  jhgqkxrh <= 431 ps;
  fn <= 3344.2;
  fwri <= 16#9_2_D_1_4.5_B# us;
end hwfzwxgb;

entity mazld is
  port (fp : inout bit_vector(4 downto 2));
end mazld;

architecture fxqagzvvh of mazld is
  
begin
  -- Single-driven assignments
  fp <= ('1', '1', '0');
end fxqagzvvh;

library ieee;
use ieee.std_logic_1164.all;

entity bircn is
  port (w : in std_logic_vector(0 to 1));
end bircn;

architecture xuwzalpi of bircn is
  signal hxcnjlc : bit_vector(4 downto 2);
  signal xg : bit_vector(4 downto 2);
begin
  wxnir : entity work.mazld
    port map (fp => xg);
  pkbh : entity work.mazld
    port map (fp => hxcnjlc);
end xuwzalpi;



-- Seed after: 6033750398677822523,15300320181035395489
