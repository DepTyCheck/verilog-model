-- Seed: 11091017937486120209,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity ukq is
  port (hxeeof : linkage time; ojlsxuo : buffer std_logic_vector(1 to 4); y : in bit_vector(1 to 0));
end ukq;

architecture eo of ukq is
  
begin
  -- Multi-driven assignments
  ojlsxuo <= "LWUW";
  ojlsxuo <= ('L', 'U', 'L', 'L');
end eo;

library ieee;
use ieee.std_logic_1164.all;

entity rq is
  port (rmemcc : linkage std_logic_vector(3 downto 2); iaebdxgpx : linkage real; k : out std_logic);
end rq;

library ieee;
use ieee.std_logic_1164.all;

architecture d of rq is
  signal xmv : bit_vector(1 to 0);
  signal hdaomlnj : std_logic_vector(1 to 4);
  signal csjirusde : time;
  signal ol : bit_vector(1 to 0);
  signal my : std_logic_vector(1 to 4);
  signal argt : time;
begin
  ivndbdld : entity work.ukq
    port map (hxeeof => argt, ojlsxuo => my, y => ol);
  crk : entity work.ukq
    port map (hxeeof => csjirusde, ojlsxuo => hdaomlnj, y => xmv);
  
  -- Single-driven assignments
  ol <= ol;
  xmv <= (others => '0');
  
  -- Multi-driven assignments
  hdaomlnj <= my;
end d;



-- Seed after: 13155000777658738853,2230106469645304029
