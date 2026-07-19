-- Seed: 6636455130168088379,5511103086789671269

entity tpdoxwhnn is
  port (fr : linkage string(3 downto 4));
end tpdoxwhnn;

architecture z of tpdoxwhnn is
  
begin
  
end z;

library ieee;
use ieee.std_logic_1164.all;

entity huoayr is
  port (xhvbw : out std_logic_vector(4 to 1); wcmbiqpwsu : linkage boolean_vector(1 downto 3); nfrwkbavqf : out std_logic_vector(0 downto 0));
end huoayr;

architecture w of huoayr is
  signal xmrqfdh : string(3 downto 4);
  signal rzsqyty : string(3 downto 4);
  signal lrib : string(3 downto 4);
begin
  fgcjzgvuzt : entity work.tpdoxwhnn
    port map (fr => lrib);
  wnv : entity work.tpdoxwhnn
    port map (fr => rzsqyty);
  dreb : entity work.tpdoxwhnn
    port map (fr => xmrqfdh);
  
  -- Multi-driven assignments
  nfrwkbavqf <= nfrwkbavqf;
  xhvbw <= xhvbw;
  nfrwkbavqf <= nfrwkbavqf;
  nfrwkbavqf <= nfrwkbavqf;
end w;



-- Seed after: 17781908614424144641,5511103086789671269
