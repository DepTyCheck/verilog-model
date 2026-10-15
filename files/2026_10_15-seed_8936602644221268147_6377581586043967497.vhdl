-- Seed: 8936602644221268147,6377581586043967497

entity hbyma is
  port (sy : inout real; gneolnoenm : linkage string(5 to 3); v : in severity_level);
end hbyma;

architecture ddck of hbyma is
  
begin
  
end ddck;

library ieee;
use ieee.std_logic_1164.all;

entity ehji is
  port (dh : buffer std_logic; etxexblv : in integer);
end ehji;

architecture fx of ehji is
  signal sa : string(5 to 3);
  signal bbquvmd : real;
  signal hviuxmcug : string(5 to 3);
  signal pk : real;
  signal uwf : string(5 to 3);
  signal aqe : real;
  signal cwkng : severity_level;
  signal x : string(5 to 3);
  signal c : real;
begin
  v : entity work.hbyma
    port map (sy => c, gneolnoenm => x, v => cwkng);
  zji : entity work.hbyma
    port map (sy => aqe, gneolnoenm => uwf, v => cwkng);
  bqrv : entity work.hbyma
    port map (sy => pk, gneolnoenm => hviuxmcug, v => cwkng);
  d : entity work.hbyma
    port map (sy => bbquvmd, gneolnoenm => sa, v => cwkng);
  
  -- Multi-driven assignments
  dh <= 'W';
  dh <= dh;
  dh <= dh;
  dh <= '-';
end fx;



-- Seed after: 15209971416347953276,6377581586043967497
