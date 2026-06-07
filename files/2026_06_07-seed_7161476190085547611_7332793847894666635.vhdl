-- Seed: 7161476190085547611,7332793847894666635



entity jaglx is
  port (ravotzjm : buffer bit);
end jaglx;



architecture aesdatn of jaglx is
  
begin
  
end aesdatn;

library ieee;
use ieee.std_logic_1164.all;

entity twnj is
  port (lnugeptyev : out time; kd : linkage time; pilmdo : out std_logic_vector(0 downto 4); vlxljxzck : out character);
end twnj;



architecture pa of twnj is
  signal bxjtkkjry : bit;
begin
  ykbeemjzh : entity work.jaglx
    port map (ravotzjm => bxjtkkjry);
end pa;



entity aahj is
  port (xupmviee : out character; sr : linkage time);
end aahj;

library ieee;
use ieee.std_logic_1164.all;

architecture hmkzdbihin of aahj is
  signal kdbf : bit;
  signal ap : std_logic_vector(0 downto 4);
  signal cki : time;
  signal kbreigf : time;
begin
  qikbuauk : entity work.twnj
    port map (lnugeptyev => kbreigf, kd => cki, pilmdo => ap, vlxljxzck => xupmviee);
  gyvlxyik : entity work.jaglx
    port map (ravotzjm => kdbf);
end hmkzdbihin;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (up : linkage std_logic_vector(0 to 2); uwkopowh : linkage time);
end q;



architecture ax of q is
  signal eud : time;
  signal liu : character;
begin
  pebfrf : entity work.aahj
    port map (xupmviee => liu, sr => eud);
end ax;



-- Seed after: 7822258275187294414,7332793847894666635
