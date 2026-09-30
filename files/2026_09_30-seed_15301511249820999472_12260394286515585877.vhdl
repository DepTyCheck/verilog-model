-- Seed: 15301511249820999472,12260394286515585877

library ieee;
use ieee.std_logic_1164.all;

entity lxxz is
  port (upj : buffer real; kklbun : inout std_logic_vector(3 downto 4); excojgrdeh : out time_vector(3 downto 1));
end lxxz;

architecture ukce of lxxz is
  
begin
  -- Single-driven assignments
  upj <= 12.1_3_3_2_1;
  excojgrdeh <= (8#4# ms, 4 min, 8#20.7# us);
end ukce;

entity xmjhggryu is
  port (a : in real_vector(1 downto 3); ladhtigkg : in time);
end xmjhggryu;

library ieee;
use ieee.std_logic_1164.all;

architecture vjl of xmjhggryu is
  signal u : time_vector(3 downto 1);
  signal ypbvokshu : real;
  signal hifhhx : time_vector(3 downto 1);
  signal wasjs : std_logic_vector(3 downto 4);
  signal xn : real;
begin
  vxtncfor : entity work.lxxz
    port map (upj => xn, kklbun => wasjs, excojgrdeh => hifhhx);
  qo : entity work.lxxz
    port map (upj => ypbvokshu, kklbun => wasjs, excojgrdeh => u);
  
  -- Multi-driven assignments
  wasjs <= wasjs;
end vjl;

entity wljsrsrj is
  port (ryp : inout boolean);
end wljsrsrj;

architecture yzd of wljsrsrj is
  
begin
  -- Single-driven assignments
  ryp <= FALSE;
end yzd;



-- Seed after: 49561866932688455,12260394286515585877
