-- Seed: 6861146145001729327,2158184632809654795

entity ctawwkru is
  port (qnv : inout integer);
end ctawwkru;

architecture miafenl of ctawwkru is
  
begin
  -- Single-driven assignments
  qnv <= 8#02363#;
end miafenl;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (jeye : out std_logic_vector(1 downto 1));
end g;

architecture ymgkipykqh of g is
  signal nszjooe : integer;
begin
  lcdtma : entity work.ctawwkru
    port map (qnv => nszjooe);
  
  -- Multi-driven assignments
  jeye <= "W";
  jeye <= jeye;
  jeye <= "U";
end ymgkipykqh;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity qnldmt is
  port (xwrqskfj : inout std_logic_vector(3 to 1); ooezrwl : inout physical_value_mirror);
end qnldmt;

library ieee;
use ieee.std_logic_1164.all;

architecture vjeumls of qnldmt is
  signal aqrdzvh : integer;
  signal hlb : std_logic_vector(1 downto 1);
begin
  vvsmekfxib : entity work.g
    port map (jeye => hlb);
  arqvgricdo : entity work.ctawwkru
    port map (qnv => aqrdzvh);
  p : entity work.g
    port map (jeye => hlb);
  
  -- Multi-driven assignments
  xwrqskfj <= (others => '0');
  xwrqskfj <= "";
end vjeumls;

use std.reflection.all;

entity jpdsyp is
  port (s : inout file_value_mirror; crtaignmwp : inout record_subtype_mirror);
end jpdsyp;

architecture yrif of jpdsyp is
  
begin
  
end yrif;



-- Seed after: 12498980963428636146,2158184632809654795
