-- Seed: 8952790672532251562,14141408946471626091

library ieee;
use ieee.std_logic_1164.all;

entity cka is
  port (lgsr : buffer real; zg : inout std_logic; gjxzppv : out time);
end cka;

architecture w of cka is
  
begin
  -- Single-driven assignments
  gjxzppv <= 16#1.1908# us;
  lgsr <= 8#5.05#;
  
  -- Multi-driven assignments
  zg <= '0';
  zg <= zg;
  zg <= zg;
end w;

library ieee;
use ieee.std_logic_1164.all;

entity pekdilv is
  port (god : inout std_logic_vector(0 downto 0); ihbox : inout severity_level);
end pekdilv;

library ieee;
use ieee.std_logic_1164.all;

architecture eekzonehvz of pekdilv is
  signal gamdahynyq : time;
  signal bnu : real;
  signal aewlm : time;
  signal qq : real;
  signal ys : time;
  signal vlk : std_logic;
  signal mze : real;
begin
  gyrrx : entity work.cka
    port map (lgsr => mze, zg => vlk, gjxzppv => ys);
  uqgntfhe : entity work.cka
    port map (lgsr => qq, zg => vlk, gjxzppv => aewlm);
  xjgyyohjc : entity work.cka
    port map (lgsr => bnu, zg => vlk, gjxzppv => gamdahynyq);
  
  -- Multi-driven assignments
  god <= god;
end eekzonehvz;



-- Seed after: 12533066137142530943,14141408946471626091
