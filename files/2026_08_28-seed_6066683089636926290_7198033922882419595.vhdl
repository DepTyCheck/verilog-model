-- Seed: 6066683089636926290,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (xljmfx : linkage std_logic; nuhayl : inout time_vector(3 to 4); psvijcgg : inout std_logic);
end y;

architecture xk of y is
  
begin
  -- Multi-driven assignments
  psvijcgg <= 'Z';
  psvijcgg <= 'H';
end xk;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (guqjkw : out std_logic_vector(2 downto 0));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture w of p is
  signal ak : time_vector(3 to 4);
  signal ajdhr : time_vector(3 to 4);
  signal e : std_logic;
begin
  msgkcwjufh : entity work.y
    port map (xljmfx => e, nuhayl => ajdhr, psvijcgg => e);
  s : entity work.y
    port map (xljmfx => e, nuhayl => ak, psvijcgg => e);
  
  -- Multi-driven assignments
  guqjkw <= ('W', 'L', 'H');
  guqjkw <= guqjkw;
  guqjkw <= "WW-";
end w;

entity qywbudiptq is
  port (gtggmxtml : out time);
end qywbudiptq;

library ieee;
use ieee.std_logic_1164.all;

architecture n of qywbudiptq is
  signal gfhqc : time_vector(3 to 4);
  signal sx : std_logic;
  signal danrzfiipe : std_logic_vector(2 downto 0);
  signal gttg : std_logic;
  signal pxfoz : time_vector(3 to 4);
  signal uplnrmrpg : std_logic;
  signal hbwoyuad : std_logic_vector(2 downto 0);
begin
  bdrrru : entity work.p
    port map (guqjkw => hbwoyuad);
  bnowjbfg : entity work.y
    port map (xljmfx => uplnrmrpg, nuhayl => pxfoz, psvijcgg => gttg);
  thirimo : entity work.p
    port map (guqjkw => danrzfiipe);
  wcypl : entity work.y
    port map (xljmfx => sx, nuhayl => gfhqc, psvijcgg => gttg);
  
  -- Single-driven assignments
  gtggmxtml <= 4.340 us;
  
  -- Multi-driven assignments
  danrzfiipe <= "ZU0";
  hbwoyuad <= "HLH";
end n;



-- Seed after: 11488234068969280796,7198033922882419595
