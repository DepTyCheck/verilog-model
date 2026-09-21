-- Seed: 14637534384680197957,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity biwyyw is
  port (eixrimqt : buffer std_logic; ulxdrw : out std_logic; jucjdeb : out integer; amzxw : inout time);
end biwyyw;

architecture okpvnetab of biwyyw is
  
begin
  -- Single-driven assignments
  amzxw <= 3 sec;
  
  -- Multi-driven assignments
  ulxdrw <= ulxdrw;
  eixrimqt <= ulxdrw;
  eixrimqt <= ulxdrw;
end okpvnetab;

entity libzgw is
  port (evz : buffer real);
end libzgw;

library ieee;
use ieee.std_logic_1164.all;

architecture poraf of libzgw is
  signal dh : time;
  signal nnnwftxck : integer;
  signal ykdhwmh : time;
  signal teheql : integer;
  signal lna : std_logic;
begin
  homw : entity work.biwyyw
    port map (eixrimqt => lna, ulxdrw => lna, jucjdeb => teheql, amzxw => ykdhwmh);
  zv : entity work.biwyyw
    port map (eixrimqt => lna, ulxdrw => lna, jucjdeb => nnnwftxck, amzxw => dh);
  
  -- Single-driven assignments
  evz <= 2#01.00#;
end poraf;



-- Seed after: 5595652219703836926,12143220691580258643
