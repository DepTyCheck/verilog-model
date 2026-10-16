-- Seed: 17221326094041560855,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity pwmmt is
  port (ovqw : out std_logic_vector(1 to 3));
end pwmmt;

architecture ycvjuvdp of pwmmt is
  
begin
  -- Multi-driven assignments
  ovqw <= ovqw;
end ycvjuvdp;

library ieee;
use ieee.std_logic_1164.all;

entity qww is
  port (r : out real; lzwwf : inout real; xjfre : buffer boolean; cacmpdcogp : in std_logic_vector(4 to 1));
end qww;

architecture w of qww is
  
begin
  -- Single-driven assignments
  xjfre <= TRUE;
  r <= 2#1010.00111#;
  lzwwf <= lzwwf;
end w;

entity elntpts is
  port (qxfmp : out real; qzwc : out severity_level);
end elntpts;

library ieee;
use ieee.std_logic_1164.all;

architecture ccvzkfvfzh of elntpts is
  signal jmcua : std_logic_vector(1 to 3);
  signal kbcdxibw : std_logic_vector(4 to 1);
  signal lhxuffzfbh : boolean;
  signal dq : real;
begin
  h : entity work.qww
    port map (r => dq, lzwwf => qxfmp, xjfre => lhxuffzfbh, cacmpdcogp => kbcdxibw);
  hsbtt : entity work.pwmmt
    port map (ovqw => jmcua);
  dimdfvlltn : entity work.pwmmt
    port map (ovqw => jmcua);
  
  -- Single-driven assignments
  qzwc <= qzwc;
  
  -- Multi-driven assignments
  kbcdxibw <= kbcdxibw;
  kbcdxibw <= kbcdxibw;
  kbcdxibw <= (others => '0');
  kbcdxibw <= (others => '0');
end ccvzkfvfzh;



-- Seed after: 13025798312485215782,9900838946149614537
