-- Seed: 8406330387395050544,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity dxb is
  port (ikgxplzmg : out std_logic; jckn : inout integer);
end dxb;

architecture ke of dxb is
  
begin
  -- Single-driven assignments
  jckn <= 3_4_4_4;
  
  -- Multi-driven assignments
  ikgxplzmg <= ikgxplzmg;
  ikgxplzmg <= ikgxplzmg;
  ikgxplzmg <= ikgxplzmg;
  ikgxplzmg <= 'U';
end ke;

library ieee;
use ieee.std_logic_1164.all;

entity oizykfh is
  port (cjmrahgsu : inout std_logic_vector(4 to 1); rqluvw : in std_logic_vector(0 to 3));
end oizykfh;

library ieee;
use ieee.std_logic_1164.all;

architecture hspl of oizykfh is
  signal hsyxxt : integer;
  signal gfnsga : std_logic;
begin
  gtu : entity work.dxb
    port map (ikgxplzmg => gfnsga, jckn => hsyxxt);
end hspl;



-- Seed after: 1387813726442899412,3400751927341804175
