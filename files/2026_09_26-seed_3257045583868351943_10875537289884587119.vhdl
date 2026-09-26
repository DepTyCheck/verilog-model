-- Seed: 3257045583868351943,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity svonimvc is
  port (jp : out std_logic; mr : in severity_level; ict : buffer time; crceg : in std_logic_vector(1 downto 0));
end svonimvc;

architecture ul of svonimvc is
  
begin
  -- Single-driven assignments
  ict <= ict;
  
  -- Multi-driven assignments
  jp <= 'W';
  jp <= 'X';
  jp <= 'W';
  jp <= '1';
end ul;

library ieee;
use ieee.std_logic_1164.all;

entity sghlh is
  port ( mgglnie : inout real_vector(2 downto 3)
  ; krndijzm : out real_vector(4 to 4)
  ; mxahwkmkaa : out std_logic_vector(3 downto 3)
  ; kpmfh : linkage bit
  );
end sghlh;

library ieee;
use ieee.std_logic_1164.all;

architecture svh of sghlh is
  signal ksvyxuxr : std_logic_vector(1 downto 0);
  signal bre : time;
  signal gichya : severity_level;
  signal yfq : std_logic;
begin
  qwokh : entity work.svonimvc
    port map (jp => yfq, mr => gichya, ict => bre, crceg => ksvyxuxr);
  
  -- Multi-driven assignments
  mxahwkmkaa <= mxahwkmkaa;
end svh;

entity mdppqxznye is
  port (dptpdakv : in severity_level; etgcx : in integer_vector(0 to 2); isfwsqu : in time);
end mdppqxznye;

library ieee;
use ieee.std_logic_1164.all;

architecture yjosk of mdppqxznye is
  signal sutuelw : std_logic_vector(1 downto 0);
  signal ivorsovc : time;
  signal xbqlexhg : std_logic;
begin
  qzhkdwh : entity work.svonimvc
    port map (jp => xbqlexhg, mr => dptpdakv, ict => ivorsovc, crceg => sutuelw);
  
  -- Multi-driven assignments
  xbqlexhg <= xbqlexhg;
  sutuelw <= "X-";
  xbqlexhg <= 'Z';
  xbqlexhg <= 'X';
end yjosk;



-- Seed after: 11192681866492334276,10875537289884587119
