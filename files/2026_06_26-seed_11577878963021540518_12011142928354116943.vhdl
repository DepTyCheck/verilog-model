-- Seed: 11577878963021540518,12011142928354116943

entity mhuob is
  port (xwkgjc : buffer time; miykz : buffer real);
end mhuob;

architecture bcgqjjbgjz of mhuob is
  
begin
  
end bcgqjjbgjz;

library ieee;
use ieee.std_logic_1164.all;

entity qsw is
  port (h : in integer; ofqrnzhdm : inout std_logic);
end qsw;

architecture wdi of qsw is
  signal zmxiohe : real;
  signal l : time;
  signal ew : real;
  signal tlkssfyu : time;
  signal hkzmpcj : real;
  signal nlrudnhtv : time;
  signal shzqto : real;
  signal bhovljpxqe : time;
begin
  wjxa : entity work.mhuob
    port map (xwkgjc => bhovljpxqe, miykz => shzqto);
  aqs : entity work.mhuob
    port map (xwkgjc => nlrudnhtv, miykz => hkzmpcj);
  byijbmozqp : entity work.mhuob
    port map (xwkgjc => tlkssfyu, miykz => ew);
  jfrb : entity work.mhuob
    port map (xwkgjc => l, miykz => zmxiohe);
  
  -- Multi-driven assignments
  ofqrnzhdm <= '0';
end wdi;

library ieee;
use ieee.std_logic_1164.all;

entity tixhj is
  port (c : out time; zyuilnahi : in real; xuyywtluc : linkage std_logic; ublpoxpdqr : out std_logic);
end tixhj;

architecture lndtqlac of tixhj is
  signal jpsmhc : real;
  signal qiet : real;
  signal aoi : time;
begin
  tdlfbplvi : entity work.mhuob
    port map (xwkgjc => aoi, miykz => qiet);
  azpmz : entity work.mhuob
    port map (xwkgjc => c, miykz => jpsmhc);
  
  -- Multi-driven assignments
  ublpoxpdqr <= 'Z';
end lndtqlac;



-- Seed after: 2824360455175273653,12011142928354116943
