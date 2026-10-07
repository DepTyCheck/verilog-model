-- Seed: 15155816894305327806,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity xplk is
  port (dzjdk : linkage std_logic; sj : inout std_logic_vector(3 downto 1); dnvob : in time; vijondbudg : inout real_vector(1 to 4));
end xplk;

architecture ef of xplk is
  
begin
  -- Single-driven assignments
  vijondbudg <= vijondbudg;
end ef;

entity dgngwn is
  port (pyqrr : linkage integer; kfshfucnxs : out real);
end dgngwn;

library ieee;
use ieee.std_logic_1164.all;

architecture jz of dgngwn is
  signal u : real_vector(1 to 4);
  signal t : std_logic_vector(3 downto 1);
  signal ipowjd : real_vector(1 to 4);
  signal rpbz : time;
  signal tk : real_vector(1 to 4);
  signal m : time;
  signal uovmpjabm : std_logic_vector(3 downto 1);
  signal kydpz : std_logic;
begin
  ssd : entity work.xplk
    port map (dzjdk => kydpz, sj => uovmpjabm, dnvob => m, vijondbudg => tk);
  ki : entity work.xplk
    port map (dzjdk => kydpz, sj => uovmpjabm, dnvob => rpbz, vijondbudg => ipowjd);
  ev : entity work.xplk
    port map (dzjdk => kydpz, sj => t, dnvob => m, vijondbudg => u);
  
  -- Single-driven assignments
  kfshfucnxs <= 8#0774.1_2_0_0_5#;
  rpbz <= 8#57.1060# us;
  m <= 4 sec;
end jz;



-- Seed after: 12997158964130910977,5906004015519833893
