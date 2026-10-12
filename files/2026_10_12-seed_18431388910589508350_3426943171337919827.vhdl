-- Seed: 18431388910589508350,3426943171337919827

entity tkzfnxgd is
  port (jlwl : linkage integer);
end tkzfnxgd;

architecture a of tkzfnxgd is
  
begin
  
end a;

library ieee;
use ieee.std_logic_1164.all;

entity uozknu is
  port (dhforlwlq : inout std_logic_vector(4 downto 1); p : linkage time);
end uozknu;

architecture d of uozknu is
  signal bqxfdfp : integer;
begin
  v : entity work.tkzfnxgd
    port map (jlwl => bqxfdfp);
  
  -- Multi-driven assignments
  dhforlwlq <= ('W', 'Z', 'X', 'L');
  dhforlwlq <= "0Z1L";
  dhforlwlq <= "HUL1";
end d;

library ieee;
use ieee.std_logic_1164.all;

entity uvpjlqtaf is
  port (sgjuvql : buffer boolean; dzwjcm : buffer std_logic_vector(2 downto 1); khod : buffer time);
end uvpjlqtaf;

library ieee;
use ieee.std_logic_1164.all;

architecture jwv of uvpjlqtaf is
  signal t : integer;
  signal zlumureskn : std_logic_vector(4 downto 1);
  signal hnoqcxv : time;
  signal jfuz : std_logic_vector(4 downto 1);
  signal lap : integer;
begin
  kclptkobxx : entity work.tkzfnxgd
    port map (jlwl => lap);
  pjtsdqe : entity work.uozknu
    port map (dhforlwlq => jfuz, p => hnoqcxv);
  fvmyeigx : entity work.uozknu
    port map (dhforlwlq => zlumureskn, p => khod);
  htjvta : entity work.tkzfnxgd
    port map (jlwl => t);
  
  -- Single-driven assignments
  sgjuvql <= sgjuvql;
  
  -- Multi-driven assignments
  dzwjcm <= dzwjcm;
  jfuz <= jfuz;
end jwv;



-- Seed after: 13772039063406574094,3426943171337919827
