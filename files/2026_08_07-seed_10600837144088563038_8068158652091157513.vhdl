-- Seed: 10600837144088563038,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity catf is
  port (fuzvfuzy : in time; qbjkfuqe : in std_logic_vector(3 to 3));
end catf;

architecture js of catf is
  
begin
  
end js;

entity qlzmck is
  port (oqjqv : in boolean; jmky : out time);
end qlzmck;

library ieee;
use ieee.std_logic_1164.all;

architecture fmncjghbrz of qlzmck is
  signal emik : std_logic_vector(3 to 3);
  signal nqrwvrawb : time;
  signal hucdq : std_logic_vector(3 to 3);
begin
  drd : entity work.catf
    port map (fuzvfuzy => jmky, qbjkfuqe => hucdq);
  tdhcy : entity work.catf
    port map (fuzvfuzy => nqrwvrawb, qbjkfuqe => emik);
  
  -- Multi-driven assignments
  hucdq <= emik;
end fmncjghbrz;

library ieee;
use ieee.std_logic_1164.all;

entity khg is
  port (fgszdjgwoa : inout bit; drq : in integer; azhvfa : buffer std_logic);
end khg;

architecture ch of khg is
  
begin
  -- Single-driven assignments
  fgszdjgwoa <= fgszdjgwoa;
  
  -- Multi-driven assignments
  azhvfa <= 'Z';
end ch;



-- Seed after: 4257336013149046122,8068158652091157513
