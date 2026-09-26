-- Seed: 2203387498383474313,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (qibmzuahr : out std_logic_vector(4 to 4); mogaiwg : linkage time; qhmqptj : buffer time; fcov : out std_logic);
end l;

architecture eujmayj of l is
  
begin
  -- Single-driven assignments
  qhmqptj <= qhmqptj;
  
  -- Multi-driven assignments
  fcov <= 'Z';
end eujmayj;

entity inpwyk is
  port (ss : inout character);
end inpwyk;

library ieee;
use ieee.std_logic_1164.all;

architecture dhu of inpwyk is
  signal b : time;
  signal wmsostjc : time;
  signal lq : time;
  signal vqe : time;
  signal dfwsvf : std_logic;
  signal xeuuhkh : time;
  signal adj : time;
  signal g : std_logic_vector(4 to 4);
begin
  os : entity work.l
    port map (qibmzuahr => g, mogaiwg => adj, qhmqptj => xeuuhkh, fcov => dfwsvf);
  wbounesmq : entity work.l
    port map (qibmzuahr => g, mogaiwg => vqe, qhmqptj => lq, fcov => dfwsvf);
  fjqfcfxr : entity work.l
    port map (qibmzuahr => g, mogaiwg => wmsostjc, qhmqptj => b, fcov => dfwsvf);
  
  -- Multi-driven assignments
  dfwsvf <= 'W';
end dhu;



-- Seed after: 13080474725405519485,10875537289884587119
