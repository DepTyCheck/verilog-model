-- Seed: 15845541103157818416,4177195558088809003

entity t is
  port (xxgrsx : in time);
end t;

architecture m of t is
  
begin
  
end m;

library ieee;
use ieee.std_logic_1164.all;

entity ptcjrshmxi is
  port (ydamh : inout std_logic_vector(1 downto 0); esj : buffer std_logic; mcxsqi : out std_logic);
end ptcjrshmxi;

architecture hez of ptcjrshmxi is
  signal tanlk : time;
  signal vmfeqzvvfv : time;
begin
  gjyznox : entity work.t
    port map (xxgrsx => vmfeqzvvfv);
  yzcoeydpb : entity work.t
    port map (xxgrsx => tanlk);
  
  -- Single-driven assignments
  vmfeqzvvfv <= tanlk;
  tanlk <= 2.3 fs;
end hez;



-- Seed after: 10811767926331919898,4177195558088809003
