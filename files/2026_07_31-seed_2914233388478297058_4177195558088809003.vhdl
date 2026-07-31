-- Seed: 2914233388478297058,4177195558088809003

entity noygubrt is
  port (pyhl : inout bit_vector(4 downto 2); judfrdv : out integer; parnbegg : inout time);
end noygubrt;

architecture rvdmxi of noygubrt is
  
begin
  -- Single-driven assignments
  pyhl <= ('1', '0', '0');
  parnbegg <= 21330 us;
end rvdmxi;

entity ilepwob is
  port (yqwsdjd : in boolean; ekkflhmni : in integer; akbsj : buffer time_vector(2 to 3));
end ilepwob;

architecture ahy of ilepwob is
  signal nbs : time;
  signal ia : integer;
  signal pjxjvcqx : bit_vector(4 downto 2);
  signal jpffqbkcro : time;
  signal ojvfewq : integer;
  signal bjxzqy : bit_vector(4 downto 2);
begin
  tzrzwh : entity work.noygubrt
    port map (pyhl => bjxzqy, judfrdv => ojvfewq, parnbegg => jpffqbkcro);
  podvvgrpfo : entity work.noygubrt
    port map (pyhl => pjxjvcqx, judfrdv => ia, parnbegg => nbs);
  
  -- Single-driven assignments
  akbsj <= (1_1_4_3 ms, 2#1_0.0_0_0# ps);
end ahy;

library ieee;
use ieee.std_logic_1164.all;

entity ee is
  port (p : inout std_logic_vector(4 to 0); lqhojbvm : inout integer);
end ee;

architecture g of ee is
  
begin
  -- Single-driven assignments
  lqhojbvm <= lqhojbvm;
  
  -- Multi-driven assignments
  p <= (others => '0');
end g;



-- Seed after: 2207957877265122647,4177195558088809003
