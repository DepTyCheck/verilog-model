-- Seed: 11220032531215156689,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity ymctbfct is
  port (lwbzwresc : linkage std_logic_vector(3 to 1); r : in std_logic_vector(1 to 3); gl : in time);
end ymctbfct;

architecture bdjkn of ymctbfct is
  
begin
  
end bdjkn;

entity ugkztpqp is
  port (i : out integer; tbgqbgwoh : buffer real_vector(4 to 4));
end ugkztpqp;

library ieee;
use ieee.std_logic_1164.all;

architecture j of ugkztpqp is
  signal svi : time;
  signal tp : std_logic_vector(1 to 3);
  signal ojdgytsogv : time;
  signal ev : std_logic_vector(3 to 1);
  signal iac : time;
  signal ppd : std_logic_vector(1 to 3);
  signal dvtmfjfk : std_logic_vector(3 to 1);
begin
  dgmpvbp : entity work.ymctbfct
    port map (lwbzwresc => dvtmfjfk, r => ppd, gl => iac);
  ihk : entity work.ymctbfct
    port map (lwbzwresc => ev, r => ppd, gl => ojdgytsogv);
  lsqu : entity work.ymctbfct
    port map (lwbzwresc => ev, r => tp, gl => svi);
  
  -- Single-driven assignments
  ojdgytsogv <= 1331.4043 fs;
  
  -- Multi-driven assignments
  ppd <= "LWZ";
end j;



-- Seed after: 16168488978307872910,2287986438313874379
