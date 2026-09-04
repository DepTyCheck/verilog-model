-- Seed: 18003242531286223610,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity rurtx is
  port (gif : in std_logic_vector(2 to 4));
end rurtx;

architecture sbarz of rurtx is
  
begin
  
end sbarz;

entity h is
  port (royoujhno : out time);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture ymttfhbvwc of h is
  signal kxzkr : std_logic_vector(2 to 4);
  signal fjvqp : std_logic_vector(2 to 4);
begin
  iympkfnh : entity work.rurtx
    port map (gif => fjvqp);
  grdsblckkj : entity work.rurtx
    port map (gif => kxzkr);
  
  -- Single-driven assignments
  royoujhno <= royoujhno;
  
  -- Multi-driven assignments
  fjvqp <= "-ZU";
  kxzkr <= fjvqp;
  fjvqp <= ('X', 'X', 'L');
  fjvqp <= ('U', 'U', '1');
end ymttfhbvwc;



-- Seed after: 15285263455450723467,4404421571376382767
