-- Seed: 11784518404648824246,2511821214772927453

entity wwa is
  port (ohs : buffer boolean_vector(2 to 1); b : in real);
end wwa;

architecture bgc of wwa is
  
begin
  -- Single-driven assignments
  ohs <= (others => TRUE);
end bgc;

library ieee;
use ieee.std_logic_1164.all;

entity dblgdjtqwl is
  port (doxbjhbje : linkage std_logic_vector(1 to 2); dudgyqh : linkage real);
end dblgdjtqwl;

architecture g of dblgdjtqwl is
  signal jdm : real;
  signal rciucil : boolean_vector(2 to 1);
  signal sh : real;
  signal x : boolean_vector(2 to 1);
begin
  u : entity work.wwa
    port map (ohs => x, b => sh);
  nyxlfzvw : entity work.wwa
    port map (ohs => rciucil, b => jdm);
  
  -- Single-driven assignments
  sh <= 8#25376.7#;
  jdm <= sh;
end g;



-- Seed after: 1363003908496209586,2511821214772927453
