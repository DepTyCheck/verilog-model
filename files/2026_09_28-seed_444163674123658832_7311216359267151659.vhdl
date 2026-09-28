-- Seed: 444163674123658832,7311216359267151659

entity vfcrhfzy is
  port (wogyxk : in time_vector(0 to 2));
end vfcrhfzy;

architecture pd of vfcrhfzy is
  
begin
  
end pd;

entity t is
  port (jkizcwpv : linkage real; n : out boolean);
end t;

architecture iwct of t is
  signal z : time_vector(0 to 2);
begin
  jdmrp : entity work.vfcrhfzy
    port map (wogyxk => z);
  gt : entity work.vfcrhfzy
    port map (wogyxk => z);
  
  -- Single-driven assignments
  n <= TRUE;
  z <= z;
end iwct;

library ieee;
use ieee.std_logic_1164.all;

entity ax is
  port (mviugzu : inout std_logic);
end ax;

architecture xbsa of ax is
  signal dfza : time_vector(0 to 2);
  signal mjukggvia : time_vector(0 to 2);
begin
  yb : entity work.vfcrhfzy
    port map (wogyxk => mjukggvia);
  rfedredlko : entity work.vfcrhfzy
    port map (wogyxk => dfza);
  
  -- Single-driven assignments
  mjukggvia <= dfza;
  
  -- Multi-driven assignments
  mviugzu <= mviugzu;
end xbsa;



-- Seed after: 8116267087006513184,7311216359267151659
