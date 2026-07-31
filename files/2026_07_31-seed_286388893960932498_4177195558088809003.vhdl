-- Seed: 286388893960932498,4177195558088809003

entity wfdqxqvm is
  port (mybsyyh : out time);
end wfdqxqvm;

architecture hzyxm of wfdqxqvm is
  
begin
  
end hzyxm;

library ieee;
use ieee.std_logic_1164.all;

entity zyn is
  port (kzmp : in time; hysuevz : inout std_logic; fa : buffer std_logic);
end zyn;

architecture dklar of zyn is
  signal tyhb : time;
begin
  ajxit : entity work.wfdqxqvm
    port map (mybsyyh => tyhb);
  
  -- Multi-driven assignments
  fa <= 'X';
end dklar;

entity sopepbvzcb is
  port (voibwlhwi : buffer character);
end sopepbvzcb;

library ieee;
use ieee.std_logic_1164.all;

architecture zwrfu of sopepbvzcb is
  signal ymou : std_logic;
  signal nqpso : time;
  signal baqluhhqnq : time;
begin
  lelwzplf : entity work.wfdqxqvm
    port map (mybsyyh => baqluhhqnq);
  kdkxla : entity work.zyn
    port map (kzmp => nqpso, hysuevz => ymou, fa => ymou);
  aeh : entity work.wfdqxqvm
    port map (mybsyyh => nqpso);
  
  -- Single-driven assignments
  voibwlhwi <= 'g';
  
  -- Multi-driven assignments
  ymou <= ymou;
  ymou <= ymou;
end zwrfu;



-- Seed after: 8324539433112605871,4177195558088809003
