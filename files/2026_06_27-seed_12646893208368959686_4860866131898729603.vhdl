-- Seed: 12646893208368959686,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity exx is
  port (azn : out std_logic);
end exx;

architecture wcl of exx is
  
begin
  -- Multi-driven assignments
  azn <= 'X';
  azn <= 'L';
  azn <= 'H';
end wcl;

entity o is
  port (r : out boolean);
end o;

architecture qu of o is
  
begin
  -- Single-driven assignments
  r <= TRUE;
end qu;



-- Seed after: 2683773977759141863,4860866131898729603
