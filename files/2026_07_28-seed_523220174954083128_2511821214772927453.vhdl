-- Seed: 523220174954083128,2511821214772927453

library ieee;
use ieee.std_logic_1164.all;

entity voukni is
  port (czcpdink : inout std_logic);
end voukni;

architecture volg of voukni is
  
begin
  -- Multi-driven assignments
  czcpdink <= 'Z';
  czcpdink <= 'Z';
  czcpdink <= czcpdink;
  czcpdink <= 'L';
end volg;



-- Seed after: 5814049927793840791,2511821214772927453
