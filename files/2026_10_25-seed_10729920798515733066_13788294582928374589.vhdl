-- Seed: 10729920798515733066,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity hekclfruk is
  port (cofpkp : buffer std_logic_vector(0 to 4); ahbby : buffer std_logic);
end hekclfruk;

architecture lb of hekclfruk is
  
begin
  -- Multi-driven assignments
  ahbby <= 'H';
  ahbby <= ahbby;
end lb;



-- Seed after: 9054512237217128400,13788294582928374589
