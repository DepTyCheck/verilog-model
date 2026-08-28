-- Seed: 4925167964594729175,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity oaxw is
  port (bohwdakh : buffer time; a : inout std_logic_vector(4 downto 0));
end oaxw;

architecture bp of oaxw is
  
begin
  -- Single-driven assignments
  bohwdakh <= 8#3_1_0.2_7_3# fs;
  
  -- Multi-driven assignments
  a <= a;
  a <= a;
  a <= a;
  a <= "0XLH-";
end bp;



-- Seed after: 14177327639504847938,7198033922882419595
