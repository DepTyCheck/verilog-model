-- Seed: 3387159723491100644,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (lbnarzlucj : buffer std_logic; g : out std_logic_vector(0 to 2); grxid : inout real; zvea : out time);
end p;

architecture yk of p is
  
begin
  -- Single-driven assignments
  zvea <= 2 sec;
  
  -- Multi-driven assignments
  g <= g;
  g <= g;
  lbnarzlucj <= 'X';
  lbnarzlucj <= lbnarzlucj;
end yk;



-- Seed after: 10850460009933054342,3400751927341804175
