-- Seed: 7693804980686230310,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity trcrafirzr is
  port (csolu : inout std_logic_vector(3 to 2); mgp : buffer std_logic_vector(4 to 1); dvv : in real; px : buffer severity_level);
end trcrafirzr;

architecture rua of trcrafirzr is
  
begin
  -- Single-driven assignments
  px <= FAILURE;
  
  -- Multi-driven assignments
  mgp <= "";
  mgp <= "";
end rua;



-- Seed after: 9857900626682987799,12011142928354116943
