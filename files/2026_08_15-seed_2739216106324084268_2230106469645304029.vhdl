-- Seed: 2739216106324084268,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity nedpe is
  port (w : out real; tnz : out time; wrvfb : inout std_logic_vector(0 downto 2); qtdbtap : inout real);
end nedpe;

architecture nwqcfon of nedpe is
  
begin
  -- Single-driven assignments
  w <= qtdbtap;
  
  -- Multi-driven assignments
  wrvfb <= wrvfb;
  wrvfb <= (others => '0');
end nwqcfon;



-- Seed after: 15652050861034400770,2230106469645304029
