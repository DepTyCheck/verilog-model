-- Seed: 10066573199011242558,9099465110516446487

library ieee;
use ieee.std_logic_1164.all;

entity htov is
  port (pljr : out std_logic_vector(2 to 4));
end htov;

architecture memfrd of htov is
  
begin
  -- Multi-driven assignments
  pljr <= "-X-";
  pljr <= pljr;
end memfrd;



-- Seed after: 8895482304243551136,9099465110516446487
