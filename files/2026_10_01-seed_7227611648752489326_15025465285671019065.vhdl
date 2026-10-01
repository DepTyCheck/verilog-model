-- Seed: 7227611648752489326,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity xjxaze is
  port (kyabmn : in std_logic_vector(4 downto 0); qa : buffer std_logic_vector(0 downto 4); blvjeuvmkf : inout boolean_vector(0 to 4));
end xjxaze;

architecture mp of xjxaze is
  
begin
  -- Single-driven assignments
  blvjeuvmkf <= (FALSE, TRUE, TRUE, TRUE, FALSE);
  
  -- Multi-driven assignments
  qa <= qa;
  qa <= qa;
  qa <= "";
  qa <= (others => '0');
end mp;



-- Seed after: 7838121272612588516,15025465285671019065
