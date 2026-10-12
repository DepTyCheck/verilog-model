-- Seed: 18058059430283100239,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity kv is
  port (m : buffer std_logic_vector(2 to 1); b : inout boolean_vector(1 downto 2); rtjo : linkage integer; asdbup : in std_logic);
end kv;

architecture mkcuxusf of kv is
  
begin
  -- Single-driven assignments
  b <= (others => TRUE);
  
  -- Multi-driven assignments
  m <= m;
  m <= m;
  m <= (others => '0');
  m <= (others => '0');
end mkcuxusf;



-- Seed after: 1962040920628999317,3426943171337919827
