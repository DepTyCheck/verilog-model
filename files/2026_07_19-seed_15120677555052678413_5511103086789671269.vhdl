-- Seed: 15120677555052678413,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity jnrusbyh is
  port (mlafyx : inout time_vector(2 downto 1); prlzoqoim : buffer std_logic);
end jnrusbyh;

architecture aqoiyblgl of jnrusbyh is
  
begin
  -- Single-driven assignments
  mlafyx <= (16#D4# fs, 2#0_0_1_0# us);
end aqoiyblgl;

entity h is
  port (oxrui : buffer integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture kbflkuql of h is
  signal v : std_logic;
  signal uxfaemcb : time_vector(2 downto 1);
  signal zn : std_logic;
  signal surzmiooy : time_vector(2 downto 1);
  signal lkzgkd : std_logic;
  signal dhbszgycqn : time_vector(2 downto 1);
begin
  vcz : entity work.jnrusbyh
    port map (mlafyx => dhbszgycqn, prlzoqoim => lkzgkd);
  df : entity work.jnrusbyh
    port map (mlafyx => surzmiooy, prlzoqoim => zn);
  vwzhobre : entity work.jnrusbyh
    port map (mlafyx => uxfaemcb, prlzoqoim => v);
  
  -- Single-driven assignments
  oxrui <= 16#4FB0C#;
  
  -- Multi-driven assignments
  lkzgkd <= lkzgkd;
  lkzgkd <= lkzgkd;
end kbflkuql;



-- Seed after: 9753362997385839822,5511103086789671269
