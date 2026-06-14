-- Seed: 14274098349031516968,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity up is
  port (laybkn : in time; pvvjtlc : inout std_logic_vector(4 downto 0));
end up;

architecture qeps of up is
  
begin
  -- Multi-driven assignments
  pvvjtlc <= "L-UZL";
  pvvjtlc <= ('W', 'H', 'H', 'U', 'W');
  pvvjtlc <= ('X', 'U', 'H', '-', '-');
  pvvjtlc <= ('X', 'H', 'W', 'L', 'W');
end qeps;

entity dllmwhsrzl is
  port (c : buffer boolean);
end dllmwhsrzl;

library ieee;
use ieee.std_logic_1164.all;

architecture pbkji of dllmwhsrzl is
  signal a : std_logic_vector(4 downto 0);
  signal injiuybyze : std_logic_vector(4 downto 0);
  signal zkqti : time;
  signal mcjyrod : std_logic_vector(4 downto 0);
  signal oiqw : time;
begin
  opr : entity work.up
    port map (laybkn => oiqw, pvvjtlc => mcjyrod);
  cyfdka : entity work.up
    port map (laybkn => zkqti, pvvjtlc => injiuybyze);
  mpfpooypeh : entity work.up
    port map (laybkn => oiqw, pvvjtlc => a);
  
  -- Single-driven assignments
  c <= TRUE;
  zkqti <= 3_4_1 ms;
  oiqw <= 4 sec;
  
  -- Multi-driven assignments
  mcjyrod <= "Z--XW";
  injiuybyze <= "LXXUL";
end pbkji;



-- Seed after: 266093936177576926,14652815260262078753
