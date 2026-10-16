-- Seed: 11162035480777733291,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (wmez : inout std_logic_vector(2 to 3));
end f;

architecture tfogf of f is
  
begin
  -- Multi-driven assignments
  wmez <= wmez;
end tfogf;

library ieee;
use ieee.std_logic_1164.all;

entity tnbdhkt is
  port (nkmk : out bit; fqtsjiay : linkage std_logic_vector(3 to 2));
end tnbdhkt;

library ieee;
use ieee.std_logic_1164.all;

architecture gqinfonnnp of tnbdhkt is
  signal vuk : std_logic_vector(2 to 3);
  signal unn : std_logic_vector(2 to 3);
begin
  rws : entity work.f
    port map (wmez => unn);
  lblyvvb : entity work.f
    port map (wmez => unn);
  h : entity work.f
    port map (wmez => vuk);
  czwlnr : entity work.f
    port map (wmez => vuk);
  
  -- Single-driven assignments
  nkmk <= '0';
  
  -- Multi-driven assignments
  unn <= ('U', 'U');
  unn <= ('L', 'H');
  vuk <= vuk;
  unn <= vuk;
end gqinfonnnp;



-- Seed after: 13937618201069157380,9900838946149614537
