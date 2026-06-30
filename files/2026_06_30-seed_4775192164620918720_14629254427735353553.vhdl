-- Seed: 4775192164620918720,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity mygafi is
  port (zcn : out std_logic_vector(2 to 4); digaxmsxyo : inout time; cch : in bit_vector(1 to 3));
end mygafi;

architecture znoslwp of mygafi is
  
begin
  -- Single-driven assignments
  digaxmsxyo <= 41440 us;
end znoslwp;

entity gifpgjhs is
  port (innq : in integer; mum : in time);
end gifpgjhs;

library ieee;
use ieee.std_logic_1164.all;

architecture rjxum of gifpgjhs is
  signal x : bit_vector(1 to 3);
  signal kuxqk : time;
  signal tfaqeue : std_logic_vector(2 to 4);
begin
  znt : entity work.mygafi
    port map (zcn => tfaqeue, digaxmsxyo => kuxqk, cch => x);
  
  -- Multi-driven assignments
  tfaqeue <= ('W', 'Z', '1');
  tfaqeue <= ('1', 'W', 'X');
  tfaqeue <= ('0', 'U', 'X');
end rjxum;

library ieee;
use ieee.std_logic_1164.all;

entity oabg is
  port (hramschd : out std_logic);
end oabg;

library ieee;
use ieee.std_logic_1164.all;

architecture lx of oabg is
  signal mgyfvsv : integer;
  signal t : bit_vector(1 to 3);
  signal eqbhiovi : time;
  signal qrkcjxyc : std_logic_vector(2 to 4);
begin
  wcsfsfi : entity work.mygafi
    port map (zcn => qrkcjxyc, digaxmsxyo => eqbhiovi, cch => t);
  svnvhxwiyx : entity work.gifpgjhs
    port map (innq => mgyfvsv, mum => eqbhiovi);
  
  -- Single-driven assignments
  t <= ('1', '1', '0');
  mgyfvsv <= 16#CC#;
  
  -- Multi-driven assignments
  hramschd <= 'W';
end lx;



-- Seed after: 1584264259233452681,14629254427735353553
