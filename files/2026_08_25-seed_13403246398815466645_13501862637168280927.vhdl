-- Seed: 13403246398815466645,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity hpnvqr is
  port (wmgyzdx : out integer; wqew : out integer; fddp : buffer std_logic);
end hpnvqr;

architecture jm of hpnvqr is
  
begin
  -- Single-driven assignments
  wqew <= 16#AB03#;
  wmgyzdx <= wqew;
end jm;

library ieee;
use ieee.std_logic_1164.all;

entity cuwahmnh is
  port (duyh : in std_logic_vector(3 to 2); r : inout integer; mmch : linkage integer; hfhca : out boolean);
end cuwahmnh;

library ieee;
use ieee.std_logic_1164.all;

architecture hskvllv of cuwahmnh is
  signal gi : std_logic;
  signal emfyf : integer;
  signal erz : integer;
begin
  zb : entity work.hpnvqr
    port map (wmgyzdx => erz, wqew => emfyf, fddp => gi);
  
  -- Single-driven assignments
  hfhca <= FALSE;
  r <= emfyf;
end hskvllv;



-- Seed after: 9266420587424331060,13501862637168280927
