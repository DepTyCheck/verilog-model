-- Seed: 14110315919188504038,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity jhwcu is
  port (qpn : out std_logic_vector(4 downto 0); i : out time; catxxa : inout std_logic_vector(0 to 0));
end jhwcu;

architecture a of jhwcu is
  
begin
  -- Single-driven assignments
  i <= 2#1# ns;
end a;

library ieee;
use ieee.std_logic_1164.all;

entity lxbv is
  port (lxxqqfluuz : out bit_vector(2 to 2); dzkf : out std_logic; kqqbwknbb : in std_logic_vector(3 to 0));
end lxbv;

library ieee;
use ieee.std_logic_1164.all;

architecture a of lxbv is
  signal p : std_logic_vector(0 to 0);
  signal foxhmmlk : time;
  signal x : std_logic_vector(4 downto 0);
  signal ecpkza : std_logic_vector(0 to 0);
  signal kzfndh : time;
  signal ewekklrq : std_logic_vector(4 downto 0);
begin
  yovkp : entity work.jhwcu
    port map (qpn => ewekklrq, i => kzfndh, catxxa => ecpkza);
  g : entity work.jhwcu
    port map (qpn => x, i => foxhmmlk, catxxa => p);
  
  -- Single-driven assignments
  lxxqqfluuz <= (others => '1');
  
  -- Multi-driven assignments
  dzkf <= 'L';
  x <= ewekklrq;
  dzkf <= dzkf;
  ewekklrq <= ewekklrq;
end a;



-- Seed after: 2520370011296400453,13501862637168280927
