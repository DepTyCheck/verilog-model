-- Seed: 5423437416451003409,13843488114570579517

entity pskbu is
  port (tmpeumri : linkage bit_vector(1 to 1); xepr : out bit_vector(4 downto 2));
end pskbu;

architecture g of pskbu is
  
begin
  -- Single-driven assignments
  xepr <= ('1', '0', '0');
end g;

library ieee;
use ieee.std_logic_1164.all;

entity lqhauesr is
  port (e : buffer std_logic; tjjb : buffer time; kelrj : buffer character);
end lqhauesr;

architecture fpvrgudin of lqhauesr is
  signal cn : bit_vector(4 downto 2);
  signal l : bit_vector(1 to 1);
  signal kbqe : bit_vector(4 downto 2);
  signal hlbieb : bit_vector(1 to 1);
begin
  mgdkmikteb : entity work.pskbu
    port map (tmpeumri => hlbieb, xepr => kbqe);
  sw : entity work.pskbu
    port map (tmpeumri => l, xepr => cn);
  
  -- Single-driven assignments
  tjjb <= 03021 ms;
  kelrj <= 'j';
  
  -- Multi-driven assignments
  e <= 'X';
  e <= e;
  e <= 'L';
  e <= 'U';
end fpvrgudin;



-- Seed after: 14323805674362323213,13843488114570579517
