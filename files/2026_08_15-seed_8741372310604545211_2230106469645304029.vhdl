-- Seed: 8741372310604545211,2230106469645304029

entity akknqcdt is
  port (mdzdaoytj : buffer integer; nttvihpsf : in real);
end akknqcdt;

architecture idmo of akknqcdt is
  
begin
  -- Single-driven assignments
  mdzdaoytj <= mdzdaoytj;
end idmo;

entity phjuhcy is
  port (oedflvnfk : in integer; bhdqr : out boolean_vector(2 to 0); lpzy : buffer real);
end phjuhcy;

architecture wwc of phjuhcy is
  signal xreoe : real;
  signal qbutyw : integer;
  signal qydd : integer;
  signal q : integer;
begin
  lj : entity work.akknqcdt
    port map (mdzdaoytj => q, nttvihpsf => lpzy);
  xo : entity work.akknqcdt
    port map (mdzdaoytj => qydd, nttvihpsf => lpzy);
  vh : entity work.akknqcdt
    port map (mdzdaoytj => qbutyw, nttvihpsf => xreoe);
end wwc;

library ieee;
use ieee.std_logic_1164.all;

entity ytbv is
  port (hdu : out time; lsfzj : buffer std_logic; kfchqelmo : inout integer; gtxkxwpry : linkage std_logic_vector(0 downto 1));
end ytbv;

architecture geblonbcif of ytbv is
  signal cv : real;
  signal q : boolean_vector(2 to 0);
begin
  bivzevei : entity work.phjuhcy
    port map (oedflvnfk => kfchqelmo, bhdqr => q, lpzy => cv);
  
  -- Single-driven assignments
  kfchqelmo <= kfchqelmo;
  
  -- Multi-driven assignments
  lsfzj <= 'U';
end geblonbcif;



-- Seed after: 2679917695963876561,2230106469645304029
