-- Seed: 18002175527064012480,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity bptluel is
  port (v : linkage std_logic_vector(1 to 2); dn : out string(4 to 1));
end bptluel;

architecture j of bptluel is
  
begin
  
end j;

entity fphhlaec is
  port (e : out integer_vector(0 to 1));
end fphhlaec;

library ieee;
use ieee.std_logic_1164.all;

architecture n of fphhlaec is
  signal wd : string(4 to 1);
  signal ryszcsrqc : std_logic_vector(1 to 2);
begin
  kwllcrj : entity work.bptluel
    port map (v => ryszcsrqc, dn => wd);
  
  -- Single-driven assignments
  e <= (0, 04314);
  
  -- Multi-driven assignments
  ryszcsrqc <= "U0";
  ryszcsrqc <= ('W', '0');
  ryszcsrqc <= ('-', '0');
  ryszcsrqc <= ('U', 'L');
end n;

entity jopbyhezk is
  port (ok : inout bit_vector(0 downto 4));
end jopbyhezk;

library ieee;
use ieee.std_logic_1164.all;

architecture qzvkvv of jopbyhezk is
  signal pkka : string(4 to 1);
  signal mcglqqlo : std_logic_vector(1 to 2);
  signal t : integer_vector(0 to 1);
begin
  vn : entity work.fphhlaec
    port map (e => t);
  qkakw : entity work.bptluel
    port map (v => mcglqqlo, dn => pkka);
  
  -- Single-driven assignments
  ok <= (others => '0');
  
  -- Multi-driven assignments
  mcglqqlo <= ('U', 'H');
  mcglqqlo <= "1-";
  mcglqqlo <= ('X', 'L');
  mcglqqlo <= "U1";
end qzvkvv;



-- Seed after: 693474522938223639,5472058987609252853
