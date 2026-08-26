-- Seed: 10341320471077602272,6000118208082478503

entity jvyqbu is
  port (kvnathar : in string(4 downto 3); mgvt : buffer bit; qmbuxo : linkage time; dmlvu : in real_vector(3 to 4));
end jvyqbu;

architecture dcyzeekvnx of jvyqbu is
  
begin
  -- Single-driven assignments
  mgvt <= '1';
end dcyzeekvnx;

library ieee;
use ieee.std_logic_1164.all;

entity rve is
  port (ugovbjdd : in real; dix : inout std_logic_vector(4 to 2); a : inout std_logic_vector(3 downto 4));
end rve;

architecture ytqocgiol of rve is
  signal xkucpnrsxu : time;
  signal zbeyyw : bit;
  signal zrfspwfz : time;
  signal xkqf : bit;
  signal aymv : string(4 downto 3);
  signal gw : real_vector(3 to 4);
  signal plrkqpf : time;
  signal akoiibjb : bit;
  signal m : string(4 downto 3);
begin
  c : entity work.jvyqbu
    port map (kvnathar => m, mgvt => akoiibjb, qmbuxo => plrkqpf, dmlvu => gw);
  l : entity work.jvyqbu
    port map (kvnathar => aymv, mgvt => xkqf, qmbuxo => zrfspwfz, dmlvu => gw);
  llnyhsbvgx : entity work.jvyqbu
    port map (kvnathar => m, mgvt => zbeyyw, qmbuxo => xkucpnrsxu, dmlvu => gw);
  
  -- Single-driven assignments
  m <= ('j', 'w');
  aymv <= m;
  gw <= (14202.1_2, 4.0_4_2);
  
  -- Multi-driven assignments
  a <= dix;
  dix <= a;
  a <= (others => '0');
end ytqocgiol;



-- Seed after: 7192600299851069061,6000118208082478503
