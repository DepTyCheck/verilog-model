-- Seed: 15425312953560469637,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity umtwsyi is
  port (jzqvvh : in integer; llps : in std_logic_vector(3 to 2); braccomb : buffer integer_vector(4 downto 4));
end umtwsyi;

architecture hr of umtwsyi is
  
begin
  -- Single-driven assignments
  braccomb <= (others => 32);
end hr;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (caluds : out std_logic; qsookpxlrc : in real);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture mzsyw of n is
  signal zzyw : integer_vector(4 downto 4);
  signal vidypmufp : std_logic_vector(3 to 2);
  signal rrvvhgb : integer_vector(4 downto 4);
  signal zw : std_logic_vector(3 to 2);
  signal nb : integer;
begin
  pzwjxsl : entity work.umtwsyi
    port map (jzqvvh => nb, llps => zw, braccomb => rrvvhgb);
  gndntisbag : entity work.umtwsyi
    port map (jzqvvh => nb, llps => vidypmufp, braccomb => zzyw);
  
  -- Single-driven assignments
  nb <= 0_0_1_2;
  
  -- Multi-driven assignments
  zw <= (others => '0');
end mzsyw;

library ieee;
use ieee.std_logic_1164.all;

entity lrxxwnwm is
  port (bwlimix : linkage std_logic_vector(0 to 2); akhozsrxtm : linkage std_logic; nphful : in integer; snok : buffer std_logic);
end lrxxwnwm;

architecture iwhtp of lrxxwnwm is
  
begin
  -- Multi-driven assignments
  snok <= 'Z';
  snok <= snok;
  snok <= 'H';
end iwhtp;



-- Seed after: 6838211591143612014,6176025545472508329
