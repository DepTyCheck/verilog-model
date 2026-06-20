-- Seed: 15889327033437785790,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity evgy is
  port (edkoje : out std_logic);
end evgy;

architecture uemin of evgy is
  
begin
  -- Multi-driven assignments
  edkoje <= 'U';
  edkoje <= '-';
end uemin;

library ieee;
use ieee.std_logic_1164.all;

entity ytob is
  port (fq : inout real; ybciaqu : inout real; kq : in std_logic_vector(2 to 2); hpepxergt : linkage std_logic_vector(1 downto 0));
end ytob;

architecture hef of ytob is
  
begin
  -- Single-driven assignments
  ybciaqu <= 3_0.4_0_1;
  fq <= 0_4_2_1_2.2_0;
end hef;

entity epwc is
  port (ysnymektx : inout real_vector(1 to 3));
end epwc;

library ieee;
use ieee.std_logic_1164.all;

architecture jdy of epwc is
  signal gzv : std_logic;
begin
  lu : entity work.evgy
    port map (edkoje => gzv);
  xost : entity work.evgy
    port map (edkoje => gzv);
  nrotiejtfk : entity work.evgy
    port map (edkoje => gzv);
  
  -- Single-driven assignments
  ysnymektx <= (3.3_2_4, 16#C4E.2_A_B#, 3011.24);
  
  -- Multi-driven assignments
  gzv <= '-';
  gzv <= 'X';
  gzv <= 'X';
  gzv <= 'Z';
end jdy;



-- Seed after: 5775024328584602967,3924983747739634027
