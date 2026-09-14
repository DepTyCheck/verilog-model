-- Seed: 3314654527744490325,13196211255131729027

library ieee;
use ieee.std_logic_1164.all;

entity rz is
  port (dvzay : buffer std_logic_vector(0 to 3));
end rz;

architecture nb of rz is
  
begin
  -- Multi-driven assignments
  dvzay <= "10WX";
  dvzay <= ('H', 'Z', 'H', '-');
end nb;

library ieee;
use ieee.std_logic_1164.all;

entity il is
  port (io : out integer; kys : inout time; lasxmqqbeo : linkage std_logic; ryfkaxfry : linkage bit);
end il;

library ieee;
use ieee.std_logic_1164.all;

architecture edxbuu of il is
  signal qpbp : std_logic_vector(0 to 3);
begin
  n : entity work.rz
    port map (dvzay => qpbp);
  fbkkzufk : entity work.rz
    port map (dvzay => qpbp);
  
  -- Single-driven assignments
  kys <= kys;
  io <= io;
  
  -- Multi-driven assignments
  qpbp <= "H-0-";
  qpbp <= ('1', '0', 'U', 'L');
  qpbp <= qpbp;
  qpbp <= qpbp;
end edxbuu;



-- Seed after: 7211439919554396918,13196211255131729027
