-- Seed: 13588060558340745077,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity azhqoza is
  port (wtw : inout std_logic_vector(0 to 2); f : buffer time; g : in std_logic; sdduzggg : buffer std_logic_vector(1 to 0));
end azhqoza;

architecture s of azhqoza is
  
begin
  -- Single-driven assignments
  f <= 2#1010# ms;
  
  -- Multi-driven assignments
  wtw <= "HH-";
end s;

entity lfblklitm is
  port (qicybxom : out boolean_vector(4 to 1));
end lfblklitm;

architecture hu of lfblklitm is
  
begin
  -- Single-driven assignments
  qicybxom <= (others => TRUE);
end hu;

library ieee;
use ieee.std_logic_1164.all;

entity cfmmkcpylz is
  port (lx : in std_logic_vector(1 downto 0); ztspxu : buffer boolean);
end cfmmkcpylz;

library ieee;
use ieee.std_logic_1164.all;

architecture ll of cfmmkcpylz is
  signal hmmx : std_logic_vector(1 to 0);
  signal ia : time;
  signal nsczvapfoj : std_logic_vector(1 to 0);
  signal relmsxwybh : std_logic;
  signal zjxsvadu : time;
  signal h : std_logic_vector(0 to 2);
begin
  uyircgrm : entity work.azhqoza
    port map (wtw => h, f => zjxsvadu, g => relmsxwybh, sdduzggg => nsczvapfoj);
  ap : entity work.azhqoza
    port map (wtw => h, f => ia, g => relmsxwybh, sdduzggg => hmmx);
  
  -- Single-driven assignments
  ztspxu <= FALSE;
  
  -- Multi-driven assignments
  h <= ('H', '0', 'H');
  h <= "XW0";
  relmsxwybh <= 'U';
end ll;



-- Seed after: 9338979990104632918,1834764876137802293
