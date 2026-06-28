-- Seed: 5050302326967022508,6697892553037813751

entity zue is
  port (jyflncyz : out time);
end zue;

architecture dph of zue is
  
begin
  -- Single-driven assignments
  jyflncyz <= 1 min;
end dph;

library ieee;
use ieee.std_logic_1164.all;

entity fplhytgh is
  port (w : out std_logic_vector(4 downto 4));
end fplhytgh;

architecture e of fplhytgh is
  signal twk : time;
  signal jwxe : time;
  signal eroyp : time;
  signal ftwrwdfk : time;
begin
  pr : entity work.zue
    port map (jyflncyz => ftwrwdfk);
  kpqae : entity work.zue
    port map (jyflncyz => eroyp);
  xrhpqtx : entity work.zue
    port map (jyflncyz => jwxe);
  kuscgmlu : entity work.zue
    port map (jyflncyz => twk);
  
  -- Multi-driven assignments
  w <= "X";
  w <= "1";
  w <= (others => 'Z');
end e;



-- Seed after: 1223329349661269486,6697892553037813751
