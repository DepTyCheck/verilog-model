-- Seed: 7808922866104728899,6869379746012019351

entity mrjzjmy is
  port (ndoly : inout bit; z : buffer real; cmusystdzb : buffer time; autuguxk : linkage real_vector(2 to 3));
end mrjzjmy;

architecture kiriw of mrjzjmy is
  
begin
  -- Single-driven assignments
  ndoly <= '0';
  cmusystdzb <= cmusystdzb;
  z <= 1.01;
end kiriw;

entity vwmh is
  port (iep : in boolean_vector(0 downto 1));
end vwmh;

architecture yazxpoxdlo of vwmh is
  signal snqbmhwp : real_vector(2 to 3);
  signal revzqel : time;
  signal xelalhvx : real;
  signal mme : bit;
begin
  boaiorwl : entity work.mrjzjmy
    port map (ndoly => mme, z => xelalhvx, cmusystdzb => revzqel, autuguxk => snqbmhwp);
end yazxpoxdlo;

library ieee;
use ieee.std_logic_1164.all;

entity hokhrzam is
  port (plb : linkage integer; ukrddhaw : out std_logic; driafaow : inout bit_vector(0 downto 3); ohuqfxszaf : inout std_logic_vector(4 downto 1));
end hokhrzam;

architecture hprnyi of hokhrzam is
  signal leqslwcp : real_vector(2 to 3);
  signal ihjc : time;
  signal aortc : real;
  signal l : bit;
  signal kwkycvf : real_vector(2 to 3);
  signal fswg : time;
  signal vshpyzy : real;
  signal brnhqhgrc : bit;
begin
  eb : entity work.mrjzjmy
    port map (ndoly => brnhqhgrc, z => vshpyzy, cmusystdzb => fswg, autuguxk => kwkycvf);
  jfgwsj : entity work.mrjzjmy
    port map (ndoly => l, z => aortc, cmusystdzb => ihjc, autuguxk => leqslwcp);
  
  -- Single-driven assignments
  driafaow <= (others => '0');
  
  -- Multi-driven assignments
  ohuqfxszaf <= ('X', 'U', '1', 'U');
end hprnyi;



-- Seed after: 14232938763268161449,6869379746012019351
