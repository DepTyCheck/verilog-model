-- Seed: 13891846433695281751,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (vjcvbgck : in bit; pnyfa : inout time; mxmj : buffer std_logic; zrulbjxtsi : in std_logic);
end x;

architecture lfrzlo of x is
  
begin
  -- Single-driven assignments
  pnyfa <= 0_1_3_0_3.2322 ns;
  
  -- Multi-driven assignments
  mxmj <= zrulbjxtsi;
  mxmj <= zrulbjxtsi;
end lfrzlo;

entity bxvqsx is
  port (d : out time_vector(3 downto 1));
end bxvqsx;

library ieee;
use ieee.std_logic_1164.all;

architecture a of bxvqsx is
  signal hxhuyaazcq : time;
  signal adn : bit;
  signal wjyhd : std_logic;
  signal vxv : time;
  signal hygrk : std_logic;
  signal egbsy : time;
  signal gxwi : bit;
begin
  qinqgl : entity work.x
    port map (vjcvbgck => gxwi, pnyfa => egbsy, mxmj => hygrk, zrulbjxtsi => hygrk);
  icdpq : entity work.x
    port map (vjcvbgck => gxwi, pnyfa => vxv, mxmj => hygrk, zrulbjxtsi => wjyhd);
  qzhlghna : entity work.x
    port map (vjcvbgck => adn, pnyfa => hxhuyaazcq, mxmj => hygrk, zrulbjxtsi => wjyhd);
  
  -- Single-driven assignments
  d <= (13134.1_1 fs, 2_2_1_1_1.02200 ns, 16#6_8_5_F# us);
  
  -- Multi-driven assignments
  hygrk <= 'Z';
  hygrk <= 'Z';
end a;

library ieee;
use ieee.std_logic_1164.all;

entity szpgxv is
  port (poqwpzzst : buffer std_logic; viagfyty : in std_logic; o : buffer string(2 downto 3));
end szpgxv;

library ieee;
use ieee.std_logic_1164.all;

architecture s of szpgxv is
  signal vyymo : time_vector(3 downto 1);
  signal togdci : std_logic;
  signal i : time;
  signal ndtlxa : bit;
begin
  nryky : entity work.x
    port map (vjcvbgck => ndtlxa, pnyfa => i, mxmj => togdci, zrulbjxtsi => poqwpzzst);
  vvrq : entity work.bxvqsx
    port map (d => vyymo);
  
  -- Single-driven assignments
  ndtlxa <= '0';
  o <= (others => ' ');
  
  -- Multi-driven assignments
  togdci <= poqwpzzst;
  togdci <= viagfyty;
end s;



-- Seed after: 15519299198604328892,8891552411914730853
