-- Seed: 2808981241492901114,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (ekqvwcc : inout real; oekyhrrde : inout time; mntqnhegxc : linkage time; ap : buffer std_logic);
end n;

architecture ylmig of n is
  
begin
  -- Single-driven assignments
  oekyhrrde <= 4233 ps;
  ekqvwcc <= 4_0.01;
  
  -- Multi-driven assignments
  ap <= ap;
  ap <= ap;
  ap <= '0';
  ap <= '1';
end ylmig;

library ieee;
use ieee.std_logic_1164.all;

entity rym is
  port (zzuya : buffer boolean_vector(3 to 4); dm : inout std_logic; zqdsg : out boolean_vector(0 to 1));
end rym;

architecture cxhp of rym is
  signal oy : time;
  signal uoaaktuqw : time;
  signal vgkwj : real;
begin
  rgbaodmlvz : entity work.n
    port map (ekqvwcc => vgkwj, oekyhrrde => uoaaktuqw, mntqnhegxc => oy, ap => dm);
  
  -- Single-driven assignments
  zqdsg <= zqdsg;
  zzuya <= zzuya;
  
  -- Multi-driven assignments
  dm <= dm;
end cxhp;

library ieee;
use ieee.std_logic_1164.all;

entity raubr is
  port (bcx : buffer std_logic_vector(4 to 3); incctgxbzq : in std_logic_vector(2 downto 4));
end raubr;

library ieee;
use ieee.std_logic_1164.all;

architecture dolpej of raubr is
  signal agrwobm : boolean_vector(0 to 1);
  signal koyaeavx : boolean_vector(3 to 4);
  signal swrk : time;
  signal fd : time;
  signal jolos : real;
  signal hdcfpbukx : std_logic;
  signal khladvi : time;
  signal rskrw : time;
  signal slyxkz : real;
  signal j : std_logic;
  signal ybynsuyc : time;
  signal yo : time;
  signal wgfyijwpc : real;
begin
  abfwuudor : entity work.n
    port map (ekqvwcc => wgfyijwpc, oekyhrrde => yo, mntqnhegxc => ybynsuyc, ap => j);
  nyvelogjl : entity work.n
    port map (ekqvwcc => slyxkz, oekyhrrde => rskrw, mntqnhegxc => khladvi, ap => hdcfpbukx);
  frqgacdrsm : entity work.n
    port map (ekqvwcc => jolos, oekyhrrde => fd, mntqnhegxc => swrk, ap => j);
  fvjf : entity work.rym
    port map (zzuya => koyaeavx, dm => hdcfpbukx, zqdsg => agrwobm);
  
  -- Multi-driven assignments
  bcx <= incctgxbzq;
  bcx <= (others => '0');
  bcx <= (others => '0');
end dolpej;



-- Seed after: 4405640760875476125,8437298063418820479
