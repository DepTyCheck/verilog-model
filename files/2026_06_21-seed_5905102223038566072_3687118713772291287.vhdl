-- Seed: 5905102223038566072,3687118713772291287

entity gamhadwe is
  port (yukdoram : in time; flby : out time; kesexuflnh : out integer);
end gamhadwe;

architecture ea of gamhadwe is
  
begin
  -- Single-driven assignments
  kesexuflnh <= 16#4F49#;
  flby <= 16#7_B_D_F.12# fs;
end ea;

library ieee;
use ieee.std_logic_1164.all;

entity mbqfsdo is
  port (mxgucmunio : inout integer; cepi : linkage std_logic; pqklvpwkj : out severity_level);
end mbqfsdo;

architecture rxgywm of mbqfsdo is
  signal nwkzymuu : integer;
  signal pr : time;
  signal dmffxoi : time;
  signal zbj : time;
  signal p : time;
  signal idb : integer;
  signal xhqizyryqh : time;
  signal vmymdkxgw : time;
begin
  ylwy : entity work.gamhadwe
    port map (yukdoram => vmymdkxgw, flby => xhqizyryqh, kesexuflnh => idb);
  kq : entity work.gamhadwe
    port map (yukdoram => p, flby => zbj, kesexuflnh => mxgucmunio);
  gkbs : entity work.gamhadwe
    port map (yukdoram => dmffxoi, flby => pr, kesexuflnh => nwkzymuu);
  
  -- Single-driven assignments
  pqklvpwkj <= ERROR;
  dmffxoi <= 16#7B# ns;
  vmymdkxgw <= 2 ns;
  p <= 8#3# us;
end rxgywm;

entity fhghksav is
  port (b : buffer severity_level);
end fhghksav;

library ieee;
use ieee.std_logic_1164.all;

architecture yysqqlhbxb of fhghksav is
  signal tpwpltzcf : integer;
  signal aklhdnpud : time;
  signal yys : std_logic;
  signal o : integer;
  signal ksg : integer;
  signal sxvwipu : time;
  signal gac : time;
begin
  jzkrrg : entity work.gamhadwe
    port map (yukdoram => gac, flby => sxvwipu, kesexuflnh => ksg);
  nq : entity work.mbqfsdo
    port map (mxgucmunio => o, cepi => yys, pqklvpwkj => b);
  zqb : entity work.gamhadwe
    port map (yukdoram => aklhdnpud, flby => gac, kesexuflnh => tpwpltzcf);
  
  -- Single-driven assignments
  aklhdnpud <= 1011.3_0_2_3 ns;
  
  -- Multi-driven assignments
  yys <= '1';
  yys <= '1';
  yys <= '-';
end yysqqlhbxb;



-- Seed after: 2511761022283326042,3687118713772291287
