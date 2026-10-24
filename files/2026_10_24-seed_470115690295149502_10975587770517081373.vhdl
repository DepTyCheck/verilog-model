-- Seed: 470115690295149502,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity jidcuekf is
  port (oqosv : in std_logic_vector(2 to 0); y : inout real; x : buffer string(1 to 2); vstn : inout integer);
end jidcuekf;

architecture xlubv of jidcuekf is
  
begin
  -- Single-driven assignments
  vstn <= vstn;
  x <= "lg";
  y <= y;
end xlubv;

library ieee;
use ieee.std_logic_1164.all;

entity livb is
  port (gbkh : inout std_logic; u : buffer std_logic_vector(2 to 4); zc : buffer std_logic);
end livb;

library ieee;
use ieee.std_logic_1164.all;

architecture er of livb is
  signal yggypvbo : integer;
  signal zzivjpipw : string(1 to 2);
  signal evazuf : real;
  signal h : integer;
  signal r : string(1 to 2);
  signal oyvlprhzhi : real;
  signal bmujtdy : integer;
  signal gniykv : string(1 to 2);
  signal oevghos : real;
  signal xmtky : std_logic_vector(2 to 0);
begin
  szjlggsh : entity work.jidcuekf
    port map (oqosv => xmtky, y => oevghos, x => gniykv, vstn => bmujtdy);
  g : entity work.jidcuekf
    port map (oqosv => xmtky, y => oyvlprhzhi, x => r, vstn => h);
  vclbfa : entity work.jidcuekf
    port map (oqosv => xmtky, y => evazuf, x => zzivjpipw, vstn => yggypvbo);
  
  -- Multi-driven assignments
  u <= ('1', 'X', 'X');
  zc <= gbkh;
  zc <= '1';
  gbkh <= zc;
end er;

library ieee;
use ieee.std_logic_1164.all;

entity hzqpclgagx is
  port (mnuc : out std_logic);
end hzqpclgagx;

library ieee;
use ieee.std_logic_1164.all;

architecture yivtnwsj of hzqpclgagx is
  signal qmwsy : std_logic;
  signal x : std_logic_vector(2 to 4);
  signal imvxsnauxb : std_logic;
begin
  eew : entity work.livb
    port map (gbkh => imvxsnauxb, u => x, zc => qmwsy);
  
  -- Multi-driven assignments
  mnuc <= 'U';
end yivtnwsj;



-- Seed after: 16116617779002663502,10975587770517081373
