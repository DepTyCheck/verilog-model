-- Seed: 17415467264912469313,4177195558088809003

entity rmuavmx is
  port (xn : inout time_vector(3 to 0));
end rmuavmx;

architecture ejrpelehfe of rmuavmx is
  
begin
  -- Single-driven assignments
  xn <= (others => 0 ns);
end ejrpelehfe;

library ieee;
use ieee.std_logic_1164.all;

entity mxaefxkla is
  port (r : out real; u : inout std_logic);
end mxaefxkla;

architecture hxyqdh of mxaefxkla is
  signal gcghkrbeq : time_vector(3 to 0);
  signal cdmldlfik : time_vector(3 to 0);
  signal gr : time_vector(3 to 0);
  signal xoar : time_vector(3 to 0);
begin
  rbmtmwyvfo : entity work.rmuavmx
    port map (xn => xoar);
  jagsailacp : entity work.rmuavmx
    port map (xn => gr);
  n : entity work.rmuavmx
    port map (xn => cdmldlfik);
  pxomipbagf : entity work.rmuavmx
    port map (xn => gcghkrbeq);
  
  -- Single-driven assignments
  r <= 4.4144;
  
  -- Multi-driven assignments
  u <= 'W';
  u <= u;
end hxyqdh;

library ieee;
use ieee.std_logic_1164.all;

entity pzhyonyrm is
  port (alabvosr : inout integer; vx : in integer; yasxhurp : in real; aywk : buffer std_logic);
end pzhyonyrm;

architecture zoktl of pzhyonyrm is
  signal dzqu : real;
  signal tvzqmlk : time_vector(3 to 0);
  signal ztxdhny : real;
begin
  utcdmx : entity work.mxaefxkla
    port map (r => ztxdhny, u => aywk);
  aughwz : entity work.rmuavmx
    port map (xn => tvzqmlk);
  fmugvxr : entity work.mxaefxkla
    port map (r => dzqu, u => aywk);
  
  -- Single-driven assignments
  alabvosr <= vx;
  
  -- Multi-driven assignments
  aywk <= 'X';
  aywk <= aywk;
end zoktl;

entity kzf is
  port (v : linkage boolean; txzmm : in boolean_vector(0 to 1));
end kzf;

library ieee;
use ieee.std_logic_1164.all;

architecture citemi of kzf is
  signal s : time_vector(3 to 0);
  signal ltac : integer;
  signal l : integer;
  signal cojdy : std_logic;
  signal oqo : real;
  signal reemz : integer;
begin
  pnskwrmm : entity work.pzhyonyrm
    port map (alabvosr => reemz, vx => reemz, yasxhurp => oqo, aywk => cojdy);
  brcqpzx : entity work.pzhyonyrm
    port map (alabvosr => l, vx => ltac, yasxhurp => oqo, aywk => cojdy);
  mxm : entity work.rmuavmx
    port map (xn => s);
  
  -- Single-driven assignments
  oqo <= 2#0_1_1_0_1.0#;
  ltac <= reemz;
end citemi;



-- Seed after: 12131953989757262889,4177195558088809003
