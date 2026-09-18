-- Seed: 2082443978526642616,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity wung is
  port (ndyfqorpa : linkage std_logic; upuwtqvur : buffer time; hkjdjvyw : buffer boolean_vector(1 downto 4));
end wung;

architecture eksd of wung is
  
begin
  
end eksd;

entity bunwe is
  port (pinwr : in boolean_vector(2 downto 1); kxivo : linkage time; yyorj : out boolean_vector(0 to 0));
end bunwe;

library ieee;
use ieee.std_logic_1164.all;

architecture u of bunwe is
  signal rqpsnwis : boolean_vector(1 downto 4);
  signal uqqubmn : time;
  signal vqnrb : std_logic;
begin
  yh : entity work.wung
    port map (ndyfqorpa => vqnrb, upuwtqvur => uqqubmn, hkjdjvyw => rqpsnwis);
  
  -- Single-driven assignments
  yyorj <= (others => FALSE);
  
  -- Multi-driven assignments
  vqnrb <= 'H';
  vqnrb <= 'Z';
  vqnrb <= 'H';
  vqnrb <= 'H';
end u;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (domwtriatr : linkage std_logic; lmj : linkage real; aqwkssgry : linkage std_logic; zabmzpu : inout string(3 to 5));
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture ib of p is
  signal eyv : boolean_vector(1 downto 4);
  signal lrqz : time;
  signal ceqrldiwub : boolean_vector(1 downto 4);
  signal xunympb : time;
  signal nj : std_logic;
begin
  caasrqj : entity work.wung
    port map (ndyfqorpa => nj, upuwtqvur => xunympb, hkjdjvyw => ceqrldiwub);
  dmibgv : entity work.wung
    port map (ndyfqorpa => domwtriatr, upuwtqvur => lrqz, hkjdjvyw => eyv);
  
  -- Multi-driven assignments
  nj <= 'W';
  nj <= nj;
  nj <= nj;
  nj <= 'W';
end ib;

library ieee;
use ieee.std_logic_1164.all;

entity jo is
  port (qztef : in std_logic; qmv : linkage std_logic_vector(0 downto 3); jqz : linkage integer);
end jo;

architecture bkvowfm of jo is
  signal oi : boolean_vector(1 downto 4);
  signal naddw : time;
  signal zzhwd : boolean_vector(0 to 0);
  signal pghmtf : time;
  signal dzg : boolean_vector(2 downto 1);
begin
  vtegpg : entity work.bunwe
    port map (pinwr => dzg, kxivo => pghmtf, yyorj => zzhwd);
  frptlik : entity work.wung
    port map (ndyfqorpa => qztef, upuwtqvur => naddw, hkjdjvyw => oi);
end bkvowfm;



-- Seed after: 2907119954948215380,3316342841050048249
