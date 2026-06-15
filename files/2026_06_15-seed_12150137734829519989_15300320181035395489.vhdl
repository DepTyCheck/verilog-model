-- Seed: 12150137734829519989,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity dedgnzuuzc is
  port (vcunrly : linkage time; mlfxm : inout std_logic; y : out integer_vector(2 to 1); txcwy : linkage integer_vector(3 to 0));
end dedgnzuuzc;

architecture ly of dedgnzuuzc is
  
begin
  -- Single-driven assignments
  y <= (others => 0);
  
  -- Multi-driven assignments
  mlfxm <= 'W';
  mlfxm <= '0';
  mlfxm <= '-';
end ly;

library ieee;
use ieee.std_logic_1164.all;

entity whqsuicyn is
  port (zhba : linkage std_logic; alpgpbzl : linkage time);
end whqsuicyn;

library ieee;
use ieee.std_logic_1164.all;

architecture sjj of whqsuicyn is
  signal knbhfjk : integer_vector(3 to 0);
  signal goxj : integer_vector(2 to 1);
  signal qsdbjqdw : integer_vector(3 to 0);
  signal kmpd : integer_vector(2 to 1);
  signal ebqskh : std_logic;
  signal il : time;
  signal cqyaupnpb : integer_vector(3 to 0);
  signal agj : integer_vector(2 to 1);
  signal lzvgjx : std_logic;
  signal svzzcaekk : time;
begin
  oaem : entity work.dedgnzuuzc
    port map (vcunrly => svzzcaekk, mlfxm => lzvgjx, y => agj, txcwy => cqyaupnpb);
  nwimffo : entity work.dedgnzuuzc
    port map (vcunrly => il, mlfxm => ebqskh, y => kmpd, txcwy => qsdbjqdw);
  ieo : entity work.dedgnzuuzc
    port map (vcunrly => alpgpbzl, mlfxm => lzvgjx, y => goxj, txcwy => knbhfjk);
  
  -- Multi-driven assignments
  ebqskh <= 'L';
  lzvgjx <= 'H';
end sjj;

entity oiirp is
  port (noyzgynyl : inout string(1 to 2); l : buffer integer);
end oiirp;

library ieee;
use ieee.std_logic_1164.all;

architecture h of oiirp is
  signal de : time;
  signal zbn : integer_vector(3 to 0);
  signal a : integer_vector(2 to 1);
  signal elrdjuk : time;
  signal e : time;
  signal osklxuuo : std_logic;
begin
  krs : entity work.whqsuicyn
    port map (zhba => osklxuuo, alpgpbzl => e);
  ebzfyubmtz : entity work.dedgnzuuzc
    port map (vcunrly => elrdjuk, mlfxm => osklxuuo, y => a, txcwy => zbn);
  nayxlwm : entity work.whqsuicyn
    port map (zhba => osklxuuo, alpgpbzl => de);
  
  -- Single-driven assignments
  l <= 8#712#;
  noyzgynyl <= ('c', 't');
  
  -- Multi-driven assignments
  osklxuuo <= 'X';
end h;

entity zpui is
  port (jsmehv : in integer);
end zpui;

library ieee;
use ieee.std_logic_1164.all;

architecture me of zpui is
  signal eeele : time;
  signal sqcqfpbvz : integer_vector(3 to 0);
  signal g : integer_vector(2 to 1);
  signal vmoguzyh : std_logic;
  signal wmoakses : time;
begin
  hab : entity work.dedgnzuuzc
    port map (vcunrly => wmoakses, mlfxm => vmoguzyh, y => g, txcwy => sqcqfpbvz);
  negxad : entity work.whqsuicyn
    port map (zhba => vmoguzyh, alpgpbzl => eeele);
  
  -- Multi-driven assignments
  vmoguzyh <= 'L';
end me;



-- Seed after: 11983697361174968026,15300320181035395489
