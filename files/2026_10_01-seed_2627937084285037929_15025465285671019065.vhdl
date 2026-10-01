-- Seed: 2627937084285037929,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (k : out std_logic_vector(2 downto 4); yhlqkw : in boolean; jpyuepjzof : in integer);
end g;

architecture wxeljqt of g is
  
begin
  
end wxeljqt;

entity ddkhos is
  port (a : out bit; oalc : buffer character);
end ddkhos;

architecture ofqenwrsjr of ddkhos is
  
begin
  -- Single-driven assignments
  oalc <= 'b';
end ofqenwrsjr;

library ieee;
use ieee.std_logic_1164.all;

entity maxwfjifu is
  port (yuyqq : in std_logic; ouzoggqx : inout std_logic; znkocqf : in std_logic_vector(2 downto 4));
end maxwfjifu;

library ieee;
use ieee.std_logic_1164.all;

architecture sducbf of maxwfjifu is
  signal tzgodilhz : boolean;
  signal cxazyj : integer;
  signal xhod : boolean;
  signal zwvcbhlgr : std_logic_vector(2 downto 4);
begin
  alikrhvcc : entity work.g
    port map (k => zwvcbhlgr, yhlqkw => xhod, jpyuepjzof => cxazyj);
  fxfwsylv : entity work.g
    port map (k => zwvcbhlgr, yhlqkw => tzgodilhz, jpyuepjzof => cxazyj);
  lovoo : entity work.g
    port map (k => zwvcbhlgr, yhlqkw => xhod, jpyuepjzof => cxazyj);
  
  -- Single-driven assignments
  xhod <= TRUE;
  cxazyj <= cxazyj;
  tzgodilhz <= FALSE;
  
  -- Multi-driven assignments
  ouzoggqx <= 'H';
  zwvcbhlgr <= znkocqf;
  zwvcbhlgr <= (others => '0');
end sducbf;



-- Seed after: 6446869691783974394,15025465285671019065
