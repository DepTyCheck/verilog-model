-- Seed: 7000934252881893394,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity jxdhg is
  port (obmhwtmjor : linkage integer_vector(0 to 1); ynkyprt : out std_logic_vector(2 downto 1); ivezhfoyva : out time);
end jxdhg;

architecture pvmrjxbxug of jxdhg is
  
begin
  -- Single-driven assignments
  ivezhfoyva <= 0.2 us;
  
  -- Multi-driven assignments
  ynkyprt <= ynkyprt;
  ynkyprt <= ('0', 'L');
  ynkyprt <= ynkyprt;
end pvmrjxbxug;

entity a is
  port (zqtsdnbof : linkage integer);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture wrjrgbu of a is
  signal okfnegyr : time;
  signal zcaljjhku : integer_vector(0 to 1);
  signal pjmeawplsw : time;
  signal zvlx : std_logic_vector(2 downto 1);
  signal rn : integer_vector(0 to 1);
  signal vuppdl : time;
  signal cpdkv : std_logic_vector(2 downto 1);
  signal sbs : integer_vector(0 to 1);
  signal dft : time;
  signal gaeqlfjoyx : std_logic_vector(2 downto 1);
  signal iaglmd : integer_vector(0 to 1);
begin
  dk : entity work.jxdhg
    port map (obmhwtmjor => iaglmd, ynkyprt => gaeqlfjoyx, ivezhfoyva => dft);
  i : entity work.jxdhg
    port map (obmhwtmjor => sbs, ynkyprt => cpdkv, ivezhfoyva => vuppdl);
  oxtpprkewa : entity work.jxdhg
    port map (obmhwtmjor => rn, ynkyprt => zvlx, ivezhfoyva => pjmeawplsw);
  getypbf : entity work.jxdhg
    port map (obmhwtmjor => zcaljjhku, ynkyprt => gaeqlfjoyx, ivezhfoyva => okfnegyr);
  
  -- Multi-driven assignments
  zvlx <= ('0', 'U');
  cpdkv <= ('Z', '1');
end wrjrgbu;



-- Seed after: 10509620943727227562,9900838946149614537
