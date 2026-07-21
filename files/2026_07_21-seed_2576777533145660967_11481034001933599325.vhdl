-- Seed: 2576777533145660967,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity xrlc is
  port (qbcp : buffer std_logic_vector(2 to 2); vwigicdhpa : linkage time; iw : out bit);
end xrlc;

architecture rtdqmnmndv of xrlc is
  
begin
  -- Single-driven assignments
  iw <= '0';
  
  -- Multi-driven assignments
  qbcp <= "U";
  qbcp <= "0";
end rtdqmnmndv;

entity knuihrwgo is
  port (vhal : inout integer);
end knuihrwgo;

library ieee;
use ieee.std_logic_1164.all;

architecture odid of knuihrwgo is
  signal pqvnzdcj : bit;
  signal holsnyitwm : time;
  signal zxapuqxhiu : bit;
  signal uaxlxqzjz : time;
  signal tfezcwim : std_logic_vector(2 to 2);
  signal zlrlbw : bit;
  signal qnvswdokq : time;
  signal hn : std_logic_vector(2 to 2);
  signal sqguupxvj : bit;
  signal wvzwvf : time;
  signal pujdnjdoqs : std_logic_vector(2 to 2);
begin
  wcsfsqdur : entity work.xrlc
    port map (qbcp => pujdnjdoqs, vwigicdhpa => wvzwvf, iw => sqguupxvj);
  cr : entity work.xrlc
    port map (qbcp => hn, vwigicdhpa => qnvswdokq, iw => zlrlbw);
  daqz : entity work.xrlc
    port map (qbcp => tfezcwim, vwigicdhpa => uaxlxqzjz, iw => zxapuqxhiu);
  ch : entity work.xrlc
    port map (qbcp => hn, vwigicdhpa => holsnyitwm, iw => pqvnzdcj);
  
  -- Multi-driven assignments
  pujdnjdoqs <= "U";
  pujdnjdoqs <= (others => 'H');
end odid;

library ieee;
use ieee.std_logic_1164.all;

entity lp is
  port (p : out std_logic_vector(3 to 2); t : buffer time);
end lp;

library ieee;
use ieee.std_logic_1164.all;

architecture dp of lp is
  signal djvot : bit;
  signal ckpjkaqd : time;
  signal mzc : std_logic_vector(2 to 2);
  signal psdzxggy : bit;
  signal eirqjkw : bit;
  signal uiz : time;
  signal axaygf : std_logic_vector(2 to 2);
  signal tzqfqgyh : bit;
  signal pflocpy : time;
  signal c : std_logic_vector(2 to 2);
begin
  tntiuryvcd : entity work.xrlc
    port map (qbcp => c, vwigicdhpa => pflocpy, iw => tzqfqgyh);
  rsk : entity work.xrlc
    port map (qbcp => axaygf, vwigicdhpa => uiz, iw => eirqjkw);
  dlqbbvib : entity work.xrlc
    port map (qbcp => c, vwigicdhpa => t, iw => psdzxggy);
  bqnke : entity work.xrlc
    port map (qbcp => mzc, vwigicdhpa => ckpjkaqd, iw => djvot);
  
  -- Multi-driven assignments
  p <= (others => '0');
  mzc <= "L";
  p <= (others => '0');
end dp;



-- Seed after: 10490278899034723440,11481034001933599325
