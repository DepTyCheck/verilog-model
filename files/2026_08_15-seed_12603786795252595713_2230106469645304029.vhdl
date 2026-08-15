-- Seed: 12603786795252595713,2230106469645304029

entity zwqndhzfm is
  port (hif : inout integer; ofhm : buffer integer; uaui : linkage boolean; etuvw : linkage integer);
end zwqndhzfm;

architecture lsvhkxk of zwqndhzfm is
  
begin
  -- Single-driven assignments
  ofhm <= 2#1_1#;
  hif <= 2#11#;
end lsvhkxk;

library ieee;
use ieee.std_logic_1164.all;

entity ijnuxe is
  port (i : buffer integer_vector(3 to 3); mzx : out time; fwnzu : inout std_logic_vector(1 downto 0); bh : buffer time);
end ijnuxe;

architecture zklqznypb of ijnuxe is
  signal clseetckla : integer;
  signal nlakz : boolean;
  signal ysnmrihgy : integer;
  signal shihymuus : integer;
begin
  zzkj : entity work.zwqndhzfm
    port map (hif => shihymuus, ofhm => ysnmrihgy, uaui => nlakz, etuvw => clseetckla);
  
  -- Single-driven assignments
  bh <= bh;
  i <= (others => 12);
  mzx <= mzx;
  
  -- Multi-driven assignments
  fwnzu <= fwnzu;
  fwnzu <= "ZX";
end zklqznypb;

entity xi is
  port (ti : out real; hkt : buffer real; eq : buffer time_vector(3 to 2));
end xi;

architecture xljf of xi is
  
begin
  -- Single-driven assignments
  eq <= eq;
end xljf;

library ieee;
use ieee.std_logic_1164.all;

entity ihzk is
  port (zg : buffer std_logic; va : out std_logic);
end ihzk;

architecture haupjig of ihzk is
  signal cedm : time_vector(3 to 2);
  signal iwnxc : real;
  signal ynj : real;
  signal ymqzsgakgu : time_vector(3 to 2);
  signal o : real;
  signal vcoxtbxndw : real;
begin
  kodiptcd : entity work.xi
    port map (ti => vcoxtbxndw, hkt => o, eq => ymqzsgakgu);
  ocui : entity work.xi
    port map (ti => ynj, hkt => iwnxc, eq => cedm);
  
  -- Multi-driven assignments
  va <= va;
  va <= zg;
end haupjig;



-- Seed after: 4267419786606678636,2230106469645304029
