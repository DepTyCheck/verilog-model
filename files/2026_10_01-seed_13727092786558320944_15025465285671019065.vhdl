-- Seed: 13727092786558320944,15025465285671019065

entity dcxobxzny is
  port (qfmhumdbo : out integer; jyxqq : out real; mozmmxzk : in time);
end dcxobxzny;

architecture yd of dcxobxzny is
  
begin
  -- Single-driven assignments
  jyxqq <= 2#0_1_0.0110#;
  qfmhumdbo <= 16#1B#;
end yd;

library ieee;
use ieee.std_logic_1164.all;

entity pncl is
  port (rdvb : inout real; xehsevj : out std_logic; isqct : linkage std_logic_vector(4 to 2));
end pncl;

architecture hjfrwjg of pncl is
  signal ki : time;
  signal nkazzeatdt : real;
  signal njioyytfkn : integer;
begin
  pku : entity work.dcxobxzny
    port map (qfmhumdbo => njioyytfkn, jyxqq => nkazzeatdt, mozmmxzk => ki);
  
  -- Multi-driven assignments
  xehsevj <= xehsevj;
  xehsevj <= 'Z';
  xehsevj <= xehsevj;
end hjfrwjg;

entity yzxzbame is
  port (wenfzfddm : out time);
end yzxzbame;

architecture nfuz of yzxzbame is
  signal htvyqqjoj : time;
  signal nbq : real;
  signal khgtfhun : integer;
begin
  fqfi : entity work.dcxobxzny
    port map (qfmhumdbo => khgtfhun, jyxqq => nbq, mozmmxzk => htvyqqjoj);
  
  -- Single-driven assignments
  wenfzfddm <= 0 hr;
  htvyqqjoj <= wenfzfddm;
end nfuz;



-- Seed after: 12264120883923213441,15025465285671019065
